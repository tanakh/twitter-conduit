{-# LANGUAGE RecordWildCards, OverloadedStrings #-}
module Web.Twitter.Api (
  api,
  apiGet,
  apiCursor,
  apiWithPages,
  ) where

import Web.Twitter.Monad
import Web.Twitter.Utils
import Web.Authenticate.OAuth

import Control.Applicative
import Control.Monad.Trans
import Data.Aeson
import Data.Aeson.Types
import Data.ByteString (ByteString)
import Data.Conduit
import qualified Data.Conduit.List as C
import Data.Monoid
import qualified Data.Text as T
import Network.HTTP.Conduit
import qualified Network.HTTP.Types as HT

endpoint :: String
endpoint = "https://api.twitter.com/1/"

api :: ByteString -- ^ HTTP request method (GET or POST)
    -> String     -- ^ API Resource URL
    -> HT.Query   -- ^ Query
    -> Source (ResourceT TW) ByteString
api m url query = do
  (req, mgr) <- lift $ do
    p    <- getProxy
    req  <- parseUrl url
    req' <- signOAuthTW $ req { method = m, queryString = HT.renderQuery False query, proxy = p }
    mgr  <- getManager
    return (req', mgr)  
  lift $ responseBody <$> http (req :: Request TW) mgr

apiGet :: FromJSON a => String -> HT.Query -> TW a
apiGet url query =
  api "GET" url query $$ sinkFromJSON

apiCursor :: FromJSON a
             => String
             -> HT.Query
             -> T.Text
             -> Source TW a
apiCursor url query cursorKey = go (-1 :: Int) where
  go cursor = do
    let query' = ("cursor", Just $ showBS cursor) `insertQuery` query
    j <- api "GET" (endpoint ++ url) query' $$ sinkJSON
    case parseMaybe p j of
      Nothing ->
        return C.sourceNull
      Just (res, 0) ->
        return $ C.sourceList res
      Just (res, nextCursor) ->
        mappend (C.sourceList res) <$> go nextCursor

  p (Object v) = (,) <$> v .: cursorKey <*> v .: "next_cursor"
  p _ = mempty

apiWithPages :: (FromJSON a, Show a) => String -> HT.Query -> Source TW a
apiWithPages url query = sourceState (1 :: Int) pull $= C.concatMap id where
  pull page = do
    let query' = ("page", Just $ showBS page) `insertQuery` query
    rs <- api "GET" (endpoint ++ url) query' $$ sinkFromJSON
    case rs of
      [] -> return StateClosed
      _ -> return $ StateOpen (page + 1) rs
