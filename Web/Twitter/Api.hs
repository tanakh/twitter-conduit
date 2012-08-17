{-# LANGUAGE RecordWildCards, OverloadedStrings, FlexibleContexts, ConstraintKinds #-}
module Web.Twitter.Api (
  sourceTwitter,
--  api,
--  apiGet,
--  apiCursor,
--  apiWithPages,
  ) where

import Web.Twitter.Monad
import Web.Twitter.Utils
import Web.Authenticate.OAuth

import Control.Applicative
import Control.Failure
import Control.Monad
import Control.Monad.Reader
import Control.Monad.Trans
import Control.Monad.Trans.Control
import Control.Monad.Trans.Resource
import Data.Aeson
import Data.Aeson.Types
import Data.ByteString (ByteString)
import Data.Conduit
import qualified Data.Conduit.List as C
import Data.Maybe
import Data.Monoid
import qualified Data.Text as T
import Network.HTTP.Conduit
import qualified Network.HTTP.Types as HT

endpoint :: String
endpoint = "https://api.twitter.com/1/"

sourceTwitter :: (MonadResource m, MonadReader Env m, MonadBaseControl IO m)
                 => HT.Method -> String -> HT.Query -> m (ResumableSource m ByteString)
sourceTwitter m url query = do
  Env {..} <- ask
  req0 <- signOAuth envOAuth envCredential $ fromJust $ parseUrl url
  req  <- return $ req0
          { method = m
          , queryString = HT.renderQuery False query
          , proxy = envProxy
          }
  resp <- http req envManager
  return $ responseBody resp

-- getJSON :: (FromJSON a, Monad m) => String -> HT.Query -> TwitterT m a
getJSON :: (FromJSON b, MonadResourceBase m)
           => String -> HT.Query -> TwitterT m b
getJSON url query = do
  rs <- sourceTwitter "GET" url query
  rs $$+- sinkJSON

{-
apiCursor :: (FromJSON a, Monad m)
             => String
             -> HT.Query
             -> T.Text
             -> Source (TwitterT m) a
apiCursor url query cursorKey = go (-1 :: Int) where
  go cursor = do
    let query' = ("cursor", Just $ showBS cursor) `insertQuery` query
    j <- sourceTwitter "GET" (endpoint ++ url) query' $$ sinkJSON
    case parseMaybe p j of
      Nothing ->
        return C.sourceNull
      Just (res, 0) ->
        return $ C.sourceList res
      Just (res, nextCursor) ->
        mappend (C.sourceList res) <$> go nextCursor

  p (Object v) = (,) <$> v .: cursorKey <*> v .: "next_cursor"
  p _ = mempty

apiWithPages :: (FromJSON a, Monad m)
                => String -> HT.Query -> Source (TwitterT m) a
apiWithPages url query = go 1 where
  go page = do
    let query' = ("page", Just $ showBS page) `insertQuery` query
    rs <- lift $ sourceTwitter "GET" (endpoint ++ url) query' $$ sinkJSON
    when (not $ null rs) $ do
      mapM_ rs yield
      go $ page + 1
-}
