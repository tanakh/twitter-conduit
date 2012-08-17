{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Web.Twitter.Api (
  sourceTwitter,
  sourceCursor,
  apiJSON,

--  apiWithPages,
  ) where

import           Web.Authenticate.OAuth
import           Web.Twitter.Monad
import           Web.Twitter.Utils

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Reader
import           Data.Aeson
import           Data.Aeson.Types
import           Data.ByteString        (ByteString)
import           Data.Conduit
import qualified Data.Conduit.List      as C
import           Data.Maybe
import           Data.Monoid
import qualified Data.Text              as T
import           Network.HTTP.Conduit
import qualified Network.HTTP.Types     as HT

endpoint :: String
endpoint = "https://api.twitter.com/1/"

sourceTwitter :: MonadResourceBase m
                 => HT.Method
                 -> String
                 -> HT.Query
                 -> TwitterT m (ResumableSource (TwitterT m) ByteString)
sourceTwitter m url query = do
  Env {..} <- ask
  let url' = fromJust $ parseUrl $ endpoint ++ url
  req0 <- signOAuth envOAuth envCredential url'
  req  <- return $ req0
          { method = m
          , queryString = HT.renderQuery False query
          , proxy = envProxy
          }
  resp <- http req envManager
  return $ responseBody resp

sourceCursor :: (FromJSON a, MonadResourceBase m)
             => HT.Method
             -> String
             -> HT.Query
             -> T.Text
             -> Source (TwitterT m) a
sourceCursor m url query cursorKey = go (-1 :: Int) where
  go cursor = do
    let query' = ("cursor", Just $ showBS cursor) `insertQuery` query
    j <- lift $ apiJSON m url query'
    case parseMaybe p j of
      Nothing ->
        return ()
      Just (res, nextCursor) -> do
        C.sourceList res
        when (nextCursor > 0) $ go nextCursor

  p (Object v) = (,) <$> v .: cursorKey <*> v .: "next_cursor"
  p _ = mempty

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

apiJSON :: (FromJSON b, MonadResourceBase m)
           => HT.Method -> String -> HT.Query -> TwitterT m b
apiJSON m url query = do
  rs <- sourceTwitter m url query
  rs $$+- sinkJSON
