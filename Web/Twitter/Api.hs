{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Web.Twitter.Api (
  -- * source of twitter APIs
  sourceTwitter,
  sourceJSON,
  sourceCursor,
  sourcePages,
  apiJSON,

  -- * endpoints
  endpoint,
  userstreamEndpoint,
  sitestreamEndpoint,

  -- * Re-exports
  StdMethod(..),
  ) where

import           Web.Twitter.Monad
import           Web.Twitter.Utils

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Trans
import           Data.Aeson
import           Data.Aeson.Types          (parseMaybe)
import           Data.ByteString           (ByteString)
import           Data.Conduit
import           Data.Maybe
import qualified Data.Text                 as T
import           Network.HTTP.Conduit
import qualified Network.HTTP.Types        as HT
import           Network.HTTP.Types.Method (StdMethod, renderStdMethod)

endpoint :: String
endpoint = "https://api.twitter.com/1/"

userstreamEndpoint :: String
userstreamEndpoint = "https://userstream.twitter.com/2/user.json"

sitestreamEndpoint :: String
sitestreamEndpoint = "https://sitestream.twitter.com/2b/site.json"

sourceTwitter :: MonadResourceBase m
                 => StdMethod
                 -> String
                 -> HT.Query
                 -> Source (TwitterT m) ByteString
sourceTwitter m url query = do
  (src, release) <- lift $ do
    let req = (fromJust (parseUrl url))
          { method = renderStdMethod m
          , queryString = HT.renderQuery False query
          }
    resp <- twitterHTTP req
    unwrapResumable $ responseBody resp
  addCleanup (const release) src

sourceJSON :: (FromJSON a, MonadResourceBase m)
              => StdMethod
              -> String
              -> HT.Query
              -> Source (TwitterT m) a
sourceJSON m url query =
  sourceTwitter m url query =$= conduitJSON

sourceCursor :: (FromJSON a, MonadResourceBase m)
                => StdMethod
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
        mapM_ yield res
        when (nextCursor > 0) $ go nextCursor

  p (Object v) = (,) <$> v .: cursorKey <*> v .: "next_cursor"
  p _ = mzero

sourcePages :: (FromJSON a, MonadResourceBase m)
               => StdMethod
               -> String
               -> HT.Query
               -> Source (TwitterT m) a
sourcePages m url query = go (1 :: Int) where
  go page = do
    let query' = ("page", Just $ showBS page) `insertQuery` query
    rs <- lift $ apiJSON m url query'
    when (not $ null rs) $ do
      mapM_ yield rs
      go $ page + 1

apiJSON :: (FromJSON b, MonadResourceBase m)
           => StdMethod -> String -> HT.Query -> TwitterT m b
apiJSON m url query =
  sourceTwitter m url query $$ sinkJSON
