{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Web.Twitter.Stream (
  -- * StreamingAPI
  userstream,
  statusesFilter,
  ) where

import           Data.Conduit
import qualified Network.HTTP.Types as HT

import           Web.Twitter.Api
import           Web.Twitter.Monad
import           Web.Twitter.Types

userstream :: MonadResourceBase m
              => Source (TwitterT m) StreamingAPI
userstream =
  sourceJSON GET userstreamEndpoint []

statusesFilter :: MonadResourceBase m
                  => HT.Query -> Source (TwitterT m) StreamingAPI
statusesFilter query =
  sourceJSON GET "https://stream.twitter.com/1/statuses/filter.json" query
