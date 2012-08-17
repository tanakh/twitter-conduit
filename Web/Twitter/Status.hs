{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Web.Twitter.Status (
  -- * Statuses
  statuses,

  -- * Timelines
  homeTimeline,
  mentions,
  publicTimeline,
  retweetedByMe,
  retweetedToMe,
  retweetsOfMe,
  userTimeline,
  retweetedToUser,
  retweetedByUser,
  ) where

import           Data.Aeson
import           Data.Conduit
import qualified Network.HTTP.Types as HT

import           Web.Twitter.Api
import           Web.Twitter.Monad
import           Web.Twitter.Types

statuses :: (FromJSON a, MonadResourceBase m)
            => String -> HT.Query -> Source (TwitterT m) a
statuses url query = sourcePages "GET" (endpoint ++ "statuses/" ++ url) query

homeTimeline :: MonadResourceBase m => HT.Query -> Source (TwitterT m) Status
homeTimeline = statuses "home_timeline.json"

mentions :: MonadResourceBase m => HT.Query -> Source (TwitterT m) Status
mentions = statuses "mentions.json"

publicTimeline :: MonadResourceBase m => HT.Query -> Source (TwitterT m) Status
publicTimeline = statuses "public_timeline.json"

retweetedByMe :: MonadResourceBase m => HT.Query -> Source (TwitterT m) Status
retweetedByMe = statuses "retweeted_by_me.json"

retweetedToMe :: MonadResourceBase m => HT.Query -> Source (TwitterT m) Status
retweetedToMe = statuses "retweeted_to_me.json"

retweetsOfMe :: MonadResourceBase m => HT.Query -> Source (TwitterT m) Status
retweetsOfMe = statuses "retweeted_of_me.json"

userTimeline :: MonadResourceBase m => HT.Query -> Source (TwitterT m) Status
userTimeline = statuses "user_timeline.json"

retweetedToUser :: MonadResourceBase m => HT.Query -> Source (TwitterT m) Status
retweetedToUser = statuses "retweeted_to_user.json"

retweetedByUser :: MonadResourceBase m => HT.Query -> Source (TwitterT m) Status
retweetedByUser = statuses "retweeted_by_user.json"

idRetweetedBy :: MonadResourceBase m
                 => StatusId -> HT.Query -> Source (TwitterT m) User
idRetweetedBy status_id = statuses (show status_id ++ "/retweeted_by.json")

idRetweetedByIds :: MonadResourceBase m
                    => StatusId -> HT.Query -> Source (TwitterT m) UserId
idRetweetedByIds status_id = statuses (show status_id ++ "/retweeted_by/ids.json")

retweetsId :: MonadResourceBase m
              => StatusId -> HT.Query -> TwitterT m [RetweetedStatus]
retweetsId status_id query = apiJSON "GET" uri query
  where uri = endpoint ++ "statuses/retweets/" ++ show status_id ++ ".json"

showId :: MonadResourceBase m
          => StatusId -> HT.Query -> TwitterT m Status
showId status_id query =
  apiJSON "GET" (endpoint ++ "statuses/show/" ++ show status_id ++ ".json") query
