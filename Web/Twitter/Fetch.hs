{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Web.Twitter.Fetch (
  -- * Search
  -- , search

  -- * Direct Messages
  -- , directMessages
  -- , directMessagesSent
  -- , directMessagesShowId

  -- * Friends & Followers
  friendsIds,
  followersIds,
  -- , friendshipsExists
  -- , friendshipsIncoming
  -- , friendshipsOutgoing
  -- , friendshipsShow
  -- , friendshipsLookup
  -- , friendshipsNoRetweetIds

  -- * Users
  -- , usersLookup
  -- , usersProfileImageScreenName
  -- , usersSearch
  usersShow,
  -- , usersContributees
  -- , usersContributors

  -- * Suggested Users
  -- , usersSuggestions
  -- , usersSuggestionsSlug
  -- , usersSuggestionsSlugMembers

  -- * Favorites
  -- , favorites

  -- * Lists
  listsAll,
  -- , listsStatuses
  -- , listsMemberships
  -- , listsSubscribers
  -- , listsSubscribersShow
  -- , listsMembersShow
  listsMembers,
  -- , lists
  -- , listsShow
  -- , listsSubscriptions
  ) where

import qualified Data.ByteString.Char8 as S
import           Data.Conduit
import qualified Network.HTTP.Types    as HT

import           Web.Twitter.Api
import           Web.Twitter.Monad
import           Web.Twitter.Query
import           Web.Twitter.Types
import           Web.Twitter.Utils

mkQueryUser :: QueryUser -> HT.Query
mkQueryUser (QUserId uid) =  [("user_id", Just $ showBS uid)]
mkQueryUser (QScreenName sn) = [("screen_name", Just . S.pack $ sn)]

mkQueryList :: QueryList -> HT.Query
mkQueryList (QListId lid) =  [("list_id", Just $ showBS lid)]
mkQueryList (QListName listname) =
  [("slug", Just . S.pack $ lstName),
   ("owner_screen_name", Just . S.pack $ screenName)]
  where
    (screenName, ln) = span (/= '/') listname
    lstName = drop 1 ln

friendsIds :: MonadResourceBase m => QueryUser -> Source (TwitterT m) UserId
friendsIds   q = sourceCursor "GET" "friends/ids.json"   (mkQueryUser q) "ids"

followersIds :: MonadResourceBase m => QueryUser -> Source (TwitterT m) UserId
followersIds q = sourceCursor "GET" "followers/ids.json" (mkQueryUser q) "ids"

usersShow :: MonadResourceBase m => QueryUser -> TwitterT m User
usersShow q = apiJSON "GET" "users/show.json" (mkQueryUser q)

listsAll :: MonadResourceBase m => QueryUser -> Source (TwitterT m) List
listsAll q = sourceCursor "GET" "listps/all.json" (mkQueryUser q) ""

listsMembers :: MonadResourceBase m => QueryList -> Source (TwitterT m) User
listsMembers q = sourceCursor "GET" "lists/members.json" (mkQueryList q) "users"
