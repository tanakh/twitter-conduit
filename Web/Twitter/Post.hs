{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ConstraintKinds, FlexibleContexts #-}

module Web.Twitter.Post
       ( statusesUpdate

         -- * Friendship
         -- , friendshipCreate
         -- , friendshipDestroy

         -- * Favorites
         -- , favoritesCreate
         -- , favoritesDestroy

         -- * Lists
         -- , listsCreate
         -- , listsDestroy
         -- , listsUpdate
         -- , listsMembersCreate
         -- , listsMembersDestroy
       )
       where

import Control.Monad.Trans.Resource
import Data.ByteString (ByteString)

import Web.Twitter.Monad
import Web.Twitter.Api

statusesUpdate :: MonadResourceBase m => ByteString -> TwitterT m ()
statusesUpdate tweet =
  apiJSON "POST" "statuses/update.json" [("status", Just tweet)]
