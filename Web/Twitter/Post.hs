{-# LANGUAGE OverloadedStrings #-}

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

import Web.Twitter.Monad
import Web.Twitter.Api

import Data.ByteString (ByteString)

import qualified Data.Conduit as C

statusesUpdate :: ByteString -> C.Sink ByteString IO a -> C.Sink ByteString TW a
statusesUpdate tweet iter = undefined -- api "POST" (endpoint ++ "statuses/update.json") [("status", Just tweet)] iter