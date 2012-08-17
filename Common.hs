{-# LANGUAGE OverloadedStrings #-}

module Common where

import Web.Twitter

import Web.Authenticate.OAuth (OAuth(..), Credential(..))
import qualified Web.Authenticate.OAuth as OA
import qualified Network.URI as URI
import Network.HTTP.Conduit
import Data.Aeson hiding (Error)
import Data.Aeson.Types (parseMaybe)
import qualified Data.Map as M
import qualified Data.ByteString.Lazy.Char8 as LB
import qualified Data.ByteString.Char8 as B
import qualified Data.CaseInsensitive as CI
import Data.Attoparsec
import Control.Applicative
import Control.Monad.Trans
import Control.Monad.Trans.Resource
import System.IO
import System.FilePath
import System.Directory
import System.Environment

