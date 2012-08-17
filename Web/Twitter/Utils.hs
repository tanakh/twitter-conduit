{-# LANGUAGE DeriveDataTypeable #-}

module Web.Twitter.Utils (
  sinkJSON, conduitJSON,
  parseFromJSON,
  showBS,
  insertQuery,
  ) where

import Control.Applicative
import Control.Exception
import Data.Aeson hiding (Error)
import qualified Data.Aeson.Types as AT
import qualified Data.Attoparsec.Types as A
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B8
import Data.Conduit as C
import Data.Conduit.Attoparsec
import Data.Data
import qualified Network.HTTP.Types as HT
import qualified Data.Map as M

data TwitterError
  = TwitterError String
  deriving (Show, Data, Typeable)

instance Exception TwitterError

parseFromJSON :: FromJSON a => A.Parser ByteString a
parseFromJSON = do
  v <- json
  case fromJSON v of
    AT.Error _ -> empty
    AT.Success r -> return r

sinkJSON :: (FromJSON a, MonadResource m) => GLSink ByteString m a
sinkJSON = sinkParser parseFromJSON

conduitJSON :: (FromJSON a, MonadResource m) => GLInfConduit ByteString m a
conduitJSON = mapOutput snd $ conduitParser parseFromJSON

showBS :: Show a => a -> ByteString
showBS = B8.pack . show

insertQuery :: (ByteString, Maybe ByteString) -> HT.Query -> HT.Query
insertQuery (key, value) = mk
  where mk = M.toList . M.insert key value . M.fromList
