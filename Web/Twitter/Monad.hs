{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

module Web.Twitter.Monad (
  -- * The Twitter Monad
  Twitter, TwitterT, unTwitterT,
  runTwitterT,
  Config(..), def,

  -- * HTTP Access
  twitterHTTP,

  -- * Useful re-exports
  MonadResourceBase,
  ) where

import           Control.Applicative
import           Control.Monad.Base
import           Control.Monad.Reader
import           Control.Monad.Trans.Control
import           Control.Monad.Trans.Identity
import           Control.Monad.Trans.Resource
import           Data.Aeson
import           Data.Aeson.Types             (parseMaybe)
import           Data.Attoparsec
import qualified Data.ByteString.Char8        as S
import qualified Data.ByteString.Lazy.Char8   as L
import           Data.Conduit
import           Data.Default
import           Network.HTTP.Conduit
import           System.Directory
import           Web.Authenticate.OAuth

type Twitter    = TwitterT IO
type TwitterT m = TwitterT_ (ReaderT Env (ResourceT m))

newtype TwitterT_ m a
  = TwitterT_ { unTwitterT :: IdentityT m a }
  deriving
    ( Functor, Applicative, Monad      -- basic
    , MonadIO, MonadTrans, MonadBase b -- transformers
    , MonadThrow, MonadResource        -- resourcet
    )

instance MonadTransControl TwitterT_ where
  newtype StT TwitterT_ a =
    StTwitterT { unStTwitter :: a }
  liftWith f =
    TwitterT_ $ lift $ f $ liftM StTwitterT . runIdentityT . unTwitterT
  restoreT =
    TwitterT_ . lift . liftM unStTwitter

instance MonadBaseControl b m => MonadBaseControl b (TwitterT_ m) where
  newtype StM (TwitterT_ m) a = StMT { unStMT :: ComposeSt TwitterT_ m a }
  liftBaseWith = defaultLiftBaseWith StMT
  restoreM     = defaultRestoreM   unStMT

data Env
  = Env
    { envOAuth      :: OAuth
    , envCredential :: Credential
    , envProxy      :: Maybe Proxy
    , envManager    :: Manager
    }

data Config
  = Config
    { configOAuthConsumerKey    :: S.ByteString
    , configOAuthConsumerSecret :: S.ByteString
    , configCredentialFile      :: FilePath
    , configGetPIN              :: String -> IO String
    , configProxy               :: Maybe Proxy
    }

runTwitterT :: MonadResourceBase m => Config -> TwitterT m a -> m a
runTwitterT Config {..} m =
  withManager $ \mng -> do
    let tokens = createToken configOAuthConsumerKey configOAuthConsumerSecret

    cred <- liftIO $ do
      mbCred <- loadCredential configCredentialFile
      case mbCred of
        Nothing -> do
          cred <- authorize configProxy tokens configGetPIN mng
          saveCredential configCredentialFile cred
          return cred
        Just cred -> do
          return cred

    runReaderT (runIdentityT (unTwitterT m)) Env
      { envOAuth = tokens
      , envCredential = cred
      , envProxy = configProxy
      , envManager = mng
      }

createToken :: S.ByteString -> S.ByteString -> OAuth
createToken consumerKey consumerSecret = newOAuth
  { oauthServerName     = "twitter"
  , oauthRequestUri     = "http://twitter.com/oauth/request_token"
  , oauthAccessTokenUri = "http://twitter.com/oauth/access_token"
  , oauthAuthorizeUri   = "http://twitter.com/oauth/authorize"
  , oauthConsumerKey    = consumerKey
  , oauthConsumerSecret = consumerSecret
  }

loadCredential :: FilePath -> IO (Maybe Credential)
loadCredential file = do
  existp <- doesFileExist file
  if existp
    then do
      content <- S.readFile file
      return $ do
        j <- maybeResult . parse json $ content
        c <- parseMaybe parseJSON j
        return $ Credential c
    else return Nothing

saveCredential :: FilePath -> Credential -> IO ()
saveCredential file cred = L.writeFile file $ encode . unCredential $ cred

authorize :: Maybe Proxy -> OAuth -> (String -> IO String) -> Manager -> IO Credential
authorize pr oauth getPIN mng = runResourceT $ do
  cred <- getTemporaryCredentialProxy pr oauth mng
  let url = authorizeUrl oauth cred
  pin <- liftIO $ getPIN url
  getAccessTokenProxy pr oauth (insert "oauth_verifier" (S.pack pin) cred) mng

twitterHTTP :: (MonadResourceBase m)
               => Request (TwitterT m)
               -> TwitterT m (Response (ResumableSource (TwitterT m) S.ByteString))
twitterHTTP req0 = do
  Env {..} <- TwitterT_ ask
  req <- signOAuth envOAuth envCredential $ req0 { proxy = envProxy }
  http req envManager
