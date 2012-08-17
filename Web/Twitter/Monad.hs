{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

module Web.Twitter.Monad (
  Twitter, TwitterT, unTwitterT,
  runTwitterT,
  Env(..),
  Config(..), def,

  -- useful re-exports
  MonadResourceBase,
  ) where

import           Control.Applicative
import           Control.Exception
import           Control.Monad.Base
import           Control.Monad.Reader
import           Control.Monad.Trans.Control
import           Control.Monad.Trans.Identity
import           Control.Monad.Trans.Resource
import           Data.Default
import           Network.HTTP.Conduit
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

deriving instance (Monad m, MonadReader Env m) => MonadReader Env (TwitterT_ m)

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
    {
    }

instance Default Config where
  def = Config {}

runTwitterT :: MonadResourceBase m => Config -> TwitterT m a -> m a
runTwitterT _conf m =
  withManager $ \mng -> do
    let tokens = assert False undefined :: OAuth
    runReaderT (runIdentityT (unTwitterT m)) Env
      { envOAuth = tokens
      , envCredential = Credential []
      , envProxy = Nothing
      , envManager = mng
      }
