{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.Trans
import Data.Conduit
import qualified Data.Conduit.List as CL
import Network
import System.IO

import Web.Twitter

conf :: Config
conf = Config
  { configOAuthConsumerKey = "K6fxaUunNsxMTNmFYhg"
  , configOAuthConsumerSecret = "nNFw21METyA0J5yuAYeeQxlB7ZXPgJFoFVoKI97Kk"
  , configCredentialFile = ".cred"
  , configGetPIN = \url -> do
    putStrLn $ "Access and get PIN: " ++ url
    getLine
  , configProxy = Nothing
  }

main :: IO ()
main = withSocketsDo $ runTwitterT conf $ do
  userstream $$ CL.mapM_ $ \tweet -> liftIO $ do
    print tweet
    hFlush stdout
