{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

import Data.Conduit
import qualified Data.Conduit.List as CL
import qualified Data.Map as M
import Control.Monad.Trans
import Network
import System.Environment

import Web.Twitter

main :: IO ()
main = withSocketsDo $ do
  [QScreenName -> screenName] <- getArgs

  conf <- return Config
    { configOAuthConsumerKey = "K6fxaUunNsxMTNmFYhg"
    , configOAuthConsumerSecret = "nNFw21METyA0J5yuAYeeQxlB7ZXPgJFoFVoKI97Kk"
    , configCredentialFile = ".cred"
    , configGetPIN = \url -> do
      putStrLn $ "Access and get PIN: " ++ url
      getLine
    , configProxy = Nothing
    }

  runTwitterT conf $ do
    folids <- followersIds screenName $$ CL.consume
    friids <- friendsIds screenName $$ CL.consume

    let folmap = M.fromList $ map (flip (,) True) folids
        os = filter (\uid -> M.notMember uid folmap) friids
        bo = filter (\usr -> M.member usr folmap) friids

    liftIO $ do
      putStrLn "one sided:"
      print os

      putStrLn "both following:"
      print bo
