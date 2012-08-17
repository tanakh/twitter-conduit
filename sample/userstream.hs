import Control.Monad.Trans
import Data.Conduit
import qualified Data.Conduit.List as CL

import Web.Twitter
import Common

main :: IO ()
main = runTwitter def $ do
  runResourceT $ userstream $$ CL.mapM_ (lift . print)
