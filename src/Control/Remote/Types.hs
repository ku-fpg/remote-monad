
module Control.Remote.Types where

import Control.Monad.Catch
import Data.Typeable

data RemoteMonadException = RemoteEmptyException
   deriving (Show, Typeable)                             
                                                         
instance Exception RemoteMonadException                 











