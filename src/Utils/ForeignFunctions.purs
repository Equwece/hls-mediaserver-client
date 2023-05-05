module Utils.ForeignFunctions where

import Data.Unit (Unit)
import Effect (Effect)

foreign import getPathName :: Effect String
foreign import goToPathName :: String -> Effect Unit
