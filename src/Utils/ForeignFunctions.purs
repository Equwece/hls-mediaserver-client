module Utils.ForeignFunctions where

import Data.Tuple (Tuple)
import Data.Unit (Unit)
import Effect (Effect)

type FormData = {usernameInput :: String, passwordInput :: String}
type JwtPair = { access :: String,
                 refresh :: String
                }

foreign import getPathName :: Effect String
foreign import getAuthFormData :: Effect FormData
foreign import goToPathName :: String -> Effect Unit
foreign import saveJwtPair :: JwtPair -> Effect Unit
