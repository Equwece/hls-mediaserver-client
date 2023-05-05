module Components.UI.Hls.HlsWrapper where

import Data.Unit (Unit)
import Effect (Effect)

foreign import initHls :: String -> Effect Unit
