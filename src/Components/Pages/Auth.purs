module Components.Pages.Auth where

import Prelude

import Components.UI.AuthForm (AuthFormOutput, authForm)
import Components.UI.Hls.Core (HlsQuery)
import Components.UI.PreviewContainer (PreviewContainerOutput)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.HTML as HH
import Type.Proxy (Proxy(..))

type Slots = ( 
  authForm :: forall query . H.Slot query AuthFormOutput Int 
)

data AuthComponentActions = HandlePreviewContainer PreviewContainerOutput
-- data AuthComponentQuery a = Play UUID a
type AuthState = {}

authComponent :: forall query output m. MonadEffect m => MonadAff m => H.Component query AuthState output m
authComponent =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval H.defaultEval {handleAction = handleAction}
    }
  where
    initialState inputState = inputState
    
    render :: AuthState -> H.ComponentHTML AuthComponentActions Slots m
    render _ = HH.div_ [
      HH.slot_ (Proxy :: _ "authForm") 0 authForm []
    ]

    -- handleQuery :: forall state a. AuthComponentQuery a -> H.HalogenM state AuthComponentActions Slots output m (Maybe a)
    -- handleQuery = case _ of
    --   Play rId a -> do
    --     log $ "Auth, send to hls"
    --     H.tell (Proxy :: _ "hlsComponent") 0 (HC.Play rId)
    --     pure (Just a)

    handleAction = do
      case _ of
        _ -> pure unit
