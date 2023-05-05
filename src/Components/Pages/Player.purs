module Components.Pages.Player where

import Prelude

import Components.UI.Hls.Core (HlsQuery, hlsComponent)
import Components.UI.Hls.Core as HC
import Components.UI.PreviewContainer (PreviewContainerOutput)
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Effect.Class.Console (log)
import Halogen as H
import Halogen.HTML as HH
import Type.Proxy (Proxy(..))
import Types.Resource (UUID)

type Slots = ( 
  hlsComponent :: forall output. H.Slot HlsQuery output Int 
)

data PlayerComponentActions = HandlePreviewContainer PreviewContainerOutput
data PlayerComponentQuery a = Play UUID a
type PlayerState = {resourceId :: UUID}

playerComponent :: forall output m. MonadEffect m => MonadAff m => H.Component PlayerComponentQuery PlayerState output m
playerComponent =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval H.defaultEval {handleAction = handleAction, handleQuery = handleQuery}
    }
  where
    initialState inputState = inputState
    
    render :: PlayerState -> H.ComponentHTML PlayerComponentActions Slots m
    render {resourceId} = HH.div_ [
      HH.slot_ (Proxy :: _ "hlsComponent") 0 hlsComponent {resourceId : Just resourceId}
    ]

    handleQuery :: forall state a. PlayerComponentQuery a -> H.HalogenM state PlayerComponentActions Slots output m (Maybe a)
    handleQuery = case _ of
      Play rId a -> do
        log $ "player, send to hls"
        H.tell (Proxy :: _ "hlsComponent") 0 (HC.Play rId)
        pure (Just a)

    handleAction = do
      case _ of
        _ -> pure unit
