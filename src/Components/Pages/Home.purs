module Components.Pages.Home where

import Prelude

import Components.UI.PreviewContainer (PreviewContainerOutput, previewContainer)
import Components.UI.PreviewContainer as PC
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.HTML as HH
import Type.Proxy (Proxy(..))
import Types.Message (RootMessage(..))

type Slots = ( 
  previewContainer :: forall query. H.Slot query PreviewContainerOutput Int 
)

data HomeComponentActions = HandlePreviewContainer PreviewContainerOutput | Init
data HomeComponentOutput = SendRoot RootMessage

homeComponent :: forall query input m. MonadEffect m => MonadAff m => H.Component query input HomeComponentOutput m
homeComponent =
  H.mkComponent
    { initialState: identity
    , render
    , eval: H.mkEval H.defaultEval {handleAction = handleAction, initialize = Just Init}
    }
  where

    render :: input -> H.ComponentHTML HomeComponentActions Slots m
    render _ = HH.div_ [
      HH.slot (Proxy :: _ "previewContainer") 0 previewContainer unit HandlePreviewContainer
    ]

    handleAction = do
      case _ of
        HandlePreviewContainer output -> case output of
          PC.SendRoot rootMessage -> case rootMessage of
            Played rId -> do
              H.raise (SendRoot $ Played rId)
        Init -> do
          pure unit
