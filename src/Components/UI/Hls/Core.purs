module Components.UI.Hls.Core where

import Prelude

import Components.UI.Hls.HlsWrapper (initHls)
import Data.Maybe (Maybe(..))
import Effect.Class (class MonadEffect)
import Effect.Class.Console (log)
import Halogen as H
import Halogen.Component (Component)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Types.Resource (UUID, uuidToString)

data HlsAction = Restart

type HlsState = {resourceId :: Maybe UUID}

data HlsQuery a = Play UUID a

hlsComponent :: forall output m. MonadEffect m => Component HlsQuery HlsState output m
hlsComponent = 
  H.mkComponent {
    initialState,
    render,
    eval: H.mkEval H.defaultEval { 
                                 handleAction = handleAction, 
                                 initialize = Just Restart, 
                                 handleQuery = handleQuery
                                 }
  }
  where
    initialState inputState = inputState :: HlsState

    render _ =
      HH.div
        [ HP.id "videoContainer" ]
        [ HH.video [ HP.id "video", HP.controls true ] [] ]

    handleQuery :: forall a. HlsQuery a -> H.HalogenM HlsState HlsAction () output m (Maybe a)
    handleQuery = case _ of
      Play rId a -> do
        log $ "Command to play " <> show rId
        H.modify_ \state -> state {resourceId = Just rId}
        handleAction Restart
        pure (Just a)

    handleAction = case _ of
      Restart -> runResourceFromState
      where
        runResourceFromState = do
          { resourceId } <- H.get
          case resourceId of
            Just rId -> do
              let apiVideoUrl = "v1/resources/" <> uuidToString rId <> "/index/index" <> uuidToString rId <> ".m3u8" 
              H.liftEffect $ initHls apiVideoUrl
            Nothing -> pure unit
