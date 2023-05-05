module Components.UI.Core where

import Prelude

import Components.Pages.Home (HomeComponentOutput(..), homeComponent)
import Components.Pages.Player (PlayerComponentQuery, PlayerState, playerComponent)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.HTML as HH
import Type.Proxy (Proxy(..))
import Types.Message (RootMessage(..))
import Types.Routes (Route(..))
import Types.State (RootState)

type Slots = ( 
  homeComponent :: forall query . H.Slot query HomeComponentOutput Int,
  playerComponent :: forall output. H.Slot PlayerComponentQuery output Int 
)

data RootComponentActions = HandleHome HomeComponentOutput

rootComponent :: forall query output m. MonadEffect m => MonadAff m => H.Component query RootState output m
rootComponent =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval H.defaultEval {handleAction = handleAction}
    }
  where
    initialState componentInput = componentInput

    render :: RootState -> H.ComponentHTML RootComponentActions Slots m
    render { appRoute } = case appRoute of
      Player rId -> resultHTML
        where
          inputState = { resourceId : rId } :: PlayerState
          resultHTML = HH.div_ [HH.slot_ (Proxy :: _ "playerComponent") 4 playerComponent inputState]
      _ -> resultHTML
        where
          resultHTML = HH.div_ [HH.slot (Proxy :: _ "homeComponent") 3 homeComponent unit HandleHome]

    handleAction = do
      case _ of
        HandleHome output -> case output of
          SendRoot rootMsg -> case rootMsg of
            Played rId -> do
              H.modify_ \state -> state {appRoute = Player rId}
