module Components.UI.AuthForm where

import Prelude

import Affjax (get, post, printError)
import Affjax.RequestBody as RB
import Affjax.ResponseFormat (json)
import Affjax.Web (driver)
import Data.Argonaut (Json, JsonDecodeError, decodeJson, encodeJson, stringify, toArray)
import Data.Array (foldl, (:))
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Class.Console (log)
import Halogen as H
import Halogen.Component (Component)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Types.Message (RootMessage(..))
import Types.Resource (Resource, UUID, uuidToString)
import Utils.ForeignFunctions (JwtPair, getAuthFormData, goToPathName, saveJwtPair)


data AuthFormActions = SubmitAuthForm
data AuthFormOutput = SendRoot RootMessage
data FormEvents = InputUsername | InputPass

showJwtPair {access, refresh} = access <> " " <> refresh

authForm :: forall query input m. MonadAff m => Component query input AuthFormOutput m
authForm = 
  H.mkComponent {
    initialState,
    render,
    eval: H.mkEval H.defaultEval { handleAction = handleAction}
  }
  where
  initialState _ = [] :: Array Resource

  render _ = HH.div [HP.id "authForm"] [
    HH.input [ HP.id "usernameInput"
                ,HP.type_ HP.InputText
                ,HP.placeholder "Username"
                ],
    HH.input [ HP.id "passwordInput"
                ,HP.type_ HP.InputText
                ,HP.placeholder "Password"
                ],
    HH.p [HP.id "authFormButton", onSubmitEvent] [HH.text "Submit"]
  ]
    where
      onSubmitEvent = HE.onClick \_ -> SubmitAuthForm

  handleAction = case _ of
    SubmitAuthForm -> do
      {usernameInput, passwordInput} <- H.liftEffect getAuthFormData
      let authData = encodeJson $ {username: usernameInput, password: passwordInput}
      response <- 
        H.liftAff $ post driver json ("/v1/auth/create") (Just $ RB.json authData)
      case response of
        Left err -> log $ printError err 
        Right resp -> do
          let result = decodeJson resp.body :: Either JsonDecodeError JwtPair
          case result of
            Left err -> log $ show err
            Right rJson -> H.liftEffect $ saveJwtPair rJson
               
      H.modify_ identity
    -- Play rId -> do
    --   H.liftEffect <<< goToPathName $ "/app/player/" <> uuidToString rId
    --   H.raise (SendRoot $ Played rId)
