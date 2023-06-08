module Components.UI.PreviewContainer where

import Prelude

import Affjax (get, printError, request, defaultRequest)
import Affjax.RequestHeader (RequestHeader(..))
import Affjax.ResponseFormat (json)
import Affjax.Web (driver)
import Data.Argonaut (Json, JsonDecodeError, decodeJson, toArray)
import Data.Array (foldl, (:))
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Class.Console (log)
import Halogen as H
import Halogen.Component (Component)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Types.Message (RootMessage(..))
import Types.Resource (Resource, UUID, uuidToString)
import Utils.ForeignFunctions (getJwtPair, goToPathName)


data PreviewContainerActions = Initialize | Play UUID
data PreviewContainerOutput = SendRoot RootMessage

previewContainer :: forall query input m. MonadAff m => Component query input PreviewContainerOutput m
previewContainer = 
  H.mkComponent {
    initialState,
    render,
    eval: H.mkEval H.defaultEval { handleAction = handleAction, initialize = Just Initialize}
  }
  where
  initialState _ = [] :: Array Resource

  render resourceList = HH.div [HP.id "previewContainer"] resourceListHtml
    where
      wrapResourceToDiv {resourceId, resourceTitle, resourceType, isSegmented} = res
        where
          onClickEvent = HE.onClick \_ -> Play resourceId
          res = HH.div [HP.classes [HH.ClassName "resPreview"], onClickEvent] <<< (\el -> el:[]) <<< HH.text <<< show $ resourceTitle
      resourceListHtml = map wrapResourceToDiv resourceList

  handleAction = case _ of
    Play rId -> do
      H.liftEffect <<< goToPathName $ "/app/player/" <> uuidToString rId
      H.raise (SendRoot $ Played rId)

    -- TODO: Refactoring
    Initialize -> do
      {access, refresh} <- H.liftEffect $ getJwtPair
      let reqHeaders = [RequestHeader "Authorization" ("Bearer " <> access)] :: Array RequestHeader
          newRequest = 
            (defaultRequest 
                {url = "/v1/resources", method = Left GET, responseFormat = json, headers = reqHeaders})
      response <- H.liftAff $ request driver newRequest
      -- response <- H.liftAff $ get driver json "/v1/resources"
      case response of
        Left err -> log $ printError err 
        Right resp -> do
          let resList = decodeJson resp.body :: Either JsonDecodeError Json
          case resList of
              Left err -> log $ show err
              Right rList -> do
                let maybeJsonArr = toArray rList
                case maybeJsonArr of
                    Nothing -> pure unit
                    Just jsonElems -> do
                        let eitherResourceArr = map decodeJson jsonElems :: Array (Either JsonDecodeError Resource)
                            extractResource acc (Left _) = acc
                            extractResource acc (Right el) = el : acc
                            resourceList = foldl extractResource [] eitherResourceArr
                            isResourceFragmented {isSegmented} = isSegmented 
                        H.modify_ \_ -> resourceList
