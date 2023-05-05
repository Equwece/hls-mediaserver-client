module Utils.Core where

import Prelude

import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), split)
import Data.UUID as UD
import Types.Resource (UUID(..))
import Types.Routes (Route(..))

parsePathName :: String -> Route
parsePathName pathNameStr = resultRoute
  where
    pathList = split (Pattern "/") pathNameStr
    resultRoute = case pathList of
      ["", "app", "player", strResourceId] -> playerResult
        where
          innerUUID = UD.parseUUID strResourceId
          playerResult = case innerUUID of
            Just ud -> Player $ UUID ud
            Nothing -> Home
      _ -> Home
