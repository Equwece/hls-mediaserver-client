module Types.State where

import Types.Routes (Route)

type RootState = {
  appRoute :: Route
}
