module Main where

import Prelude

import Components.UI.Core (rootComponent)
import Effect (Effect)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Types.State (RootState)
import Utils.Core (parsePathName)
import Utils.ForeignFunctions (getPathName)

main :: Effect Unit
main = do
  pathName <- getPathName
  let appRoute = parsePathName pathName
      initAppState = { appRoute: appRoute} :: RootState
  HA.runHalogenAff do
    body <- HA.awaitBody
    runUI rootComponent initAppState body

