module Types.Routes where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Types.Resource (UUID)

data Route = Home |
              Auth |
              Library |
              Player UUID
derive instance genericRoute :: Generic Route _

-- Necessary to eta-expand because the generic data type is recursive.
instance showRoute :: Show Route where
  show = genericShow
