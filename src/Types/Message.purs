module Types.Message where

import Types.Resource (UUID)

data RootMessage = Played UUID
