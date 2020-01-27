
class Dispatch a where
  dispatch :: Text
    -> Maybe (Value -> [Object])

data
