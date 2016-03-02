{-# LANGUAGE OverloadedStrings #-}

module Network.Internal.Utilities
    ( jsonToHandwriting
    ) where

import Network.Wreq
import Data.Aeson.Lens        (key, _String, _Double)
import Control.Lens           ((^?))
import Data.Aeson             (Value)
import Data.Maybe             (fromMaybe)

import Network.Internal.Model

jsonToHandwriting :: Value -> Handwriting
jsonToHandwriting json =
  Handwriting { hId                  = s $ json ^? (key "id" . _String)
              , title                = s $ json ^? (key "title" . _String)
              , dateCreated          = s $ json ^? (key "date_created" . _String)
              , dateModified         = s $ json ^? (key "date_modified" . _String)
              , ratingNeatness       = d $ json ^? (key "rating_neatness" . _Double)
              , ratingCursivity      = d $ json ^? (key "rating_cursivity" . _Double)
              , ratingEmbellishment  = d $ json ^? (key "rating_embellishment" . _Double)
              , ratingCharacterWidth = d $ json ^? (key "rating_character_width" . _Double)
              }
                where s = fromMaybe ""
                      d = fromMaybe 0
