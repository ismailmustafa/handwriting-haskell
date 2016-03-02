{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Network.Handwriting
    ( getHandwritings,
      getHandwriting,
      Credentials(..),
    ) where

import Network.Wreq
import Data.Aeson.Lens       (key, _Array, _String, _Double)
import Control.Lens          ((&), (?~), (^?), (^.), toListOf)
import Data.ByteString.Char8 (pack)
import Data.Map              (Map)
import Data.Text             (Text)
import GHC.Generics          (Generic)
import Data.Aeson            (FromJSON, Value)
import Data.Maybe            (fromMaybe)
import Data.Monoid           ((<>))

data Credentials = 
  Credentials { keyToken    :: String
              , secretToken :: String
              } deriving Show

data Handwriting = Handwriting { 
    hId                  :: Text
  , title                :: Text
  , dateCreated          :: Text
  , dateModified         :: Text
  , ratingNeatness       :: Double
  , ratingCursivity      :: Double
  , ratingEmbellishment  :: Double
  , ratingCharacterWidth :: Double
  } deriving (Show, Generic)

instance FromJSON Handwriting

getHandwritings :: Credentials -> IO [Handwriting]
getHandwritings c = do
  let opts     = defaults & auth ?~ basicAuth (pack $ keyToken c) (pack $ secretToken c)
  response     <- getWith opts "https://api.handwriting.io/handwritings"
  jsonResponse <- asValue response
  return $ jsonToHandwriting <$> toListOf (responseBody . _Array . traverse) jsonResponse

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

getHandwriting :: String -> Credentials -> IO Handwriting
getHandwriting hId c = do
  let opts     = defaults & auth ?~ basicAuth (pack $ keyToken c) (pack $ secretToken c)
  response     <- getWith opts $ "https://api.handwriting.io/handwritings/" <> hId
  jsonResponse <- asValue response
  return $ jsonToHandwriting $ jsonResponse ^. responseBody

type Color = (Double, Double, Double)

data ImageParams = ImageParams {
    width               :: Integer
  , height              :: Integer
  , handwritingSize     :: Integer
  , handwritingColor    :: Color
  , lineSpacing         :: Integer
  , lineSpacingVariance :: Double
  , wordSpacingVariance :: Double
  , randomSeed          :: Integer
  } deriving (Show)

