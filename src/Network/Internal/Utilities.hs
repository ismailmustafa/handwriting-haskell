{-# LANGUAGE OverloadedStrings #-}

-------------------------------------------------------------------------------
-- |
-- Module : Network.Internal.Utilities
-- Copyright : (C) 2016 Ismail Mustafa
-- License : BSD-style (see the file LICENSE)
-- Maintainer : Ismail Mustafa <ismailmustafa@rocketmail.com
-- Stability : provisional
-- Portability : OverloadedStrings
--
-- Helper functions that don't blong anywhere else.
--
-------------------------------------------------------------------------------

module Network.Internal.Utilities
    ( jsonToHandwriting,
      processImageParams
    ) where

import Control.Lens    ((^?))
import Data.Aeson      (Value)
import Data.Aeson.Lens (key, _String, _Double)
import Data.Maybe      (fromMaybe)
import Data.Monoid     ((<>))
import Network.Wreq
import Numeric         (showHex, showFFloat)

import Network.Internal.Model

{-| Convert a json object to a handwriting data type.
-}
jsonToHandwriting :: Value -> Handwriting
jsonToHandwriting json =
  Handwriting { handwritingId        = s $ json ^? (key "id" . _String)
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

{-| Takes in image parameters and the text to render and generates
    a properly formatted endpoint.
-}
processImageParams :: ImageParams -> String -> String
processImageParams ip s = mconcat [hFormat, handId, hSize, hColor, 
                                   hText, hWidth, hHeight,
                                   hLineSpace, hLineSpaceVar,
                                   hWordSpaceVar, hRandomSeed]
  where hFormat       = case format ip of {PNG -> "png";PDF -> "pdf"}
        hUnits        = case format ip of {PNG -> "px";PDF -> case pdfUnits ip of {Points -> "pt";Inches -> "in" }}
        handId        = fromMaybe "" $ ("?handwriting_id="<>) <$> hId ip 
        hSize         = fromMaybe "" $ (\x -> "&handwriting_size=" <> show x <> hUnits) <$> size ip
        hColor        = handleColor (format ip) $ color ip
        hText         = "&text=" <> s
        hWidth        = fromMaybe "" $ (\x -> "&width=" <> show x <> hUnits) <$> width ip
        hHeight       = fromMaybe "&height=auto" $ (\x -> "&height=" <> show x <> hUnits) <$> height ip
        hLineSpace    = fromMaybe "" $ (("&line_spacing="<>) . show) <$> lineSpacing ip 
        hLineSpaceVar = fromMaybe "" $ (("&line_spacing_variance="<>) . show) <$> lineSpacingVariance ip 
        hWordSpaceVar = fromMaybe "" $ (("&word_spacing_variance="<>) . show) <$> wordSpacingVariance ip 
        hRandomSeed   = "&random_seed=" <> case randomSeed ip of {Randomize -> "-1" ; Repeatable -> "1"}

{-| Convert RGB to either hex or png depending on output format.
-}
handleColor :: Format -> Maybe Color -> String
handleColor format color = case format of
                                  PNG -> fromMaybe "" $ toHex <$> color
                                  PDF -> fromMaybe "" $ toCMYK <$> color

{-| Convert RGB to Hex.
-}
toHex :: Color -> String
toHex (r,g,b) = "&handwriting_color=" <> showHex r "" <> showHex g "" 
                                      <> showHex b ""

{-| Limit double values to 3 significant figures.
-}
sigFigs :: Double -> String
sigFigs floatNum = showFFloat (Just 3) floatNum ""


{-| Convert RGB to CMYK.
-}
toCMYK :: Color -> String
toCMYK color = "&handwriting_color=" <> "(" <> sigFigs c <> "," <> sigFigs m 
                                     <> "," <> sigFigs y <> "," <> sigFigs k 
                                     <> ")"
  where (c0,m0,y0) = convertRGBtoCMY color
        k = minimum [c0,m0,y0]
        c = ( c0 - k ) / ( 1 - k )
        m = ( m0 - k ) / ( 1 - k )
        y = ( y0 - k ) / ( 1 - k )

{-| Convert RGB to CMY.
-}
convertRGBtoCMY :: Color -> (Double,Double,Double)
convertRGBtoCMY (r,g,b) = (c, m, y)
  where c = 1 - (fromIntegral r / 255)
        m = 1 - (fromIntegral g / 255)
        y = 1 - (fromIntegral b / 255)
