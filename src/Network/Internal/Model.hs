{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

-------------------------------------------------------------------------------
-- |
-- Module : Network.Internal.Model
-- Copyright : (C) 2016 Ismail Mustafa
-- License : BSD-style (see the file LICENSE)
-- Maintainer : Ismail Mustafa <ismailmustafa@rocketmail.com
-- Stability : provisional
-- Portability : OverloadedStrings
--
-- Model definitions for the API wrapper.
--
-------------------------------------------------------------------------------

module Network.Internal.Model
    ( Credentials(..),
      Handwriting(..),
      ImageParams(..),
      Color(..),
      Format(..),
      PDFUnits(..),
      RandomSeed(..),
      defaultImageParams
    ) where

import Data.Aeson  (FromJSON)
import Data.Monoid ((<>), mconcat)
import Data.Text
import Data.Word   (Word8)
import GHC.Generics

{-| Credentials that take and key and secret token.
-}
data Credentials = 
  Credentials { keyToken    :: String
              , secretToken :: String
              } deriving Show

{-| Handwriting data type that contains all the information
    about a specific handwriting style.
-}
data Handwriting = Handwriting { 
    handwritingId        :: Text 
  , title                :: Text  
  , dateCreated          :: Text  
  , dateModified         :: Text  
  , ratingNeatness       :: Double
  , ratingCursivity      :: Double
  , ratingEmbellishment  :: Double
  , ratingCharacterWidth :: Double
  } deriving (Generic)

{-| Pretty print the handwriting data type.
-}
instance Show Handwriting where
  show (Handwriting a b c d e f g h) = "{"
    <> mconcat ["       Handwriting Id: ", show a, "\n"]  
    <> mconcat ["                Title: ", show b, "\n"]
    <> mconcat ["         Date Created: ", show c, "\n"]
    <> mconcat ["        Date Modified: ", show d, "\n"]
    <> mconcat ["      Rating Neatness: ", show e, "\n"]
    <> mconcat ["     Rating Cursivity: ", show f, "\n"]
    <> mconcat [" Rating Embellishment: ", show g, "\n"]
    <> mconcat ["Rating CharacterWidth: ", show h, "\n"]
    <> "}"

{-| Handwriting JSON instance.
-}
instance FromJSON Handwriting

{-| Color type representing (R,G,B).
-}
type Color = (Word8, Word8, Word8)

{-| Format determines Rendered image format in either 
    png or pdf.
-}
data Format = PNG | PDF deriving (Show)

{-| RandomSeed is used to specify is every rendered
    image called with the same parameters should render
    differently or the same every time.
-}
data RandomSeed = Randomize | Repeatable deriving (Show)

{-| PDFUnits is used to specify measurements when rendering
    a PDF.
-}
data PDFUnits = Points | Inches deriving (Show)

{-| Optional image parameters that dictate different
    properties of the rendered image.
-}
data ImageParams = ImageParams {
    format              :: Format
  , width               :: Maybe Double
  , height              :: Maybe Double
  , hId                 :: Maybe String
  , size                :: Maybe Double
  , color               :: Maybe Color
  , lineSpacing         :: Maybe Double
  , lineSpacingVariance :: Maybe Double
  , wordSpacingVariance :: Maybe Double
  , randomSeed          :: RandomSeed
  , pdfUnits            :: PDFUnits
  } deriving (Show)

{-| Default image parameters provided for convenience.
-}
defaultImageParams :: ImageParams
defaultImageParams = ImageParams {
    format              = PNG
  , width               = Nothing
  , height              = Nothing
  , hId                 = Just "2D5S46A80003"
  , size                = Nothing
  , color               = Nothing
  , lineSpacing         = Nothing
  , lineSpacingVariance = Nothing
  , wordSpacingVariance = Nothing
  , randomSeed          = Randomize
  , pdfUnits            = Inches
  }
