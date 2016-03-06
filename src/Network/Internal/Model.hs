{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

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

data Credentials = 
  Credentials { keyToken    :: String
              , secretToken :: String
              } deriving Show

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

instance FromJSON Handwriting

type Color = (Word8, Word8, Word8)
data Format = PNG | PDF deriving (Show)
data RandomSeed = Randomize | Repeatable deriving (Show)
data PDFUnits = Points | Inches deriving (Show)

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

