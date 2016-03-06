{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Network.Internal.Model
    ( Credentials(..),
      Handwriting(..),
      ImageParams(..),
      Color(..),
      Format(..),
      defaultImageParams
    ) where

import Data.Aeson  (FromJSON)
import Data.Monoid ((<>), mconcat)
import Data.Text
import GHC.Generics

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

type Color = (Double, Double, Double)
data Format = PNG | PDF deriving (Show)

data ImageParams = ImageParams {
    format              :: Format
  , width               :: Maybe Integer
  , height              :: Maybe Integer
  , handwritingId       :: Maybe String
  , handwritingSize     :: Maybe Integer
  , handwritingColor    :: Maybe Color
  , lineSpacing         :: Maybe Integer
  , lineSpacingVariance :: Maybe Double
  , wordSpacingVariance :: Maybe Double
  , randomSeed          :: Maybe Integer
  } deriving (Show)

defaultImageParams :: ImageParams
defaultImageParams = ImageParams {
    format              = PNG
  , width               = Nothing
  , height              = Nothing
  , handwritingId       = Just "2D5S46A80003"
  , handwritingSize     = Nothing
  , handwritingColor    = Nothing
  , lineSpacing         = Nothing
  , lineSpacingVariance = Nothing
  , wordSpacingVariance = Nothing
  , randomSeed          = Nothing
  }

