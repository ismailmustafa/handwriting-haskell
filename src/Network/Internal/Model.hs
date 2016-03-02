{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Network.Internal.Model
    ( Credentials(..),
      Handwriting(..),
      ImageParams(..),
      Color(..)
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
