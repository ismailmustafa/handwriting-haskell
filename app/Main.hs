module Main where

import Network.Handwriting

import System.Environment   (getEnv)

import Data.ByteString.Lazy (writeFile)
import Prelude hiding       (writeFile) 
import System.Directory     (getCurrentDirectory)
import System.FilePath      ((</>))

--main :: IO (Either String Bool)
main :: IO ()
main = do
  key <- getEnv "KEY"
  secret <- getEnv "SECRET"
  
  let creds = Credentials key secret
  --handwritings <- getHandwritings creds
  --print $ length handwritings

  --let id = "81Y6NR5000CX"
  --handwriting <- getHandwriting creds id
  --print handwriting

  let params = defaultImageParams {format              = PNG, 
                                   hId                 = Just "31SF81NG00ES",
                                   size                = Just 30,
                                   color               = Just (242,38,19),
                                   lineSpacing         = Just 2,
                                   lineSpacingVariance = Just 0.2,
                                   wordSpacingVariance = Just 0.4,
                                   randomSeed          = Randomize}
  imageByteString <- renderImage creds params "You are my delicious monkey"
  dir <- getCurrentDirectory
  let imageDir = dir </> "image.png"
  writeFile imageDir imageByteString

--data ImageParams = ImageParams {
--    format              :: Format
--  , width               :: Maybe Double
--  , height              :: Maybe Double
--  , handwritingId       :: Maybe String
--  , handwritingSize     :: Maybe Double
--  , handwritingColor    :: Maybe Color
--  , lineSpacing         :: Maybe Double
--  , lineSpacingVariance :: Maybe Double
--  , wordSpacingVariance :: Maybe Double
--  , randomSeed          :: RandomSeed
--  , units               :: Units
--  } deriving (Show)

