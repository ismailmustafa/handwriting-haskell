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

  let params = defaultImageParams {width = Just 1000, height = Just 1000}
  imageByteString <- renderImage creds params "Hello, World!"
  dir <- getCurrentDirectory
  let imageDir = dir </> "image.png"
  --let pdfDir = dir </> "image.pdf"
  writeFile imageDir imageByteString



--data ImageParams = ImageParams {
--    width               :: Maybe Integer
--  , height              :: Maybe Integer
--  , handwritingSize     :: Maybe Integer
--  , handwritingColor    :: Maybe Color
--  , lineSpacing         :: Maybe Integer
--  , lineSpacingVariance :: Maybe Double
--  , wordSpacingVariance :: Maybe Double
--  , randomSeed          :: Maybe Integer
--  } deriving (Show)
