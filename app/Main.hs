{-# LANGUAGE OverloadedStrings #-}
module Main where

import Network.Handwriting
import System.Environment   (getEnv)
import Data.ByteString.Lazy (writeFile, ByteString)
import Prelude hiding       (writeFile)
import System.Directory     (getCurrentDirectory)
import System.FilePath      ((</>))
import System.Random        (newStdGen, randomR)
import Data.Text            (unpack)

main :: IO ()
main = do
  -- Credentials
  creds        <- Credentials <$> getEnv "KEY" <*> getEnv "SECRET"

  -- Get all handwritings
  handwritings <- getHandwritings creds

  -- Generate random number in range of handwritings
  g            <- newStdGen
  let rand = randomR (0, length handwritings - 1) g

  -- Select random handwriting font
  let id   = unpack $ handwritingId $ handwritings !! fst rand
  handwriting <- getHandwriting creds id
  print handwriting

  -- Generate image
  let params = defaultImageParams { format              = PNG
                                  , hId                 = Just "31SB2CWG00DZ"
                                  , size                = Just 205
                                  , lineSpacing         = Just 2
                                  , lineSpacingVariance = Just 0.2
                                  , wordSpacingVariance = Just 0.4
                                  , width               = Just 800
                                  , color               = Just (0, 0, 0)
                                  , randomSeed          = Randomize
                                  }
  imageByteString <- renderImage creds params "Hello World!"

  -- Write image to current directory
  writeHandwritingFile imageByteString "image.png"

writeHandwritingFile :: ByteString -> String -> IO ()
writeHandwritingFile image name = do
  dir <- getCurrentDirectory
  let imageDir = dir </> name
  writeFile imageDir image

