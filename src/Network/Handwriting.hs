{-# LANGUAGE OverloadedStrings #-}

module Network.Handwriting
    ( getHandwritings,
      getHandwriting,
      Credentials(..),
      Handwriting(..),
      ImageParams(..),
      Format(..),
      PDFUnits(..),
      RandomSeed(..),
      Color,
      defaultImageParams,
      renderImage
    ) where

import           Control.Lens          ((&), (?~), (^.), toListOf)
import           Data.Aeson.Lens       (_Array)
import           Data.ByteString.Char8 (pack)
import qualified Data.ByteString.Lazy.Char8 as BSL
import           Data.Maybe            (fromMaybe)
import           Data.Monoid           ((<>))
import           Data.List             (intercalate, minimum)
import           Data.List.Split       (splitOn)
import           Network.Wreq
import           Numeric               (showHex, showFFloat)

import Network.Internal.Model
import Network.Internal.Utilities

baseUrl :: String
baseUrl = "https://api.handwriting.io/"

opts :: Credentials -> Options
opts c = defaults & auth ?~ basicAuth (pack $ keyToken c) (pack $ secretToken c)

getHandwritings :: Credentials -> IO [Handwriting]
getHandwritings c = do
  response     <- getWith (opts c) $ baseUrl <> "handwritings"
  jsonResponse <- asValue response
  return $ jsonToHandwriting <$> toListOf (responseBody . _Array . traverse) jsonResponse

getHandwriting :: Credentials -> String -> IO Handwriting
getHandwriting c hId = do
  response     <- getWith (opts c) $ baseUrl <> "handwritings/" <> hId
  jsonResponse <- asValue response
  return $ jsonToHandwriting $ jsonResponse ^. responseBody

renderImage :: Credentials -> ImageParams -> String -> IO BSL.ByteString
renderImage c ip s = do
  let endpointString = processImageParams ip s
  let endpoint = mconcat [baseUrl, "render/", processImageParams ip s]
  response <- getWith (opts c) endpoint
  return $ response ^. responseBody
