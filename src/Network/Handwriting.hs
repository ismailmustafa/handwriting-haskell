{-# LANGUAGE OverloadedStrings #-}

-------------------------------------------------------------------------------
-- |
-- Module : Network.Handwriting
-- Copyright : (C) 2016 Ismail Mustafa
-- License : BSD-style (see the file LICENSE)
-- Maintainer : Ismail Mustafa <ismailmustafa@rocketmail.com
-- Stability : provisional
-- Portability : OverloadedStrings
--
-- API Client for the handwriting.io API.
--
-------------------------------------------------------------------------------

module Network.Handwriting
    ( -- * Endpoints
      getHandwriting,
      getHandwritings,
      renderImage,
      -- * Types, DataTypes, and Utilites
      Color,
      Credentials(..),
      defaultImageParams,
      Format(..),
      Handwriting(..),
      ImageParams(..),
      PDFUnits(..),
      RandomSeed(..),
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

-- | Get a single handwriting by id.
--
-- > import Network.Handwriting
-- > creds :: Credentials
-- > creds = Credentials "key" "secret"
-- > 
-- > main :: IO ()
-- > main = do
-- >     handwritings <- getHandwritings creds "31SF81NG00ES"
--
getHandwriting :: Credentials -> String -> IO Handwriting
getHandwriting c hId = do
  response     <- getWith (opts c) $ baseUrl <> "handwritings/" <> hId
  jsonResponse <- asValue response
  return $ jsonToHandwriting $ jsonResponse ^. responseBody

-- | Get a list of all Handwritings.
--
-- > import Network.Handwriting
-- > creds :: Credentials
-- > creds = Credentials "key" "secret"
-- > 
-- > main :: IO ()
-- > main = do
-- >     handwritings <- getHandwritings creds
--
getHandwritings :: Credentials -> IO [Handwriting]
getHandwritings c = do
  response     <- getWith (opts c) $ baseUrl <> "handwritings"
  jsonResponse <- asValue response
  return $ jsonToHandwriting <$> toListOf (responseBody . _Array . traverse) jsonResponse

-- | Get a handwriting image as either a PDF or PNG.
--
-- > import Network.Handwriting
-- > creds :: Credentials
-- > creds = Credentials "key" "secret"
-- > 
-- > main :: IO ()
-- > main = do
-- >     let params = defaultImageParams {format = PDF}
-- >     imageByteString <- renderImage creds params "Hello World!"
--
renderImage :: Credentials -> ImageParams -> String -> IO BSL.ByteString
renderImage c ip s = do
  let endpointString = processImageParams ip s
  let endpoint = mconcat [baseUrl, "render/", processImageParams ip s]
  response <- getWith (opts c) endpoint
  return $ response ^. responseBody
