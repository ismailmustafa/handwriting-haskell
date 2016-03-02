{-# LANGUAGE OverloadedStrings #-}

module Network.Handwriting
    ( getHandwritings,
      getHandwriting,
      Credentials(..),
      Handwriting(..)
    ) where

import Network.Wreq
import Data.Aeson.Lens       (_Array)
import Control.Lens          ((&), (?~), (^.), toListOf)
import Data.ByteString.Char8 (pack)
import Data.Monoid           ((<>))

import Network.Internal.Model
import Network.Internal.Utilities

baseUrl :: String
baseUrl = "https://api.handwriting.io/handwritings/"

opts :: Credentials -> Options
opts c = defaults & auth ?~ basicAuth (pack $ keyToken c) (pack $ secretToken c)

getHandwritings :: Credentials -> IO [Handwriting]
getHandwritings c = do
  response     <- getWith (opts c) baseUrl 
  jsonResponse <- asValue response
  return $ jsonToHandwriting <$> toListOf (responseBody . _Array . traverse) jsonResponse

getHandwriting :: String -> Credentials -> IO Handwriting
getHandwriting hId c = do
  response     <- getWith (opts c) $ baseUrl <> hId
  jsonResponse <- asValue response
  return $ jsonToHandwriting $ jsonResponse ^. responseBody
