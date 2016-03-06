{-# LANGUAGE OverloadedStrings #-}

module Network.Handwriting
    ( getHandwritings,
      getHandwriting,
      Credentials(..),
      Handwriting(..),
      ImageParams(..),
      Format(..),
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
import           Data.List             (intercalate)
import           Data.List.Split       (splitOn)
import           Network.Wreq
import           Control.Monad.IO.Class  (liftIO)

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
  liftIO $ print endpointString
  let endpoint = mconcat [baseUrl, "render/", processImageParams ip s]
  response <- getWith (opts c) endpoint
  return $ response ^. responseBody

processImageParams :: ImageParams -> String -> String
processImageParams ip s = mconcat [hFormat, hId, hWidth, hHeight, hText]
  where hFormat = case format ip of {PNG -> "png" ; PDF -> "pdf"}
        hId = fromMaybe "" $ ("?handwriting_id="++) <$> handwritingId ip 
        hWidth = fromMaybe "" $ (\x -> "&width=" ++ show x ++ "px") <$> width ip
        hHeight = fromMaybe "" $ (\x -> "&height=" ++ show x ++ "px") <$> height ip
        hText = "&text=" <> intercalate "+" (splitOn " " s)


--data ImageParams = ImageParams {
--    format              :: Format
--  , width               :: Maybe Integer
--  , height              :: Maybe Integer
--  , handwritingId       :: Maybe String
--  , handwritingSize     :: Maybe Integer
--  , handwritingColor    :: Maybe Color
--  , lineSpacing         :: Maybe Integer
--  , lineSpacingVariance :: Maybe Double
--  , wordSpacingVariance :: Maybe Double
--  , randomSeed          :: Maybe Integer
--  } deriving (Show)

--defaultImageParams :: ImageParams
--defaultImageParams = ImageParams {
--    format              = PNG
--  , width               = Nothing
--  , height              = Nothing
--  , handwritingId       = Just "2D5S46A80003"
--  , handwritingSize     = Nothing
--  , handwritingColor    = Nothing
--  , lineSpacing         = Nothing
--  , lineSpacingVariance = Nothing
--  , wordSpacingVariance = Nothing
--  , randomSeed          = Nothing
--  }

