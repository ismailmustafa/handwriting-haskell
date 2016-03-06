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
      renderImage,
      toCMYK
    ) where

import           Control.Lens          ((&), (?~), (^.), toListOf)
import           Control.Monad.IO.Class  (liftIO)
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
  liftIO $ print endpointString
  let endpoint = mconcat [baseUrl, "render/", processImageParams ip s]
  response <- getWith (opts c) endpoint
  return $ response ^. responseBody

processImageParams :: ImageParams -> String -> String
processImageParams ip s = mconcat [hFormat, handId, hSize, hColor, 
                                   hText, hWidth, hHeight,
                                   hLineSpace, hLineSpaceVar,
                                   hWordSpaceVar, hRandomSeed]
  where hFormat       = case format ip of {PNG -> "png";PDF -> "pdf"}
        hUnits        = case format ip of {PNG -> "px";PDF -> case pdfUnits ip of {Points -> "pt";Inches -> "in" }}
        handId        = fromMaybe "" $ ("?handwriting_id="<>) <$> hId ip 
        hSize         = fromMaybe "" $ (\x -> "&handwriting_size=" <> show x <> hUnits) <$> size ip
        hColor        = handleColor (format ip) $ color ip
        hText         = "&text=" <> s
        hWidth        = fromMaybe "" $ (\x -> "&width=" <> show x <> hUnits) <$> width ip
        hHeight       = fromMaybe "&height=auto" $ (\x -> "&height=" <> show x <> hUnits) <$> height ip
        hLineSpace    = fromMaybe "" $ (("&line_spacing="<>) . show) <$> lineSpacing ip 
        hLineSpaceVar = fromMaybe "" $ (("&line_spacing_variance="<>) . show) <$> lineSpacingVariance ip 
        hWordSpaceVar = fromMaybe "" $ (("&word_spacing_variance="<>) . show) <$> wordSpacingVariance ip 
        hRandomSeed   = "&random_seed=" <> case randomSeed ip of {Randomize -> "-1" ; Repeatable -> "1"}

handleColor :: Format -> Maybe Color -> String
handleColor format color = case format of
                                  PNG -> fromMaybe "" $ toHex <$> color
                                  PDF -> fromMaybe "" $ toCMYK <$> color

toHex :: Color -> String
toHex (r,g,b) = "&handwriting_color=" <> showHex r "" <> showHex g "" 
                                      <> showHex b ""

sigFigs :: Double -> String
sigFigs floatNum = showFFloat (Just 3) floatNum ""

convertRGBtoCMY :: Color -> (Double,Double,Double)
convertRGBtoCMY (r,g,b) = (c, m, y)
  where c = 1 - (fromIntegral r / 255)
        m = 1 - (fromIntegral g / 255)
        y = 1 - (fromIntegral b / 255)

toCMYK :: Color -> String
toCMYK color = "&handwriting_color=" <> "(" <> sigFigs c <> "," <> sigFigs m 
                                     <> "," <> sigFigs y <> "," <> sigFigs k 
                                     <> ")"
  where (c0,m0,y0) = convertRGBtoCMY color
        k = minimum [c0,m0,y0]
        c = ( c0 - k ) / ( 1 - k )
        m = ( m0 - k ) / ( 1 - k )
        y = ( y0 - k ) / ( 1 - k )

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

