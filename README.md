# handwriting-haskell
API Client for the handwriting.io API.

handwriting.io provides an API that generates automatically generates handwritten text. 

To use this API, you'll need to get a KEY and SECRET from [here](https://handwriting.io).

# Endpoints
```
/handwritings
/handwritings/{id}
/render/pdf
/render/png
```

# Usage
### /handwritings

```hs
import Network.Handwriting
    
creds :: Credentials
creds = Credentials "key" "secret"

main :: IO ()
main = do
	handwritings <- getHandwritings creds
```

### /handwritings/{id}

```hs
import Network.Handwriting
    
creds :: Credentials
creds = Credentials "key" "secret"

main :: IO ()
main = do
	handwriting <- getHandwriting creds "31SF81NG00ES"
```

### /render/png

```hs
import Network.Handwriting
    
creds :: Credentials
creds = Credentials "key" "secret"

main :: IO ()
main = do
	imageByteString <- renderImage creds defaultImageParams "Hello World!"
```

### /render/pdf

```hs
import Network.Handwriting
    
creds :: Credentials
creds = Credentials "key" "secret"

main :: IO ()
main = do
	let params = defaultImageParams {format = PDF}
	imageByteString <- renderImage creds params "Hello World!"
```

### Optional Parameters

The optional image paramters data type is show below:
```hs
data ImageParams = ImageParams {
  format              :: Format       -- PNG | PDF
, width               :: Maybe Double -- Units are Pixels for PNG and Inches or Points for PDF
, height              :: Maybe Double 
, hId                 :: Maybe String -- ID of the handwriting style selected
, size                :: Maybe Double -- Handwriting size. Pixels for PNG, Inches or Points for PDF
, color               :: Maybe Color  -- RGB -> (0-255, 0-255, 0-255)
, lineSpacing         :: Maybe Double -- Spacing between lines of handwriting. Between 0 and 5
, lineSpacingVariance :: Maybe Double -- Randomizes line spacing, 0 - 1
, wordSpacingVariance :: Maybe Double -- Randomizes word spacing, 0 - 1
, randomSeed          :: RandomSeed   -- Randomize : (image for same text is DIFFERENT on every call) 
									  -- Repeatable : (image for same text is the SAME on every call)
, pdfUnits            :: PDFUnits     -- Inches | Points
} deriving (Show)
```

### Optional Parameter Examples

600px x 800px png with red text:
```hs
import Network.Handwriting
    
creds :: Credentials
creds = Credentials "key" "secret"

main :: IO ()
main = do
	let params = defaultImageParams { width  = Just 800
                                    , height = Just 600
                                    , color  = Just (255,0,0) }
	imageByteString <- renderImage creds params "Hello World!"
```

8.5 inches x 11 inches PDF with blue text:
```hs
import Network.Handwriting
    
creds :: Credentials
creds = Credentials "key" "secret"

main :: IO ()
main = do
	let params = defaultImageParams { width    = Just 8.5
                                    , height   = Just 11
                                    , color    = Just (0,0,255)
                                    , pdfUnits = Inches }
	imageByteString <- renderImage creds params "Hello World!"
```
