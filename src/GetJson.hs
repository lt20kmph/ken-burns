#!/usr/bin/env stack
-- stack script --resolver lts-12.21
 {-# LANGUAGE QuasiQuotes, ExtendedDefaultRules #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module GetJson
( mkPageResult
, mkPageDl
, mainJSON
, Page (..)
)where

-- import Text.InterpolatedString.Perl6 (q,qc)
import Text.Hamlet
import Text.Julius
import Text.Lucius
import Data.Typeable
import           Data.Aeson
import qualified Data.ByteString.Char8      as S8
import qualified Data.Yaml                  as Yaml
import           GHC.Generics
import           Network.HTTP.Simple
import           Control.Monad
import           Data.Aeson                 (encode, object, (.:?), (.=))
import           Data.Aeson.Text            (encodeToLazyText)
import           Data.ByteString.Base64     as B64
import qualified Data.ByteString.Lazy       as B
import qualified Data.ByteString.Lazy.Char8 as L8
import           Data.Text.Lazy             (Text)
import           Data.Text.Lazy.IO          as I
import           Network.HTTP.Client        as HC
import           Network.HTTP.Client.TLS
import           Network.HTTP.Types.Status  (statusCode)
import           System.Directory           (getDirectoryContents
                                            ,doesFileExist   
                                            ,createDirectoryIfMissing
                                            ,listDirectory)
import           System.FilePath            (takeExtension 
                                            ,splitExtension
                                            ,takeBaseName
                                            ,replaceExtension
                                            ,(</>)
                                            ,takeFileName)
import           System.IO                  as S
import           System.Process
import Data.Maybe
import Text.Printf
import           System.Random
import Data.Sort
import Foundation
import Data.Foldable
import           Graphics.GD
import Data.Functor.Adjunction (zipR)
import Data.Functor

ratio :: Float
ratio = 0.5

scalefactor :: Float
scalefactor = 0.5

rawImgWidth :: FilePath -> Int
rawImgWidth fp = 1500

rawImgHeight :: FilePath -> IO Int
rawImgHeight fp = do
  file <- loadJpegFile fp
  size <- imageSize file
  return $ snd size

rawImgRatio :: FilePath -> IO Float
rawImgRatio fp = do
  h <- rawImgHeight fp
  let w = fromIntegral $ rawImgWidth fp 
  return $ (fromIntegral h)/w

data Page url = Page
  { pageCss  :: [CssUrl url]
  , pageHtml :: HtmlUrl url 
  , pageJs   :: JavascriptUrl url
  } 

data Box = Box 
  { width  :: Float
  , height :: Float
  , xt     :: Float
  , yt     :: Float
  }
  deriving (Show)

data IdealBox = IdealBox
  { xiT :: Float
  , yiT :: Float
  }
  deriving (Show)

data AspectRatio = AspectRatio
    { aspectRatios :: [Float]
    }
    deriving (Show, Generic)

instance FromJSON AspectRatio
instance ToJSON AspectRatio

data ImageContext = ImageContext
    { cropHintsParams :: AspectRatio
    }
    deriving (Show, Generic)

instance FromJSON ImageContext
instance ToJSON ImageContext

data Feature = Feature
    { _type      :: String
    , maxResults :: Maybe Int
    }
    deriving (Show)

instance FromJSON Feature where
 parseJSON (Object v) =
    Feature <$> v .: "type"
            <*> v .:? "maxResults"
 parseJSON _ = mzero

instance ToJSON Feature where
 toJSON (Feature _type maxResults) =
    object [ "type"  .= _type
           , "maxResults"   .= maxResults
             ]

data Image = Image
    { content :: String
    }
    deriving (Show, Generic)

instance FromJSON GetJson.Image
instance ToJSON GetJson.Image

data Request = Request
    { image        :: GetJson.Image
    , features     :: [Feature]
    , imageContext :: Maybe ImageContext
    }
    deriving (Show, Generic)

instance FromJSON GetJson.Request
instance ToJSON GetJson.Request

data In = In
    { requests :: [GetJson.Request]
    }
    deriving (Show, Generic)

instance FromJSON In
instance ToJSON In

data Vertex = Vertex
    { x :: Maybe Float
    , y :: Maybe Float
    }
    deriving (Show, Generic)

instance FromJSON Vertex
instance ToJSON Vertex

data BoundingPoly = BoundingPoly
    { vertices           :: Maybe [Vertex]
    , normalizedVertices :: Maybe [Vertex]
    }
    deriving (Show, Generic)

instance FromJSON BoundingPoly
instance ToJSON BoundingPoly

data CropHint = CropHint
    { name               :: Maybe String
    , score              :: Maybe Float
    , boundingPoly       :: BoundingPoly
    , confidence         :: Maybe Float
    , importanceFraction :: Maybe Float
    }
    deriving (Show, Generic)

instance FromJSON CropHint
instance ToJSON CropHint

data CropHints = CropHints
    { cropHints :: [CropHint]
    }
    deriving (Show, Generic)

instance FromJSON CropHints
instance ToJSON CropHints

data Response = Response
    { localizedObjectAnnotations :: Maybe [CropHint]
    , cropHintsAnnotation        :: Maybe CropHints
    }
    deriving (Show, Generic)

instance FromJSON GetJson.Response
instance ToJSON GetJson.Response

data Out = Out
    { responses :: [GetJson.Response]
    }
    deriving (Show, Generic)

instance ToJSON Out
instance FromJSON Out

-- jsonFile :: S.FilePath
-- jsonFile = "request.json"

-- imgFile :: S.FilePath
-- imgFile = "1.jpg"

getJSON :: FilePath -> IO (Either String Out)
getJSON file = do
    f <- B.readFile file 
    return $ eitherDecode f

-- jsonTest = do
--     od <-getJSON
--     case od of
--         Left e  -> S.putStrLn ("Error!! " ++ show e)
--         Right y -> S.putStrLn (show y)

-- getIMAGE :: IO (S8.ByteString)
-- getIMAGE = do
--     file <- S8.readFile imgFile
--     return $ B64.encode file

-- imgTest :: IO ()
-- imgTest = do
--     img <- getIMAGE
--     S.putStrLn $ show img

-- listAll :: IO ()
-- listAll  = do
--     con <- getDirectoryContents "img/"
--     let conClean = filter (\fn -> takeExtension fn == ".JPG") con
--     mapM_ S.putStrLn conClean

headerAuth :: IO String
headerAuth = do
    key <- readProcess "gcloud" ["auth","application-default", "print-access-token"] ""  
    return $ "Bearer " ++ key 

headerType :: String
headerType = "Content-Type: application/json; charset=utf-8" 

reqURL :: HC.Request
reqURL = "POST https://vision.googleapis.com/v1/images:annotate" 

printHeader :: IO ()
printHeader = do
    h <- headerAuth
    S.putStrLn $ filter (/= '\n') h

mkRequest2 :: FilePath -> String -> FilePath -> IO () 
mkRequest2 uf sid infile = do
    let outfile = uf 
                ++ sid 
                ++ "/responses/" 
                ++ (takeBaseName infile) 
                ++ ".json"
    check <- doesFileExist outfile
    case check of
      False -> do 
        img <- S8.readFile infile
        let imgb64 = S8.unpack $ B64.encode img 
        let r1 = GetJson.Request (GetJson.Image imgb64) 
                              [Feature "OBJECT_LOCALIZATION" (Just 10)]
                              Nothing  
        let r2 = GetJson.Request (GetJson.Image imgb64) 
                             [Feature "CROP_HINTS" Nothing] 
                             (Just (ImageContext  (AspectRatio [1.6])))
        let preReq = In [r1,r2] 
        key <- headerAuth
        let h = S8.pack $ filter (/= '\n') key
        -- S8.putStrLn h
        let request = setRequestBodyJSON preReq 
                      $ setRequestHeader "Authorization" [h] 
                      $ reqURL
        response <- httpJSON request
        let out = getResponseBody response :: Out
        -- L8.putStrLn $ Data.Aeson.encode out
        I.writeFile outfile (encodeToLazyText out)
      True -> S.putStrLn "File allready exists! "

mainJSON :: FilePath -> String -> [(String,Int)] -> IO ()
mainJSON uf sid xs = do 
    -- con <- getDirectoryContents "img/"
    -- let con1 = filter (\fn -> takeExtension fn == ".JPG") con 
    -- let con2 = map (\fn ->  "img/" ++ fn) con1
    createDirectoryIfMissing True (uf </> sid </> "responses") 
    let files = map (\x -> uf </> sid </> "imgs" </> (fst x) ++ ".JPG") xs
    mapM_ (\x-> mkRequest2 uf sid x) files

sortFiles :: (a,Int) -> (a,Int) -> Ordering
sortFiles (a1, b1) (a2, b2)
  | b1 < b2  = LT
  | b1 > b2  = GT
  | b1 == b2 = EQ 

mkPage :: (String -> String -> [(String,Int)] -> JavascriptUrl (Route App))
       -> (String -> String -> Int -> HtmlUrl (Route App))
       -> (Int -> CssUrl (Route App))
       -> FilePath 
       -> String 
       -> [(String,Int)] 
       -> Int
       -> IO (Page (Route App)) 
mkPage mkj mkht precss uf sid xs dur = do 
  let duration = round (fromIntegral dur / (12 :: Float) + (4 :: Float)) 
      fadeInOutTime = 8 -- percent 
      ys = map (\(x,y) -> (uf </> sid </> "responses" </> x, y)) xs
      _ys = map (\(x,y) -> (uf </> sid </> "imgs" </> x ++ ".JPG",y)) xs
  let sorted = sortBy sortFiles ys
      css0 = precss duration
      delay = show $ duration * 500 - fadeInOutTime * 20
      script = mkj delay sid sorted 
      html = mkht sid (takeBaseName $ fst $ head sorted) (snd $ head sorted) 
      kb0 = map (\(x,i) -> (f x,i)) ys
              where f x = json2CoOrd sid uf $ x ++ ".json"
      kb1 = map (g _ys) kb0 
              where (g _ys) (m,i) = fmap (f i (hh _ys i)) m 
                    f i j   = (\x -> coOrd2css j 
                                               x 
                                               i 
                                               duration 
                                               fadeInOutTime)
                    hh ss i = fst $ head $ filter (\(x,y) -> y == i) ss 
  kb2 <- sequence kb1

  kb3 <- mconcat kb2

  return $ Page ([css0] ++ kb3) html script

mkPageResult :: FilePath -> String -> [(String,Int)] -> Int -> IO (Page (Route App)) 
mkPageResult = mkPage mkjs mkhtml preCss

mkPageDl :: FilePath -> String -> [(String,Int)] -> Int -> IO (Page (Route App)) 
mkPageDl = mkPage mkjsDl mkhtmlDl preCssDl

mkhtml :: String -> String -> Int -> HtmlUrl (Route App) 
mkhtml sid file index = [hamlet|
  <div .container.centered-container>
    <div #imageContainer>
      <img #fst .all-pics.pic#{show $ index - 1} src="/files/#{sid}/imgs/#{file}.JPG">
      <div #snd>
  |]

mkhtmlDl :: String -> String -> Int -> HtmlUrl (Route App) 
mkhtmlDl _ file index = [hamlet|
$doctype 5
<html lang="en">
  <head>
    <meta charset="utf-8">
    <title>
      Ken-Burns slideshow
    <link rel="stylesheet" href="slideshow.css">
    <script src="https://cdnjs.cloudflare.com/ajax/libs/jquery/3.5.1/jquery.min.js">
  <body>
    <div .container.centered-container>
      <div #imageContainer>
        <img #fst .all-pics.pic#{show $ index - 1} src="imgs/#{file}.JPG">
        <div #snd>
    <script src="slideshow.js">
  |]

mkjs :: String -> String -> [(String,Int)] -> JavascriptUrl (Route App)
mkjs delay sid ns = _mkjs delay files indices 
    where g = (\x -> "/files" </> sid </> "imgs" </> x ++ ".JPG")  
          files = map ( g . takeBaseName . fst) ns 
          indices = map ((\x -> x - 1) . snd) ns

mkjsDl :: String -> String -> [(String,Int)] -> JavascriptUrl (Route App)
mkjsDl delay sid ns = _mkjs delay files indices
    where g = (\x -> "imgs" </> x ++ ".JPG")  
          files = map ( g . takeBaseName . fst) ns 
          indices = map ((\x -> x - 1) . snd) ns

preCss :: Int -> CssUrl (Route App) 
preCss duration = [lucius|

#imageContainer {
  background-color: var(--color-0);
  width: 80vw;
  height: 40vw;
  margin: 0% 0% 0% 0%;
  overflow: hidden;
  border-radius: 13px;
  border: 6px var(--color-0) solid;
  position:relative
}

#imageContainer img {
  width: 100vw;
  height: auto;
  position:absolute;
}

.all-pics {
  opacity: 0;
  animation-duration: #{show duration}s;
  -webkit-animation-duration: #{show duration}s;
  animation-iteration-count: 1;
  -webkit-animation-iteration-count: 1;
}
|]

preCssDl :: Int -> CssUrl (Route App) 
preCssDl duration = [lucius|
:root {
  --color-0:#0D1B2A;
  --color-1:#1b263b;
  --color-2:#415A77;
  --color-3:#778DA9;
  --color-4:#E0E1DD;
}

body {
  background-color: var(--color-4);
  height: 100vh;
}

.centered-container {
  display:flex;
  justify-content: center;
  align-items: center;
  height:100%;
}

#imageContainer {
  background-color: var(--color-0);
  width: 80vw;
  height: 40vw;
  margin: 0% 0% 0% 0%;
  overflow: hidden;
  border-radius: 13px;
  border: 6px var(--color-0) solid;
  position:relative
}

#imageContainer img {
  width: 100vw;
  height: auto;
  position:absolute;
}

.all-pics {
  opacity: 0;
  animation-duration: #{show duration}s;
  -webkit-animation-duration: #{show duration}s;
  animation-iteration-count: 1;
  -webkit-animation-iteration-count: 1;
}
|]

_mkjs :: String -> [String] -> [Int] -> JavascriptUrl (Route App)
_mkjs delay files indices = [julius| 
var files = #{rawJS $ show files};
var indices = #{rawJS $ show indices};
var numfiles = files.length;
window.evens = 2;
window.odds = 1;
window.delay = #{rawJS delay};
$('body').on("animationstart","#fst", function(){
  console.log(window.odds);
  console.log(indices[window.odds]);
  window.setTimeout(function(){
      $("#snd").replaceWith(
        '<img id="snd" class="all-pics pic'.concat(
                                    indices[window.odds]
                                  ).concat(
                                    '" src="'
                                  ).concat(
                                    files[window.odds]
                                  ).concat('"/>')
      );
      window.odds = (window.odds + 2)%numfiles;
            }, window.delay 
  );
});
$('body').on("animationstart","#snd", function(){
  console.log(window.evens);
  console.log(indices[window.evens]);
  window.setTimeout(function(){
      $("#fst").replaceWith(
        '<img id="fst" class="all-pics pic'.concat(
                                    indices[window.evens]
                                  ).concat(
                                    '" src="'
                                  ).concat(
                                    files[window.evens]
                                  ).concat('"/>')
      );
      window.evens = (window.evens + 2)%numfiles;
            }, window.delay 
  );
});
|]

heightShift :: FilePath -> IO Int
heightShift fp = do
  h <- rawImgRatio fp 
  return $ round $ (h * (fromIntegral 100) - (fromIntegral 40))
                   /(fromIntegral 2)

coOrd2css :: FilePath -> (Float,Float) -> Int -> Int -> Int -> IO [CssUrl (Route App)]
coOrd2css fp (x,y) index duration fadeInOutTime = do
  hgtShift <- heightShift fp
  widthWiggle <- fmap fromIntegral $ randomRIO (-10,10)
  heightWiggle <- fmap fromIntegral $ randomRIO (-hgtShift,hgtShift)
  let ww = (fromIntegral (-10)) + widthWiggle/(fromIntegral 2)
      hw = (fromIntegral (-hgtShift)) + heightWiggle/(fromIntegral 2)
      css1 = [lucius|
      .pic#{show $ index - 1} {
        animation-name: kenburns#{show $ index - 1};
        -webkit-animation-name: kenburns#{show $ index - 1};
      }

      |]
  css2 <- zoomInOrOut [lucius|
      @keyframes kenburns#{show $ index - 1} {
        0% {
          transform: scale(0.9,0.9) translate(#{show  ww}vw, #{show hw}vw);
          -webkit-transform: scale(0.9,0.9) translate(#{show  ww}vw, #{show hw}vw);
          animation-timing-function: ease-in;
          -webkit-animation-timing-function: ease-in;
          opacity: 0;
        }

        #{show t1}% {
          animation-timing-function: linear;
          -webkit-animation-timing-function: linear;
          opacity: 1;
        }

        #{show t2}% {
          animation-timing-function: linear;
          -webkit-animation-timing-function: linear;
          opacity: 1;
        }

        #{show t3}% {
          transform: scale(#{show scale},#{show scale}) translate(#{show $ r x}vw, #{show $ r y}vw);
          -webkit-transform: scale(#{show scale},#{show scale}) translate(#{show $ r x}vw, #{show $ r y}vw);
          animation-timing-function: ease-out;
          -webkit-animation-timing-function: ease-out;
          opacity: 0;
        }
      }

      |]
                   [lucius|
      @keyframes kenburns#{show $ index - 1} {
        0% {
          transform: scale(#{show scale},#{show scale}) translate(#{show $ mr x}vw, #{show $ mr y}vw);
          -webkit-transform: scale(#{show scale},#{show scale}) translate(#{show $ mr x}vw, #{show $ mr y}vw);
          animation-timing-function: ease-in;
          -webkit-animation-timing-function: ease-in;
          opacity: 0;
        }
        #{show t1}% {
          animation-timing-function: linear;
          -webkit-animation-timing-function: linear;
          opacity: 1;
        }
        #{show t2}%{
          animation-timing-function: linear;
          -webkit-animation-timing-function: linear;
          opacity: 1;
        }
        #{show t3}% {
          transform: scale(0.9,0.9) translate(#{show ww}vw, #{show hw}vw);
          -webkit-transform: scale(0.9,0.9) translate(#{show ww}vw, #{show hw}vw);
          animation-timing-function: ease-out;
          -webkit-animation-timing-function: ease-out;
          opacity: 0;
        }
      }
  |]
  return [css1,css2]
  where t1 = fadeInOutTime 
        t2 = 50 + fadeInOutTime 
        t3 = 50 + 2 * fadeInOutTime 
        r = (\x -> (fromIntegral (round (x * o)))/o)
        o = fromIntegral 100
        m = (\x -> 1.1*x)
        mr = m . r
        scale = 1.6

zoomInOrOut :: a -> a -> IO a
zoomInOrOut s1 s2 = do
    r <- randomRIO (0,1)
    case r of
      0 -> return s1
      1 -> return s2

json2CoOrd ::  String -> FilePath -> FilePath -> IO (Float,Float)
json2CoOrd sid uf file = do
  json <- getJSON file
  let imgf = uf </> sid </> "imgs" </> (takeBaseName file) ++ ".JPG"
  case json of
    Right x -> do
      let objects = localizedObjectAnnotations $ head $ responses x
      let crops = cropHintsAnnotation $ (responses x)!!1
          hints = case (objects,crops) of
                   (Just xs,_)       -> head xs 
                   (Nothing,Just x)  -> head $ cropHints x 
                   (Nothing,Nothing) -> CropHint (Just "Error!") 
                                                 Nothing 
                                                 (BoundingPoly Nothing Nothing) 
                                                 Nothing
                                                 Nothing
          bp = boundingPoly hints
          nv = normalizedVertices bp
          ve = vertices bp
          w = fromIntegral $ rawImgWidth imgf 
      h <- fmap fromIntegral $ rawImgHeight imgf 
      let nve = fmap (map f) ve 
                  where f = (\(Vertex x y) -> (Vertex (Just $ (g x)/w)
                                                      (Just $ (g y)/h)))
                        g = (\x -> fromMaybe 0 x)
          box = case (nv,nve) of
                  (Just xs,Nothing) -> xs
                  (Nothing,Just xs) -> xs
                  (Nothing,Nothing) -> [Vertex Nothing Nothing] 
      s <- fitBox box imgf 
      r <- rawImgRatio imgf
      return $ ib2css s r 

    Left x -> return (0.5,0.5) 

idealBox :: Float -> Float -> IdealBox 
idealBox xt yt = IdealBox xt yt 

checkCenter :: FilePath -> (Float,Float)-> IO (Float,Float)
checkCenter file (x,y) = do
  h <- rawImgRatio file 
  let ccy (x,y)
        | y < 0.125     = (x,0.125)
        | y > h - 0.125 = (x,h - 0.125)
        | otherwise     = (x,y)
      ccx (x,y)
        | x < 0.25     = (0.25,y)
        | x > 0.75     = (0.75,y)
        | otherwise     = (x,y)
  return $ (ccx . ccy) (x,y)

fitBox :: [Vertex] -> FilePath -> IO IdealBox
fitBox xs file = do
  let v1 = xs!!0
      v2 = xs!!1
      v3 = xs!!2
      v4 = xs!!3
      box = case (v1,v2,v3,v4) of
              (Vertex v1x v1y,
               Vertex v2x v2y,
               Vertex v3x v3y,
               Vertex v4x v4y)  -> Just (Box ((f v2x) - (f v1x))
                                             ((f v3y) - (f v2y))
                                             (f v4x)
                                             (f v4y))
                                           where f = (\x -> fromMaybe 0 x)

      center = case box of 
                  Just b  -> ((width b)/2 + xt b,yt b - (height b)/2)
                  Nothing -> (0.5,0.5)
      
  centerSafe <- checkCenter file center
  -- let centerSafe = center

  let ib = idealBox ((fst centerSafe) - scalefactor/2) 
                    ((snd centerSafe) + ratio*scalefactor/2)
  return $ ib 

ib2css :: IdealBox -> Float -> (Float,Float)
ib2css b ratio = (10-x0/r,h/2 - y0/r - 20)
            where x0 = 100 * xiT b
                  y0 = h * yiT b
                  r  = 1.6
                  h  = ratio * 100 
