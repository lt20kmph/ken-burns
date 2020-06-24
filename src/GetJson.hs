#!/usr/bin/env stack
-- stack script --resolver lts-12.21
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE QuasiQuotes          #-}

module GetJson
( mkPageResult
, mkPageDl
, json2info
, mainJSON
, Page (..)
, SettingsData (..)
, ImageSettings (..)
, PageSettings (..)
, ObjectSettings (..)
, ImageSettingsPage (..)
, ImageId
, SessionId
, Info (..)
)where

import           Control.Monad
import           Data.Aeson
import           Data.Aeson.Text         (encodeToLazyText)
import           Data.ByteString.Base64  as B64
import qualified Data.ByteString.Char8   as S8
import qualified Data.ByteString.Lazy    as B
import           Data.Hashable
import           Data.List               (groupBy)
import           Data.Map                (Map)
import qualified Data.Map                as Map
import           Data.Maybe
import           Data.Sort
import           Data.Text               (Text, pack, unpack)
import           Data.Text.Lazy.IO       as I
import           Foundation
import           GHC.Generics
import           Language.English.Plural
import           Network.HTTP.Client     as HC
import           Network.HTTP.Simple
import           System.Directory        (createDirectoryIfMissing,
                                          doesFileExist)
import           System.FilePath         (takeBaseName, (</>))
import           System.IO               as S
import           System.Process
import           System.Random
import           Text.Hamlet
import           Text.Julius
import           Text.Lucius
import           Text.Printf             (printf)

ratio :: Float
ratio = 0.5

scalefactor :: Float
scalefactor = 0.5

fadeInOutTime :: Int
fadeInOutTime = 4 --percent

data Page url = Page
    { pageCss  :: [CssUrl url]
    , pageHtml :: HtmlUrl url
    , pageJs   :: JavascriptUrl url
    }

type ImageId = String

type SessionId = String

class Info a where
  iid            :: a -> ImageId
  sid            :: a -> SessionId
  uf             :: a -> FilePath
  globalduration :: a -> Int
  index          :: a -> Int
  obj            :: a -> Maybe Text
  maxzoom        :: a -> Maybe Text
  minzoom        :: a -> Maybe Text
  rand           :: a -> Maybe Text

data SettingsData = SettingsData
    { sid1            :: SessionId
    , uf1             :: FilePath
    , globalduration1 :: Int
    , settings        :: Map ImageId ImageSettings
    }

instance Info SettingsData where
  sid globalsettings = sid1 globalsettings
  uf globalsettings = uf1 globalsettings
  globalduration globalsettings = globalduration1 globalsettings

data PageSettings = PageSettings
    { sid2            :: SessionId
    , uf2             :: FilePath
    , globalduration2 :: Int
    , pagesettings    :: Map ImageId ImageSettingsPage
    }

instance Info PageSettings where
  sid globalsettings = sid2 globalsettings
  uf globalsettings = uf2 globalsettings
  globalduration globalsettings = globalduration2 globalsettings

data ImageSettings = ImageSettings
    { _id       :: ImageId
    , _index    :: Int
    , _obj      :: Maybe Text
    , _maxzoom  :: Maybe Text
    , _minzoom  :: Maybe Text
    , _random   :: Maybe Text
    , duration  :: Int
    , rawwidth  :: Int
    , rawheight :: IO Int
    , rawratio  :: IO Float
    }

instance Info ImageSettings where
  iid imgsettings = _id imgsettings
  index imgsettings = _index imgsettings
  obj imgsettings = _obj imgsettings
  maxzoom imgsettings = _maxzoom imgsettings
  minzoom imgsettings = _minzoom imgsettings
  rand imgsettings = _random imgsettings

data ObjectSettings = ObjectSettings
    { nam       :: String
    , val       :: String
    , num       :: String
    , sco       :: String
    , isChecked :: Bool
    }

data ImageSettingsPage = ImageSettingsPage
    { pid      :: ImageId
    , pindex   :: Int
    , pobj     :: Maybe Text
    , pmaxzoom :: Maybe Text
    , pminzoom :: Maybe Text
    , prandom  :: Maybe Text
    , objs     :: [ObjectSettings]
    }

instance Info ImageSettingsPage where
  iid imgsettings = pid imgsettings
  index imgsettings = pindex imgsettings
  obj imgsettings = pobj imgsettings
  maxzoom imgsettings = pmaxzoom imgsettings
  minzoom imgsettings = pminzoom imgsettings
  rand imgsettings = prandom imgsettings

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
 toJSON (Feature _type mR) =
    object [ "type"  .= _type
           , "maxResults"   .= mR
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

-- don't change from x and y!!
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

-- headerType :: String
-- headerType = "Content-Type: application/json; charset=utf-8"

reqURL :: HC.Request
reqURL = "POST https://vision.googleapis.com/v1/images:annotate"

-- printHeader :: IO ()
-- printHeader = do
--     h <- headerAuth
--     S.putStrLn $ filter (/= '\n') h

mkRequest2 :: FilePath -> String -> FilePath -> IO ()
mkRequest2 upf s infile = do
    let outfile = upf
                ++ s
                ++ "/responses/"
                ++ (takeBaseName infile)
                ++ ".json"
    check <- doesFileExist outfile
    case check of
      False -> do
        img <- S8.readFile infile
        let imgb64 = S8.unpack $ B64.encode img
        let r1 = GetJson.Request (GetJson.Image imgb64)
                              [Feature "OBJECT_LOCALIZATION" (Just 15)]
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
mainJSON upf s xs = do
    -- con <- getDirectoryContents "img/"
    -- let con1 = filter (\fn -> takeExtension fn == ".JPG") con
    -- let con2 = map (\fn ->  "img/" ++ fn) con1
    createDirectoryIfMissing True (upf </> s </> "responses")
    let files = map (\x -> upf </> s </> "imgs" </> (fst x) ++ ".JPG") xs
    mapM_ (\x-> mkRequest2 upf s x) files

sortkeys :: SettingsData -> ImageId -> ImageId -> Ordering
sortkeys globalsettings k1 k2
  | index (m Map.! k1) < index (m Map.! k2)  = LT
  | index (m Map.! k1) > index (m Map.! k2)  = GT
  | index (m Map.! k1) == index (m Map.! k2)  = EQ
    where m = settings globalsettings

mkPage :: (SettingsData -> JavascriptUrl (Route App))
       -> (SettingsData -> ImageId -> HtmlUrl (Route App))
       -> (SettingsData -> CssUrl (Route App))
       -> SettingsData
       -> IO (Page (Route App))
mkPage mkj mkht precss globalsettings = do
  let css0 = precss globalsettings
      script = mkj globalsettings
      _k = Map.keys $ settings globalsettings
      k = sortBy (sortkeys globalsettings) _k
      iId = head k
      html = mkht globalsettings iId
      kb0 = map (json2CoOrd globalsettings) k
      kb1 = zipWith (\x key -> fmap (\p -> coOrd2css p key) x)
                    kb0
                    (map ((settings globalsettings) Map.!) k)
  kb2 <- sequence kb1
  kb3 <- mconcat kb2
  return $ Page ([css0] ++ kb3) html script

mkPageResult :: SettingsData -> IO (Page (Route App))
mkPageResult = mkPage mkjs mkhtml preCss

mkPageDl :: SettingsData -> IO (Page (Route App))
mkPageDl = mkPage mkjsDl mkhtmlDl preCssDl

mkhtml :: SettingsData -> ImageId -> HtmlUrl (Route App)
mkhtml globalsettings iId = [hamlet|
  <div .container.centered-container>
    <div #imageContainer>
      <img #fst .all-pics.pic#{show $ indx - 1} src="/files/#{s}/imgs/#{f}.JPG">
      <div #snd>
  |]
  where indx = index $ (settings globalsettings) Map.! iId
        f = iId
        s = sid globalsettings

mkhtmlDl :: SettingsData -> ImageId -> HtmlUrl (Route App)
mkhtmlDl globalsettings iId = [hamlet|
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
        <img #fst .all-pics.pic#{show $ indx - 1} src="imgs/#{f}.JPG">
        <div #snd>
    <script src="slideshow.js">
  |]
  where indx = index $ (settings globalsettings) Map.! iId
        f = iId

calcDelay :: Int -> String
calcDelay d = printf "%.2f" $ (fromIntegral 5)
                            * ((fromIntegral d)/(fromIntegral 10) + (fromIntegral 2))
                            * (fromIntegral (100 - fadeInOutTime))

calcDur :: Int -> String
calcDur d = printf "%.2f" $ (fromIntegral d)/(fromIntegral 10) + (fromIntegral 2)

mkjs :: SettingsData -> JavascriptUrl (Route App)
mkjs globalsettings = _mkjs delay files indices
    where delay = calcDelay $ globalduration globalsettings
          files = map g sortedkeys
          indices = map f sortedkeys
          g = (\x -> "/files" </> s </> "imgs" </> x ++ ".JPG")
          f = (\x -> x - 1) . index . ((settings globalsettings) Map.!)
          sortedkeys = sortBy (sortkeys globalsettings)
                        $ Map.keys
                        $ settings globalsettings
          s = sid globalsettings

mkjsDl :: SettingsData -> JavascriptUrl (Route App)
mkjsDl globalsettings = _mkjs delay files indices
    where delay = calcDelay $ globalduration globalsettings
          files = map g sortedkeys
          indices = map f sortedkeys
          g = (\x -> "imgs" </> x ++ ".JPG")
          f = (\x -> x - 1) . index . ((settings globalsettings) Map.!)
          sortedkeys = sortBy (sortkeys globalsettings)
                        $ Map.keys
                        $ settings globalsettings

preCss :: SettingsData -> CssUrl (Route App)
preCss globalsettings = [lucius|

#imageContainer {
  background-color: var(--color-0);
  width: 80vw;
  height: 45vw;
  padding:0;
  margin: 0% 0% 0% 0%;
  overflow: hidden;
  border-radius: 13px;
  border: 6px var(--color-0) solid;
  position:relative
}

#imageContainer img {
  width: 100vw;
  height: auto;
  transform-origin: top left;
  padding: 0;
  margin: 0;
  position:absolute;
}

.all-pics {
  opacity: 0;
  animation-duration: #{dur}s;
  -webkit-animation-duration: #{dur}s;
  animation-iteration-count: 1;
  -webkit-animation-iteration-count: 1;
}
|]
  where dur = calcDur $ globalduration globalsettings

preCssDl :: SettingsData -> CssUrl (Route App)
preCssDl globalsettings = [lucius|
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
  height: 45vw;
  margin: 0% 0% 0% 0%;
  padding:0;
  overflow: hidden;
  border-radius: 13px;
  border: 6px var(--color-0) solid;
  position:relative
}

#imageContainer img {
  width: 100vw;
  height: auto;
  transform-origin: top left;
  padding: 0;
  margin: 0;
  position:absolute;
}

.all-pics {
  opacity: 0;
  animation-duration: #{dur}s;
  -webkit-animation-duration: #{dur}s;
  animation-iteration-count: 1;
  -webkit-animation-iteration-count: 1;
}
|]
  where dur = calcDur $ globalduration globalsettings

_mkjs :: String -> [String] -> [Int] -> JavascriptUrl (Route App)
_mkjs delay files indices = [julius|
var files = #{rawJS $ show files};
var indices = #{rawJS $ show indices};
var numfiles = files.length;
window.odds = 1;
window.delay = #{rawJS delay};

if (numfiles == 2) {
  $(".pic0").css("animation-iteration-count","infinite");
};

window.evens = 2%numfiles;

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

extractMaxZooms :: Maybe Text -> Float
extractMaxZooms zm = (read $ unpack $ fromMaybe (pack "50") zm)
                  /(fromIntegral 300)
                  + (0.78 :: Float)

extractMinZooms :: Maybe Text -> Float
extractMinZooms zm = (read $ unpack $ fromMaybe (pack "50") zm)
                  /(fromIntegral 200)
                  + (1.35 :: Float)

extractRand :: Maybe Text -> Float
extractRand ra = (read $ unpack $ fromMaybe (pack "50") ra)
                  /(fromIntegral 1000)

heightShift :: ImageSettings -> IO Float
heightShift imgsettings = do
  h <- rawratio imgsettings
  let z = extractMaxZooms $ maxzoom imgsettings
  return $ (h * (fromIntegral (100 :: Int) * z) - (fromIntegral (45 :: Int)))
                   /(fromIntegral (2 :: Int))

widthShift :: ImageSettings -> IO Float
widthShift imgsettings = do
  let z = extractMaxZooms $ maxzoom imgsettings
  return $ ((fromIntegral (100 :: Int) * z) - (fromIntegral (80 :: Int)))
                   /(fromIntegral (2 :: Int))

jsonFp :: SettingsData -> ImageId -> FilePath
jsonFp globalsettings x  =  (uf globalsettings)
                         </> (sid globalsettings)
                         </> "responses"
                         </> x ++ ".json"

coOrd2css :: (Float,Float)
          -> ImageSettings
          -> IO [CssUrl (Route App)]
coOrd2css (x,y) imagesettings = do
  hgtShift <- heightShift imagesettings
  widShift <- widthShift imagesettings
  let rm  = extractRand $ rand imagesettings
  let r i = fmap fromIntegral $ (randomRIO (-i*10,i*10) :: IO Int)
  wWiggle  <- r $ round widShift
  hWiggle  <- r $ round hgtShift
  -- wWiggle  <- fmap fromIntegral $ (randomRIO (-widShift,widShift) :: IO Int)
  -- hWiggle  <- fmap fromIntegral $ (randomRIO (-hgtShift,hgtShift) :: IO Int)
  let ww = -widShift 
           + rm * wWiggle/(fromIntegral (2 :: Int)) :: Float
      hw = -hgtShift
           + rm * hWiggle/(fromIntegral (2 :: Int)) :: Float
  -- let ww = - widShift
  --     hw = - hgtShift
      indx = index imagesettings
      css1 = [lucius|
      .pic#{show $ indx - 1} {
        animation-name: kenburns#{show $ indx - 1};
        -webkit-animation-name: kenburns#{show $ indx - 1};
      }

      |]
  css2 <- zoomInOrOut [lucius|
      @keyframes kenburns#{show $ indx - 1} {
        0% {
          transform: scale(#{maxZm},#{maxZm}) translate(#{show  ww}vw, #{show hw}vw);
          -webkit-transform: scale(#{maxZm},#{maxZm}) translate(#{show  ww}vw, #{show hw}vw);
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
          transform: translate(#{show $ mr x}vw, #{show $ mr y}vw) scale(#{minZm},#{minZm});
          -webkit-transform: translate(#{show $ mr x}vw, #{show $ mr y}vw)scale (#{minZm},#{minZm});
          animation-timing-function: ease-out;
          -webkit-animation-timing-function: ease-out;
          opacity: 0;
        }
      }

      |]
                   [lucius|
      @keyframes kenburns#{show $ indx - 1} {
        0% {
          transform: translate(#{show $ mr x}vw, #{show $ mr y}vw) scale(#{minZm},#{minZm});
          -webkit-transform: translate(#{show $ mr x}vw, #{show $ mr y}vw) scale(#{minZm},#{minZm});
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
          transform: scale(#{maxZm},#{maxZm}) translate(#{show ww}vw, #{show hw}vw);
          -webkit-transform: scale(#{maxZm},#{maxZm}) translate(#{show ww}vw, #{show hw}vw);
          animation-timing-function: ease-out;
          -webkit-animation-timing-function: ease-out;
          opacity: 0;
        }
      }
  |]
  return [css1,css2]
  where t1 = 2 * fadeInOutTime
        t2 = 50 -- + fadeInOutTime
        t3 = 50 + 2 * fadeInOutTime
        r = (\v -> (fromIntegral (round (v * o) :: Int))/o)
        o = fromIntegral (100 :: Int)
        m = (\v -> 1.0*v)
        mr = m . r
        maxZm = show $ extractMaxZooms $ maxzoom imagesettings
        minZm = show $ extractMinZooms $ minzoom imagesettings

zoomInOrOut :: a -> a -> IO a
zoomInOrOut s1 s2 = do
    r <- randomRIO (0,1) :: IO Int
    return $ f r
      where f v | v == 0    = s1
                | v == 1    = s2
                | otherwise = s1

formatScore :: Float -> String
formatScore = printf "%.0f" . (*100)

formatName :: [(String,a)] -> String
formatName x = peopleFilter $ tryPlural (length x) $ fst $ head x
                where peopleFilter p | "Persons" == p = "People"
                                     | otherwise      = p

-- formatName :: [(String,a)] -> String
-- formatName x = fst $ head x

mean :: [Float] -> Float
mean xs = sum xs / (fromIntegral $ length xs)

-- json2info :: String -> FilePath -> FilePath -> IO [(String,String,String)]
json2info :: SettingsData -> IO PageSettings
json2info globalsettings = do
  let jsonFilePaths x = (uf globalsettings) </> (sid globalsettings)
                                            </> "responses"
                                            </> x ++ ".json"
  let m = settings globalsettings
  let ks = Map.keys m
  _j <- mapM getJSON $ map jsonFilePaths ks
  S.putStrLn $ concat $ map (show . obj . (m Map.!)) ks
  let f (j,k) = case j of
        Right x -> map (\a -> ObjectSettings (formatName a)
                                             (show $ hash $ fst $ head a)
                                             (show $ length a)
                                             (formatScore $ mean $ map snd a)
                                             (selectedObject m k a objects))
                                             uniqs
          where objects = localizedObjectAnnotations $ head $ responses x
                names = case objects of
                          Just xs -> map (\a -> (fromMaybe "Object" $ name a
                                              ,fromMaybe 0 $ score a)) xs
                          Nothing -> []
                uniqs = groupBy (\(a,_) (b,_) -> a == b)
                              $ sortBy
                              (\(a,_) (b,_) -> compare a b) names
  let g (k,v) = (k,ImageSettingsPage k
                                     (index $ m Map.! k)
                                     (obj $ m Map.! k)
                                     (maxzoom $ m Map.! k)
                                     (minzoom $ m Map.! k)
                                     (rand  $ m Map.! k)
                                     v)
  let _m = map g $ zip ks (map  f (zip _j ks))
  return $ PageSettings (sid globalsettings)
                        (uf globalsettings)
                        (globalduration globalsettings)
                        (Map.fromList _m)

selectedObject :: (Map ImageId ImageSettings)
               -> ImageId
               -> [(String,Float)]
               -> Maybe [CropHint]
               -> Bool
selectedObject m k a chs = (show $ hash $ fst $ head a) == t
  where t = case (obj $ m Map.! k) of
               Just v  -> unpack v
               Nothing -> show $ hash
                               $ fromMaybe "Object"
                               $ fromMaybe (Just "Object")
                               $ fmap (name . chooseObject) chs

orderCH :: [CropHint] -> CropHint -> CropHint -> Ordering
orderCH chs ch1 ch2
  | (av chs ch1) > (av chs ch2)  = LT
  | (av chs ch1) < (av chs ch2)  = GT
  | (av chs ch1) == (av chs ch2) = EQ
  where av chs ch = mean $ map ((fromMaybe 0) . score)
                         $ filterObjects (fmap (pack . show . hash)
                         $ name ch) chs

chooseObject :: [CropHint] -> CropHint
chooseObject chs = head $ sortBy (orderCH chs) chs

filterObjects :: Maybe Text -> [CropHint] -> [CropHint]
filterObjects ob = case ob of
                    Just _  -> filter (\ch -> (fmap (show . hash) $ name ch)
                                               == (fmap unpack ob))
                    Nothing -> id

json2CoOrd :: SettingsData -> ImageId -> IO (Float,Float)
json2CoOrd globalsettings iId = do
  let file = jsonFp globalsettings iId
  let imgsettings = (settings globalsettings) Map.! iId
  let ob = obj imgsettings
  _json <- getJSON file
  S.putStrLn $ show ob
  case _json of
    Right x -> do
      let objects = localizedObjectAnnotations $ head $ responses x
      -- S.putStrLn $ show objects
      let crops = cropHintsAnnotation $ (responses x)!!1
      let hints = case (objects,crops) of
                   (Just xs,_)       -> chooseObject $ filterObjects ob xs
                   (Nothing,Just a)  -> head $ cropHints a
                   (Nothing,Nothing) -> CropHint (Just "Error!")
                                                 Nothing
                                                 (BoundingPoly Nothing Nothing)
                                                 Nothing
                                                 Nothing
          bp = boundingPoly hints
          nv = normalizedVertices bp
          ve = vertices bp
          w = fromIntegral $ rawwidth imgsettings
      h <- fmap fromIntegral $ rawheight imgsettings
      let nve = fmap (map f) ve
                  where f = (\(Vertex a y) -> (Vertex (Just $ (g a)/w)
                                                      (Just $ (g y)/h)))
                        g = (\b -> fromMaybe 0 b)
          box = case (nv,nve) of
                  (Just xs,_)       -> xs
                  (Nothing,Just xs) -> xs
                  (Nothing,Nothing) -> [Vertex Nothing Nothing]
      s <- fitBox box imgsettings
      ib2css s imgsettings

    Left _ -> return (0.5,0.5)

idealBox :: Float -> Float -> IdealBox
idealBox x y = IdealBox x y

checkCenter :: ImageSettings -> (Float,Float) -> IO (Float,Float)
checkCenter imgsettings p = do
  r <- rawratio imgsettings
  let z = extractMinZooms $ minzoom imgsettings
  let h = (fromIntegral 100)*z*r
  let w = (fromIntegral 100)*z
  S.putStrLn $ show (w,h)
  let yb = (23 :: Float)/h
  let wb = (40.5 :: Float)/w
  let ccy (a,b)
        | b <= yb     = (a,yb)
        | b >= 1 - yb = (a,1 - yb)
        | otherwise  = (a,b)
      ccx (a,b)
        | a <= wb     = (wb,b)
        | a >= 1 - wb = (1 - wb,b)
        | otherwise  = (a,b)
  return $ (ccx . ccy) p

fitBox :: [Vertex] -> ImageSettings -> IO IdealBox
fitBox xs imgsettings = do
  let v1 = xs!!0
      v2 = xs!!1
      v3 = xs!!2
      v4 = xs!!3
      box = case (v1,v2,v3,v4) of
              (Vertex v1x _,
               Vertex v2x v2y,
               Vertex _   v3y,
               Vertex v4x v4y)  -> Just (Box ((f v2x) - (f v1x))
                                             ((f v3y) - (f v2y))
                                             (min (f v1x) (f v2x))
                                             (min (f v3y) (f v2y)))
                                           where f = (\x -> fromMaybe 0 x)

  let center = case box of
                  Just b  -> ((width b)/2 + xt b,yt b + (height b)/2)
                  Nothing -> (0.5,0.5)
  centerSafe <- checkCenter imgsettings center
  let ib = idealBox (fst centerSafe ) (snd centerSafe )
  return $ ib

ib2css :: IdealBox -> ImageSettings -> IO (Float,Float)
ib2css b imgsettings = do
  r <- rawratio imgsettings
  let z = extractMinZooms $ minzoom imgsettings
      h = (fromIntegral 100)*r
      w = (fromIntegral 100)
      yb = (22.5 :: Float)
      wb = (40.0 :: Float)
      x0 = w * (xiT b) * z
      y0 = h * (yiT b) * z
  S.putStrLn $ show (-x0 +wb, -y0 +yb)
  S.putStrLn $ show $ index imgsettings
  return (- x0 + wb, - y0 + yb)
