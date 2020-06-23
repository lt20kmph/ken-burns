{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE QuasiQuotes #-}

module Handler.Files where

import           Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8  as S8
import           Import
import           System.Directory
import           Yesod.Form.Bootstrap3  (BootstrapFormLayout (..)
                                        ,renderBootstrap3
                                        )
import           Data.Time.Clock
import           Data.Time.Clock.POSIX
-- import Data.Map  (Map)
import qualified Data.Map as Map
import           Graphics.GD
import           System.Random
import           Test.RandomStrings
import           GetJson
import           Text.Read (read)
import           System.FilePath (takeBaseName)
import           Data.List (nub,(!!))
import           Text.Blaze.Html.Renderer.Text
import qualified Data.Text.Lazy as L
import qualified Data.ByteString.Lazy as B
import           Codec.Archive.Zip

data UploadForm = UploadForm
    { fileInfo' :: FileInfo
    }

data FileForm = FileForm
    { fileInfo        :: FileInfo
    , fileDescription :: Text
    }

type QueryString = String

width :: Int
width = 1500

thumbHeight :: Int
thumbHeight = 168

getLoadingR :: Handler Html
getLoadingR = do
  sid <- lookupSession "sessionID"
  case sid of
    Just x  -> do
      fl <- getFileList x
      g fl (unpack x)
        where g fl u 
                  | (length fl) > 1 = getLoadingSuccess
                  | otherwise       = getLoadingFail u

getLoadingFail :: String -> Handler Html
getLoadingFail sid = do
  (formWidget_, enctype) <- generateFormPost buildForm
  setMessage "Please upload 2 or more pictures!"
  uf <- uploadFolder
  fl <- getFileList exampleSid
  let thumbsDir = uf </> (unpack sid) </> "thumbs"
  let navbar = uploadNav
  settings <- mkSettingsDefault uf (unpack exampleSid) fl
  let slideshow = getResultById settings
  -- let upldhead = uploadHeader 0 
  defaultLayout $ do
    let _fileList True dir = listDirectory dir  
        _fileList False _ = return [] :: IO [FilePath]
    b <- liftIO $ doesDirectoryExist thumbsDir
    fileList <- liftIO $ _fileList b thumbsDir
    let fileDir = (unpack sid) </> "thumbs"
    let th = thumbHeight :: Int
    setTitle "Ken-Burns slideshow"
    $(widgetFile "fileupload")


getLoadingSuccess :: Handler Html
getLoadingSuccess = do
  defaultLayout $ do
    setTitle "loading.. "
    let header = [hamlet|
      <h1>Generating your slideshow...
        |]
    $(widgetFile "loading")

getResultR :: Handler Html
getResultR = do
  sid <- lookupSession "sessionID"
  let _sid = fromMaybe (pack "ERROR") sid
  case sid of
    Just x  -> do
      let navbar = resultsNav x 50 
      fl <- getFileList x
      uf <- uploadFolder
      liftIO $ mainJSON uf (unpack x) fl
      -- page <- liftIO $ mkPageResult uf (unpack x) fl 50 
      -- let slideshow = pageHtml page 
      settings <- mkSettingsDefault uf (unpack x) fl
      let query = buildQuery settings 
      let slideshow = getResultById settings 
      defaultLayout $ do
        setTitle "Ken-Burns slideshow"
        -- fold (map toWidget $ pageCss page) 
        -- toWidgetBody $ pageJs page
        $(widgetFile "results")
    Nothing -> do 
      -- let fl = [] :: [(String,Int)]
      let slideshow = [hamlet|<h1>
        Unfortunatly something seems to have gone wrong
        |]
      let navbar = resultsNav "NA" 50
      settings <- mkSettingsDefault "NA" "NA" []
      let query = buildQuery settings 
      defaultLayout $ do
        setTitle "Oops!"
        $(widgetFile "results")

postResultR :: Handler Html
postResultR = getResultR 

getResultById :: SettingsData -> Widget 
getResultById settings = do
  page <- liftIO $ mkPageResult settings 
  toWidget $ pageHtml page 
  fold (map toWidget $ pageCss page) 
  toWidgetBody $ pageJs page

getResultByIdR :: Text -> Handler Html
getResultByIdR sid = do
  setSession "sessionID" sid 
  fl <- getFileList sid
  uf <- uploadFolder
  settings <- mkSettingsDefault uf (unpack sid) fl
  defaultLayout $ do
    setTitle "Ken-Burns slideshow"
    let navbar = resultsNav sid 50 
    let slideshow = getResultById settings 
    let _sid = sid
    let query = buildQuery settings
    $(widgetFile "results")

postResultByIdR :: Text -> Handler Html
postResultByIdR sid = do
  setSession "sessionID" sid 
  fl        <- getFileList sid
  -- zooms     <- mapM (g "Zooms") fl
  -- objects   <- mapM (g "Object") fl
  -- let f (x,y) o z = (x, ImageSettings y o z 50)
  uf <- uploadFolder
  settings <- mkSettings uf 
                            (unpack sid)
                            fl 
                            50
                            (Just (lookupPostParam . (++ "Object")))
                            (Just (lookupPostParam . (++ "maxZoom")))
                            (Just (lookupPostParam . (++ "minZoom")))
                            (Just (lookupPostParam . (++ "Random")))
                            Nothing
  defaultLayout $ do
    setTitle "Ken-Burns slideshow"
    let navbar = resultsNav sid 50 
    let slideshow = getResultById settings
    let _sid = sid
    let query = buildQuery settings
    $(widgetFile "results")

getFileList :: Text -> Handler [(String,Int)]
getFileList sessId = runDB $ do
  sessImgs <- selectList [ ImageDataSessionID ==. sessId ] []
  return $ map (f . entityVal) sessImgs
    where f = (\x -> (unpack $ g x,i x))
          g = (\x -> imageDataImgID x)
          i = (\x -> imageDataIndex x)

exampleSid :: Text
exampleSid = "20058826"
-- exampleSid = "00000000"

getUploadFileR :: Handler Html
getUploadFileR = do
  (formWidget_, enctype) <- generateFormPost buildForm
  iD <- liftIO $ mkSessId
  setSession "sessionID" (pack iD)
  uf <- uploadFolder
  -- fp <- liftIO $ listDirectory $ uf </> "20058826" </> "imgs"
  -- addTesting2DB fp
  fl <- getFileList exampleSid
  -- sfl <- liftIO $ sampleRandomFiles 20 fl
  settings <- mkSettingsDefault uf (unpack exampleSid) fl
  let slideshow = getResultById settings
  let navbar = uploadNav
  -- let upldhead = uploadHeader 0 
  defaultLayout $ do
      let fileList = [] :: [FilePath]
          fileDir = "" :: FilePath
      let th = thumbHeight :: Int
      setTitle "Ken-Burns slideshow"
      $(widgetFile "fileupload")

uploadNav :: HtmlUrl (Route App) 
uploadNav = [hamlet|
    <div .nav-container>
      <button .nav-btn.my-tooltip 
        onclick="window.location.href='@{UploadFileR}#upload-head';">
        <i .fa.fa-home>
        <span .tooltiptext>
          Home
      <button .nav-btn.my-tooltip 
        onclick="window.location.href='@{UploadFileR}#form';">
        <i .fa.fa-upload>
        <span .tooltiptext>
          Upload files
      <button .nav-btn.my-tooltip
        onclick="window.location.href='@{UploadFileR}#info';">
        <i .fa.fa-info>
        <span .tooltiptext>
          Information
      <button .nav-btn.my-tooltip 
        onclick="window.location.href='@{UploadFileR}#examples';">
        <i .fa.fa-photo>
        <span .tooltiptext>
          Gallery
  |]

uploadHeader :: Int -> HtmlUrl (Route App)
uploadHeader n 
  | n == 0    = [hamlet|Upload your pictures to get started|]
  | otherwise = [hamlet|Upload more pictures|]

--   (formWidget_, enctype) <- generateFormPost buildForm
--   fl <- getFileList exampleSid
--   uf <- uploadFolder
--   let uplpwidget = $(widgetFile "uploadprogress")
--   let slideshow = getResultById exampleSid fl uf 50
--   let navbar = uploadNav
--   defaultLayout $ do
--       let fileList = [] :: [FilePath]
--           fileDir = "" :: FilePath
--       let th = thumbHeight :: Int
--       setTitle "Ken-Burns slideshow"
--       $(widgetFile "fileupload")


postUploadFileR :: Handler Html
postUploadFileR = do
  ((result, formWidget_), enctype) <- runFormPost buildForm
  sid <- lookupSession "sessionID"
  fl <- getFileList exampleSid
  uf <- uploadFolder
  -- sfl <- liftIO $ sampleRandomFiles 12 fl
  settings <- mkSettingsDefault uf (unpack exampleSid) fl
  let slideshow = getResultById settings
  let navbar = uploadNav
  case sid of

    Just x -> do 
      let imgsDir = uf </> (unpack x) </> "imgs"
      let thumbsDir = uf </> (unpack x) </> "thumbs"
      liftIO $ createDirectoryIfMissing True imgsDir 
      liftIO $ createDirectoryIfMissing True thumbsDir 
      case result of

        FormSuccess formData -> do
          iiD <- liftIO $ mkImgId
          let file = fileInfo' formData
          liftIO $ writeToServer imgsDir file iiD
          liftIO $ mkThumbnail imgsDir
                               thumbsDir 
                               iiD
                               thumbHeight 
          addimage2DB x (pack iiD) 

        FormFailure _ -> do
          setMessage "Please select a file!" 

        FormMissing ->
          setMessage "Please select a file!!!"

      defaultLayout $ do
        fileList <- liftIO $ listDirectory thumbsDir 
        let fileDir = (unpack x) </> "thumbs"
        let th = thumbHeight :: Int
        -- let upldhead = uploadHeader $ length fileList 
        setTitle "Ken-Burns slideshow"
        $(widgetFile "fileupload")

    Nothing -> do
      defaultLayout $ do
        let fileList = [] :: [FilePath] 
            fileDir = "" :: FilePath
        let th = thumbHeight :: Int
        -- let upldhead = uploadHeader 0 
        setTitle "Ken-Burns slideshow"
        $(widgetFile "fileupload")

addimage2DB :: Text -> Text -> HandlerFor App () 
addimage2DB sessId imgId = runDB $ do
  sessImgs <- selectList 
                [ ImageDataSessionID ==. sessId ]
                []
  let i = length (sessImgs) + 1
  _ <- insert $ ImageData sessId imgId i
  return ()

addTesting2DB :: [FilePath] -> HandlerFor App ()
addTesting2DB fp = runDB $ do 
  let bases = map takeBaseName fp 
      indexedBase = zip bases ([1..6] :: [Int])
      inputs = map (\(x,y) -> ImageData "20058826" (pack x) y) indexedBase
  _ <- insertMany inputs 
  return ()

removeduplicatesfromDB :: Text -> HandlerFor App ()
removeduplicatesfromDB imgId = runDB $ do
  testImgs <- selectKeysList 
                [ ImageDataSessionID ==. "00000000"
                , ImageDataImgID ==. imgId ]
                []
  let surplasKey = f testImgs 
                    where f []   = Nothing
                          f (x:_) = Just x
  case surplasKey of 
    Just x -> delete x
    Nothing -> return () 
  return ()

buildForm :: Form UploadForm
buildForm =  renderBootstrap3 BootstrapInlineForm $ UploadForm
  <$> fileAFormReq settings 
  where settings = FieldSettings
          { fsLabel = "Choose a file"
          , fsTooltip = Nothing
          , fsId = Just "fileupload" 
          , fsName = Nothing
          , fsAttrs = []
          }

sampleForm :: Form FileForm
sampleForm = renderBootstrap3 BootstrapBasicForm $ FileForm
    <$> fileAFormReq "Choose a file"
    <*> areq textField textSettings Nothing
    -- Add attributes like the placeholder and CSS classes.
    where textSettings = FieldSettings
            { fsLabel = "What's on the file?"
            , fsTooltip = Nothing
            , fsId = Nothing
            , fsName = Nothing
            , fsAttrs =
                [ ("class", "form-control")
                , ("placeholder", "File description")
                ]
            }

writeToServer :: String -> FileInfo -> String -> IO () 
writeToServer uf file iid = do
  -- let filename = unpack $ fileName file
  let filename = iid ++ ".JPG"
      path = uf </> filename
  liftIO $ fileMove file path
  img   <- loadJpegFile path 
  size  <- imageSize img
  let ratio = (fromIntegral $ fst size :: Double) / 
              (fromIntegral $ snd size :: Double)
  newimg <- resizeImage width (round $ fromIntegral width / ratio) img
  liftIO $ saveJpegFile 74 path newimg 
  -- return filename

img2b64 :: FilePath -> IO ()
img2b64 file = S8.readFile file >>= (\x -> writeFile "test.b64" $ B64.encode x)

mkSessId :: IO String
mkSessId = do
  t <- getCurrentTime
  let i = fromInteger $ diffTimeToPicoseconds $ (utctDayTime t)
  let rs = randomRs (0,9) (mkStdGen i) :: [Integer]
  return $ concat $ map show $ take 8 rs

mkImgId :: IO String
mkImgId = randomWord randomASCII 8

mkThumbnail :: FilePath -> FilePath -> String -> Int -> IO ()
mkThumbnail imgsDir thumbDir iid height = do
  createDirectoryIfMissing True thumbDir 
  let filename = iid ++ ".JPG"
  img   <- loadJpegFile (imgsDir </> filename) 
  size  <- imageSize img
  let ratio = (fromIntegral $ fst size :: Double)/
              (fromIntegral $ snd size :: Double)
  thumb <- resizeImage (round $ ratio * fromIntegral height) height img
  saveJpegFile 84 (thumbDir </> filename) thumb

uploadFolder :: Handler FilePath
uploadFolder = do
  master <- getYesod
  let uf = unpack
                   $ appFileUploadDirectory
                   $ appSettings 
                   $ master
  return uf

sampleRandomFiles :: Int -> [(String,Int)] -> IO [(String,Int)]
sampleRandomFiles n xs = do
  t <- getCurrentTime
  let i = fromInteger $ diffTimeToPicoseconds $ (utctDayTime t)
  let rs = randomRs (1,242) (mkStdGen i) :: [Int]
  let frs = take n $ nub rs 
  return $ filter (\(_,indx) -> elem indx frs) xs

getDownloadR :: Handler TypedContent
getDownloadR = do
  sid <- lookupSession "sessionID"
  case sid of
    Just x  -> do
      fl <- getFileList x 
      uf <- uploadFolder
      -- page <- liftIO $ mkPageDl uf (unpack x) fl 50 
      settings <- mkSettingsDefault uf (unpack x) fl   
      page <- liftIO $ mkPageDl settings 
      let html = fmap renderHtml $ withUrlRenderer $ pageHtml page
          css = map (renderCssUrl undefined) $ pageCss page
          js = renderJavascriptUrl undefined $ pageJs page
      _html <- fmap (fromString . L.unpack) html
      let _css = fromString $ fold $ map L.unpack css 
          _js = fromString $ L.unpack js 
      time <- liftIO $ round `fmap` getPOSIXTime

      let imgName p = "imgs" </> p ++ ".JPG" 
          imgLoc p = uf </> (unpack x) </> (imgName p) 
          g y = (imgName y,B.readFile (imgLoc y))
          f (n,b) = fmap (toEntry n time) b

      pics <- liftIO $ sequence $ map (f . g . fst) fl

      let hE = toEntry "slideshow.html" time _html
          cE = toEntry "slideshow.css" time _css
          jE = toEntry "slideshow.js" time _js
          entries = [hE,cE,jE] ++ pics
          zipfile = fromArchive $ addEntriesToArchive entries emptyArchive
      addHeader "Content-Disposition" $ concat
            [ "attachment; filename=\"", "slideshow.zip", "\""]
      sendResponse ( fromString "application/zip" :: ByteString, toContent zipfile)

addEntriesToArchive :: [Entry] -> Archive -> Archive
addEntriesToArchive []     a = a
addEntriesToArchive (e:es) a = addEntryToArchive e $ addEntriesToArchive es a

data Duration = Duration { duration :: Int }


resultsNav :: Text -> Int -> HtmlUrl (Route App) 
resultsNav sid d = do
  [hamlet|
  <div #afterpost .nav-container>
    <button .nav-btn.my-tooltip 
      onclick="window.location.href='@{UploadFileR}';">
      <i .fa.fa-home>
      <span .tooltiptext>
        Go back to the starting page.
    <button .nav-btn.my-tooltip 
      onclick="window.location.href='@{DownloadR}';">
      <i .fa.fa-download>
      <span .tooltiptext>
        Download a copy of your slideshow
    <button .nav-btn.my-tooltip 
      onclick="displayLinkDiv();">
      <i .fa.fa-chain>
      <span .tooltiptext style="width:300px;">
        Get a link to your slide show
    <button .nav-btn.my-tooltip 
      onclick="displaySettingsDiv();">
      <i .fa.fa-cogs>
      <span .tooltiptext style="width:300px;">
        Fiddle with various settings
    <div .slidecontainer>
      <input type="range" min="1" max="100" class="slider" value="#{show d}"
        id="durationSlider" name="duration">
<div #linkdisplay .content-bubble.container>
    <div .input-group>
      <span .input-group-addon.set-bg>
        <i .fa.fa-chain>
      <input #sharing-link type="text" .form-control.set-bg 
        value=@{ResultByIdR sid} readonly>
      <div .input-group-btn>
        <button .btn.btn-link.btn-default 
          onclick="copyToClipBoard();">
          <i .fa.fa-files-o>
    |]

getSettingsR :: Text -> Handler Html
getSettingsR sid = do
  fl <- getFileList sid
  uf <- uploadFolder
  settings <- mkSettings uf 
                            (unpack sid)
                            fl 
                            50
                            (Just (lookupGetParam . (++ "Object")))
                            (Just (lookupGetParam . (++ "maxZoom")))
                            (Just (lookupGetParam . (++ "minZoom")))
                            (Just (lookupGetParam . (++ "Random")))
                            Nothing
  obs <- liftIO $ json2info settings
  let objectsSorter o1 o2 = compare (sco o2) (sco o1)
      imgSorter (_,i1) (_,i2) = compare (GetJson.index i1) (GetJson.index i2)
      fileDir = (unpack sid) </> "thumbs"
  pc <- widgetToPageContent $ do
    $(widgetFile "settings")
  withUrlRenderer
    [hamlet|
              $doctype 5
              <html>
                  <head>
                      <title>Hmmmm?
                      <meta charset=utf-8>
                      ^{pageHead pc}
                  <body>
                      ^{pageBody pc}
          |]

selectedObject :: PageSettings -> [String]
selectedObject pages = Map.elems m 
  where m = map (unpack . f) p
        p = pagesettings pages 
        f x = case obj x of
                Just t  -> unpack t
                Nothing -> val $ (sortBy g $ objs x)!!0
        g o1 o2 = compare (sco o2) (sco o1)

imageFp :: FilePath -> SessionId -> ImageId -> FilePath
imageFp uf sid x  = uf </> sid </> "imgs" </> x ++ ".JPG"

rawImgWidth :: FilePath -> Int
rawImgWidth _ = width

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

mkSettings :: FilePath
           -> SessionId
           -> [(ImageId,Int)]
           -> Int
           -> Maybe (Text -> Handler (Maybe Text))
           -> Maybe (Text -> Handler (Maybe Text))
           -> Maybe (Text -> Handler (Maybe Text)) 
           -> Maybe (Text -> Handler (Maybe Text)) 
           -> Maybe Int 
           -> Handler SettingsData
mkSettings uf sid fl gdur objF maxZF minZF randF dur = do
  let a = map fst fl
  obs <- mapM ((fromMaybe (\_ -> return Nothing) objF) . pack) a
  maxzms <- mapM ((fromMaybe (\_ -> return $ Just "50") maxZF) . pack) a
  minzms <- mapM ((fromMaybe (\_ -> return $ Just "50") minZF) . pack) a
  randss <- mapM ((fromMaybe (\_ -> return $ Just "50") randF) . pack) a
  let mo = Map.fromList $ zip a obs
      maxz = Map.fromList $ zip a maxzms
      minz = Map.fromList $ zip a minzms
      randz = Map.fromList $ zip a randss
      f (x,y) = (x,ImageSettings x
                                 y
                                 (mo Map.! x)
                                 (maxz Map.! x)
                                 (minz Map.! x)
                                 (randz Map.! x)
                                 (fromMaybe 0 dur)
                                 width
                                 (rawImgHeight $ imageFp uf sid x)
                                 (rawImgRatio $ imageFp uf sid x))
      m = Map.fromList $ map f fl
  return $ SettingsData sid uf gdur m 

mkSettingsDefault :: FilePath
                  -> SessionId
                  -> [(ImageId,Int)] 
                  -> Handler SettingsData
mkSettingsDefault uf sid fl = mkSettings uf 
                                         sid 
                                         fl 
                                         60 
                                         Nothing 
                                         Nothing 
                                         Nothing
                                         Nothing 
                                         Nothing

buildQuery :: SettingsData -> QueryString
buildQuery globalsettings = intercalate "&" $ concat params 
  where m = Map.toList $ settings globalsettings
        f (k,v) = [case (obj v) of  
                    Just b  -> (k ++ "Object", unpack b)
                    Nothing -> (k ++ "Null", "Null")
                 ,(k ++ "maxZoom", unpack $ fromMaybe "Nothing" $ maxzoom v)
                 ,(k ++ "minZoom", unpack $ fromMaybe "Nothing" $ minzoom v)
                 ,(k ++ "Random", unpack $ fromMaybe "Nothing" $ rand v)]
        p (x,y) = x ++ "=" ++ y
        params = map ((map p) . f) m
