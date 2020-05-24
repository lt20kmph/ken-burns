{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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
import           Graphics.GD
import           System.Random
import           Test.RandomStrings
import           GetJson
import           System.FilePath (takeBaseName)
import           Prelude (read)
import           Data.List (nub)
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

width :: Int
width = 1500

thumbHeight :: Int
thumbHeight = 168

getLoadingR :: Handler Html
getLoadingR = do
  defaultLayout $ do
    setTitle "loading.. "
    let header = [hamlet|
      <h1>Generating your slideshow...
        |]
    $(widgetFile "loading")

getResultR :: Handler Html
getResultR = do
  sid <- lookupSession "sessionID"
  case sid of
    Just x  -> do
      let navbar = resultsNav x 50 
      fl <- getFileList x
      uf <- uploadFolder
      liftIO $ mainJSON uf (unpack x) fl
      -- page <- liftIO $ mkPageResult uf (unpack x) fl 50 
      -- let slideshow = pageHtml page 
      let slideshow = getResultById x fl uf 50
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
      defaultLayout $ do
        setTitle "Oops!"
        $(widgetFile "results")

getResultById :: Text -> [(String,Int)] -> FilePath -> Int -> Widget 
getResultById sid xs fp dur = do
  page <- liftIO $ mkPageResult fp (unpack sid) xs dur 
  toWidget $ pageHtml page 
  fold (map toWidget $ pageCss page) 
  toWidgetBody $ pageJs page

getResultByIdR :: Text -> Handler Html
getResultByIdR sid = do
  setSession "sessionID" sid 
  fl <- getFileList sid
  uf <- uploadFolder
  defaultLayout $ do
    setTitle "Ken-Burns slideshow"
    let navbar = resultsNav sid 50 
    let slideshow = getResultById sid fl uf 50
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

getUploadFileR :: Handler Html
getUploadFileR = do
  (formWidget_, enctype) <- generateFormPost buildForm
  iD <- liftIO $ mkSessId
  setSession "sessionID" (pack iD)
  uf <- uploadFolder
  -- fp <- liftIO $ listDirectory $ uf </> "00000000" </> "imgs"
  -- addTesting2DB fp
  fl <- getFileList exampleSid
  -- sfl <- liftIO $ sampleRandomFiles 6 fl
  let slideshow = getResultById exampleSid fl uf 50
  let navbar = uploadNav
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

postUploadFileR :: Handler Html
postUploadFileR = do
  ((result, formWidget_), enctype) <- runFormPost buildForm
  sid <- lookupSession "sessionID"
  fl <- getFileList exampleSid
  uf <- uploadFolder
  -- sfl <- liftIO $ sampleRandomFiles 12 fl
  let slideshow = getResultById exampleSid fl uf 50
  let navbar = uploadNav
  case sid of

    Just x -> do 
      let imgsDir = uf </> (unpack x) </> "imgs"
      let thumbsDir = uf </> (unpack x) </> "thumbs"
      liftIO $ createDirectoryIfMissing True imgsDir 
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

        _ -> do
          setMessage "Sorry, something bad happened, please try again."

      defaultLayout $ do
        fileList <- liftIO $ listDirectory thumbsDir 
        let fileDir = (unpack x) </> "thumbs"
        let th = thumbHeight :: Int
        setTitle "Ken-Burns slideshow"
        $(widgetFile "fileupload")

    Nothing -> do
      defaultLayout $ do
        let fileList = [] :: [FilePath] 
            fileDir = "" :: FilePath
        let th = thumbHeight :: Int
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
      inputs = map (\x -> ImageData "00000000" (pack x) (read x)) bases
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
      page <- liftIO $ mkPageDl uf (unpack x) fl 50 
      let html = fmap renderHtml $ withUrlRenderer $ pageHtml page
          css = map (renderCssUrl undefined) $ pageCss page
          js = renderJavascriptUrl undefined $ pageJs page
      _html <- fmap (fromString . L.unpack) html
      let _css = fromString $ fold $ map L.unpack css 
      let _js = fromString $ L.unpack js 
      time <- liftIO $ round `fmap` getPOSIXTime

      let imgName p = "imgs" </> p ++ ".JPG" 
      let imgLoc p = uf </> (unpack x) </> (imgName p) 
      let g x = (imgName x,B.readFile (imgLoc x))
      let f (n,b) = fmap (toEntry n time) b

      pics <- liftIO $ sequence $ map (f . g . fst) fl

      let hE = toEntry "slideshow.html" time _html
      let cE = toEntry "slideshow.css" time _css
      let jE = toEntry "slideshow.js" time _js
      let entries = [hE,cE,jE] ++ pics
      let zipfile = fromArchive $ addEntriesToArchive entries emptyArchive
      addHeader "Content-Disposition" $ concat
            [ "attachment; filename=\"", "slideshow.zip", "\""]
      sendResponse ( fromString "application/zip" :: ByteString, toContent zipfile)

addEntriesToArchive :: [Entry] -> Archive -> Archive
addEntriesToArchive []     a = a
addEntriesToArchive (e:es) a = addEntryToArchive e $ addEntriesToArchive es a

data Duration = Duration { duration :: Int }


resultsNav :: Text -> Int -> HtmlUrl (Route App) 
resultsNav sid d = [hamlet|
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

-- postResultByIdR :: Text -> Handler Html
-- postResultByIdR fuck = do
--   dur <- runInputPost $ Duration 
--                      <$> ireq intField "duration"
--   liftIO $ putStrLn $ pack $ show $ duration dur
--   let sid = Just fuck
--   case sid of
--     Just x -> do
--       setSession "sessionID" x 
--       fl <- getFileList x
--       uf <- uploadFolder
--       defaultLayout $ do
--         setTitle "Ken-Burns slideshow"
--         let navbar = resultsNav $ duration dur
--         let slideshow = getResultById x fl uf $ duration dur
--         $(widgetFile "results")
