{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

import ClassyPrelude
import Control.Lens    as Lens hiding ((<.>))
import Web.Scotty      as S

import System.FilePath ((</>), (<.>))

import qualified Prelude
import qualified Network.Wai.Middleware.RequestLogger as Wai
import qualified Network.Wai.Middleware.Static        as Wai
import qualified Network.Wai.Parse                    as Wai
import qualified System.Directory                     as Sys
import qualified Data.ByteString.Lazy                 as B
import qualified Data.ByteString.Char8                as BS
import qualified System.Process                       as Sys

io = liftIO

mkDir ∷ FilePath → IO FilePath
mkDir path = do
  ex ← Sys.doesDirectoryExist path
  unless ex (Sys.createDirectoryIfMissing True path)
  pure path

main ∷ IO ()
main = do
  args <- getArgs

  let port = args & \case [port] → Prelude.read (unpack port)
                          []     → 3000

  scotty port $ do
    get "/" $ do
      html $ mconcat
        [ "<!DOCTYPE html>"
        , "<html>"
        , "<body>"
        , ""
        , "<a href=\"./wiki\">See wiki documents.</a>"
        , ""
        , "<br />"
        , ""
        , "<form action=\"./upload\" method=\"post\" enctype=\"multipart/form-data\">"
        , "    Select tarball to upload:"
        , "    <input type=\"file\" name=\"fileToUpload\" id=\"fileToUpload\">"
        , "    <br />"
        , "    <input type=\"submit\" value=\"Upload Tarball\" name=\"submit\">"
        , "</form>"
        , ""
        , "</body>"
        , "</html>"
        ]

    get "/wiki/" $ do
      let docNames = [ "AnnabelleLane"
                     , "ConorLondon"
                     , "KeepNoteAboutPizzaX"
                     , "OldSimpleTextWidgetContents"
                     , "TestNote"
                     ]

      let wikiDoc nm = "<li><a href=\"" <> nm <> "\">" <> nm <> "</a></li>"

      html $ mconcat (["<ul>"] <> (wikiDoc <$> docNames) <> ["</ul>"])


    get "/wiki/:doc" $ do
      docName <- param "doc"
      setHeader "Content-Type" "text/html"
      file ("./out/wiki/" </> docName <.> ".html")

    post "/upload" $ do
      fs <- files

      let fs' = do (fieldName, fi) <- fs
                   pure (fieldName, BS.unpack (Wai.fileName fi), Wai.fileContent fi)

      io $ mkDir "./takeout-tarballs"

      io $ do
        for_ fs' $ \(_, fn, fc) → do
          let tarballFile = "./takeout-tarballs/" </> fn
          B.writeFile tarballFile fc
          Sys.callProcess "./runme.sh" [tarballFile]

      html "<h1>File uploaded and processed</h1>"
