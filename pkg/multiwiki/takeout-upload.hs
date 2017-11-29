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
import Control.Lens    as Lens
import Web.Scotty      as S

import System.FilePath ((</>))

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
main = scotty 3000 $ do
  get "/" $ do
    html $ mconcat
      [ "<!DOCTYPE html>"
      , "<html>"
      , "<body>"
      , ""
      , "<form action=\"./upload\" method=\"post\" enctype=\"multipart/form-data\">"
      , "    Select tarball to upload:"
      , "    <input type=\"file\" name=\"fileToUpload\" id=\"fileToUpload\">"
      , "    <input type=\"submit\" value=\"Upload Tarball\" name=\"submit\">"
      , "</form>"
      , ""
      , "</body>"
      , "</html>"
      ]

  post "/upload" $ do
    fs <- files

    let fs' = do (fieldName, fi) <- fs
                 pure (fieldName, BS.unpack (Wai.fileName fi), Wai.fileContent fi)

    io $ mkDir "./takeout-tarballs"

    io $ do
      for_ fs' $ \(_, fn, fc) → do
        let tarballFile = "./takeout-tarballs/" </> fn
        B.writeFile tarballFile fc
        Sys.callProcess "./mkwiki.sh" ["./takeout-tarballs", tarballFile]

    html "<h1>File uploaded and processed</h1>"
