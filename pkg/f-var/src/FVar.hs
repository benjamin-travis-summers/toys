{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase          #-}

module FVar
    ( FVar
    , loadFVar
    , readFVar
    , writeFVar
    ) where

--------------------------------------------------------------------------------------------------------------

import ClassyPrelude
import Data.LargeWord

import Data.Binary (Binary)
import Prelude ()

import qualified System.Directory   as Sys
import qualified Data.Binary        as Binary
import qualified Crypto.Hash.SHA256 as SHA256

--------------------------------------------------------------------------------------------------------------

data FVar a = FVar { key :: Word256
                   , box :: IORef a
                   }

fvarPath :: FVar a -> IO FilePath
fvarPath fvar = do
    pwd <- Sys.getCurrentDirectory
    let dir = pwd </> ".fvar"
    Sys.createDirectoryIfMissing False dir
    let result = dir </> show (key fvar)
    pure result

loadFVar :: Binary a => Text -> a -> IO (FVar a)
loadFVar varNm initialValue = do
    let nmHash :: Word256 = Binary.decode $ fromStrict $ SHA256.hash $ encodeUtf8 varNm

    box <- newIORef initialValue

    let result = (FVar { key=nmHash, box=box })

    filePath <- fvarPath result

    Sys.doesFileExist filePath >>= \case
        True -> do
            v <- (Binary.decode . fromStrict) <$> readFile filePath
            writeIORef box v
        False -> do
            writeFile filePath (toStrict $ Binary.encode initialValue)

    pure result

readFVar :: FVar a -> IO a
readFVar v = readIORef (box v)

-- TODO Use STM (instead of just an IORef) to avoid race conditions here.
writeFVar :: Binary a => FVar a -> a -> IO ()
writeFVar var val = do
  fn <- fvarPath var
  writeFile fn (toStrict $ Binary.encode val)
  writeIORef (box var) val
