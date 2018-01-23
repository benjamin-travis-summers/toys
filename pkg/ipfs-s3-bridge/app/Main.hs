-- Upload a file or directory both to s3 and to IPFS.

module Main where

--------------------------------------------------------------------------------------------------------------

import RIO
import Data.Conduit
import Control.Lens.TH
import Data.Acquire

import System.Directory (removeFile)
import ClassyPrelude (unpack, getArgs)
import Data.Conduit.Shell (($|))
import Control.Monad.Trans.Resource (ResourceT)
import Control.Monad.Trans.Control (MonadBaseControl (..))
import Control.Monad.Base (MonadBase (..))

import qualified Data.Text           as Text
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List   as CL
import qualified Data.Conduit.Shell  as Sh
import qualified Data.Conduit.Text   as CT

--------------------------------------------------------------------------------------------------------------

instance MonadBase IO (RIO env) where
  liftBase = undefined

instance MonadBaseControl IO (RIO env) where
  type StM (RIO env) a = a
  liftBaseWith _f = undefined
  restoreM = undefined

--------------------------------------------------------------------------------------------------------------

data Config = Config { _bucketL :: !Text }

makeLenses ''Config

class HasConfig env where
  configL :: Lens' env Config

instance HasConfig Config where
  configL = id

getBucket :: HasConfig env => RIO env Text
getBucket = view (configL . bucketL)

--------------------------------------------------------------------------------------------------------------

blockTmpFile :: Text -> Acquire FilePath
blockTmpFile block = mkAcquire (pure $ unpack $ block <> ".block") removeFile

withTmpFile :: Text -> (FilePath -> RIO env a) -> RIO env a
withTmpFile block action = do
    with (blockTmpFile block) action
    -- let tmp = unpack (block <> ".block")
    -- result <- action tmp
    -- liftIO (removeFile tmp)
    -- pure result

linesSink :: ConduitM ByteString c IO [Text]
linesSink = CT.decodeUtf8 .| CT.lines .| CL.consume

shellLines :: Sh.Segment () -> RIO env [Text]
shellLines seg = liftIO $ Sh.run (seg $| Sh.conduit linesSink)

listBlockClosure :: Text -> RIO env [Text]
listBlockClosure block =
    shellLines (Sh.ipfs "refs" "-r" "-u" block)

listBlockRefs :: Text -> RIO env [Text]
listBlockRefs block = do
    shellLines (Sh.ipfs "refs" "-u" block)

listAllPinnedBlocks :: RIO env [Text]
listAllPinnedBlocks = do
    let listPinned    = Sh.ipfs "pin" "ls"
    let justFilenames = Sh.sed "s/ .*//"
    shellLines (listPinned $| justFilenames)

sinkFile :: FilePath -> ConduitM ByteString o IO ()
sinkFile = undefined (CB.sinkFile ∷ FilePath -> ConduitM ByteString o (ResourceT IO) ())

sourceFile :: FilePath -> ConduitM i ByteString IO ()
sourceFile = undefined (CB.sourceFile ∷ FilePath -> ConduitM i ByteString (ResourceT IO) ())

persistBlock :: HasConfig env => Text -> RIO env ()
persistBlock block = do
    withTmpFile block $ \tmp -> do
        bucket ← getBucket
        let s3Url = unpack ("s3://" <> bucket <> "/block/" <> block)
        liftIO $ do
            Sh.run (Sh.ipfs "block" "get" block $| Sh.conduit (sinkFile tmp))
            Sh.run (Sh.proc "/usr/bin/aws" ["s3", "cp", tmp, s3Url])

restoreBlock :: HasConfig env => Text -> RIO env ()
restoreBlock block = do
    withTmpFile block $ \tmp -> do
        bucket ← getBucket
        let s3Url = unpack ("s3://" <> bucket <> "/block/" <> block)
        liftIO $ Sh.run $ do
            Sh.proc "/usr/bin/aws" ["s3", "cp", s3Url, tmp]
            Sh.conduit (sourceFile tmp) $| Sh.ipfs "block" "put"

s3pin :: HasConfig env => Text -> RIO env ()
s3pin block = do
    bucket ← getBucket

    let fn     = unpack block
    let pinUrl = unpack ("s3://" <> bucket <> "/pin/" <> block)

    liftIO $ do
        Sh.run $ do
            Sh.echo fn $| Sh.conduit (sinkFile fn)
            Sh.proc "/usr/bin/aws" ["s3", "cp", fn, pinUrl]
        removeFile (fn :: String)

persistClosure block = do
    persistBlock block
    s3pin block
    listBlockClosure block >>= traverse_ persistBlock

-- TODO This will do duplicate work if there are shared subtrees.
restoreClosure :: HasConfig env => Text -> RIO env ()
restoreClosure block = do
  restoreBlock block
  listBlockRefs block >>= traverse_ restoreClosure

-- TODO This will duplicate lots of work!
persistPinned :: HasConfig env => RIO env ()
persistPinned = do
  listAllPinnedBlocks >>= traverse_ persistClosure

looksLikeAnIpfsBlockHash t = Text.take 2 t == "Qm"

s3ListAllBlocks = do
    bucket <- getBucket
    let s3BlockDir = unpack ("s3://" <> bucket <> "/block/")
    result <- shellLines (Sh.proc "/usr/bin/aws" ["s3", "ls", s3BlockDir])
    pure (filter looksLikeAnIpfsBlockHash result)

s3ListAllPins = do
    bucket <- getBucket
    let s3PinDir = unpack ("s3://" <> bucket <> "/pin/")
    result <- shellLines (Sh.proc "/usr/bin/aws" ["s3", "ls", s3PinDir])
    pure (filter looksLikeAnIpfsBlockHash result)

ipfsPinBlock block =
  liftIO $ Sh.run (Sh.ipfs "pin" "add" block)

restorePinned = do
    s3ListAllBlocks >>= traverse_ restoreBlock
    s3ListAllPins >>= traverse_ ipfsPinBlock

main :: IO ()
main = do
    let config = Config { _bucketL = "ipfs-archive-backups" }
    runRIO config $
      liftIO getArgs >>= \case
        ["all"]        → persistPinned
        ["restoreall"] → restorePinned
        blocks         → for_ blocks persistClosure
