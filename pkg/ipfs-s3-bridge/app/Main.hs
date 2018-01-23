-- Upload a file or directory both to s3 and to IPFS.

module Main where

--------------------------------------------------------------------------------------------------------------

import ClassyPrelude hiding (bracket)
import Control.Lens
import Control.Lens.TH
import Data.Conduit

import RIO (RIO, runRIO, bracket)
import System.Directory (removeFile)
import Data.Conduit.Shell (($|), ipfs, echo)
import Control.Monad.Trans.Resource (ResourceT)

import qualified Data.Text                   as Text
import qualified Data.Conduit.Binary         as CB
import qualified Data.Conduit.List           as CL
import qualified Data.Conduit.Shell          as Sh
import qualified Data.Conduit.Shell.Segments as Sh
import qualified Data.Conduit.Text           as CT

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

withTmpFile :: Text -> (FilePath -> RIO env a) -> RIO env a
withTmpFile block action =
  bracket (pure tmpfn) (liftIO . removeFile) action
  where tmpfn = unpack (block <> ".block")

linesSink :: ConduitM ByteString c IO [Text]
linesSink = CT.decodeUtf8 .| CT.lines .| CL.consume

shellLines :: Sh.Segment () -> RIO env [Text]
shellLines = liftIO . Sh.run . Sh.texts

listBlockClosure :: Text -> RIO env [Text]
listBlockClosure block =
    shellLines (ipfs "refs" "-r" "-u" block)

listBlockRefs :: Text -> RIO env [Text]
listBlockRefs block = do
    shellLines (ipfs "refs" "-u" block)

listAllPinnedBlocks :: RIO env [Text]
listAllPinnedBlocks = do
    traceM "listing pinned blocks"
    roots  <- shellLines (ipfs "pin" "ls" "-q" "--type=recursive")
    direct <- shellLines (ipfs "pin" "ls" "-q" "--type=direct")
    let pinned = roots <> direct
    traceM "These blocks are pinned:"
    traceM (show pinned)
    pure pinned

persistBlock :: HasConfig env => Text -> RIO env ()
persistBlock block = do
    withTmpFile block $ \tmp -> do
        bucket ← getBucket
        let s3Url = unpack ("s3://" <> bucket <> "/block/" <> block)
        liftIO $ do
            Sh.run $ do
                echo "$ ipfs" "block" "get" block
                Sh.shell ("ipfs block get " <> unpack block <> " >" <> tmp)
                echo "$ aws" "s3" "cp" tmp s3Url
                Sh.proc "/usr/bin/aws" ["s3", "cp", tmp, s3Url]

restoreBlock :: HasConfig env => Text -> RIO env ()
restoreBlock block = do
    withTmpFile block $ \tmp -> do
        bucket ← getBucket
        let s3Url = unpack ("s3://" <> bucket <> "/block/" <> block)
        liftIO $ Sh.run $ do
            echo "Restoring: " block
            Sh.echo "$ aws" ["s3", "cp", s3Url, tmp]
            Sh.proc "/usr/bin/aws" ["s3", "cp", s3Url, tmp]
            echo ("$ ipfs block put <" <> tmp)
            Sh.shell ("ipfs block put <" <> tmp)

s3pin :: HasConfig env => Text -> RIO env ()
s3pin block = do
    bucket ← getBucket

    let fn     = unpack block
    let pinUrl = unpack ("s3://" <> bucket <> "/pin/" <> block)

    liftIO $ do
        Sh.run $ do
            writeFile (pack fn) (encodeUtf8 $ pack fn)
            Sh.proc "/usr/bin/aws" ["s3", "cp", fn, pinUrl]
        removeFile (fn :: String)

persistClosure block = do
    traceM ("persisting the closure of: " <> unpack block)
    persistBlock block
    s3pin block
    listBlockClosure block >>= traverse_ persistBlock

-- TODO This will do duplicate work if there are shared subtrees.
restoreClosure :: HasConfig env => Text -> RIO env ()
restoreClosure block = do
    traceM ("restoring the closure of: " <> unpack block)
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
  liftIO $ Sh.run (ipfs "pin" "add" block)

restorePinned = do
    s3ListAllBlocks >>= traverse_ restoreBlock
    s3ListAllPins >>= traverse_ ipfsPinBlock

main :: IO ()
main = do
    let config = Config { _bucketL = "ipfs-archive-backups" }
    runRIO config $
      liftIO getArgs >>= \case
        "persist-all"   : [] → persistPinned
        "restore-all"   : [] → restorePinned
        "persist-tree"  : bs → for_ bs persistClosure
        "restore-tree"  : bs → for_ bs restoreClosure
        "persist-block" : bs → for_ bs persistClosure
        "restore-block" : bs → for_ bs restoreClosure
