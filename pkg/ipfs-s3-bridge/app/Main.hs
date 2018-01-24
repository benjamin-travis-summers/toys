-- TODO Testing

module Main where

--------------------------------------------------------------------------------------------------------------

import FattyPrelude

import qualified Data.Conduit.Binary         as CB
import qualified Data.Conduit.List           as CL
import qualified Data.Conduit.Text           as CT
import qualified Data.Text                   as Text
import qualified Turtle                      as Sh
import qualified Control.Foldl               as Fold

--------------------------------------------------------------------------------------------------------------

type MultiHash = Text
type BlockSet = Set MultiHash

data Block = Block { bHash :: !MultiHash
                   , bData :: !ByteString
                   }
  deriving (Show, Eq)

data Config = Config { cPinned    :: TVar (Maybe BlockSet)
                     , cPersisted :: TVar (Maybe BlockSet)
                     , cBucket    :: !Text
                     }

data BlockStore env = BlockStore { bsGet  :: MultiHash  -> RIO env Block
                                 , bsPut  :: ByteString -> RIO env Block
                                 , bsPin  :: MultiHash  -> RIO env ()
                                 , bsPins :: RIO env (Set MultiHash)
                                 }

makeFields ''Block
makeFields ''Config
makeFields ''BlockStore

class HasConfig env where configL :: Lens' env Config
instance HasConfig Config where configL = id

-- Utilities -------------------------------------------------------------------------------------------------

initialize ∷ (Lens' env (TVar (Maybe a))) → RIO env a → RIO env a
initialize varLens buildVal =
  do var ← view varLens
     atomically (readTVar var) >>= \case
       Just val → pure val
       Nothing  → do val ← buildVal
                     atomically $ writeTVar var (Just val)
                     pure val

withTmpFile :: Text -> (Text -> RIO env a) -> RIO env a
withTmpFile block action = bracket (pure (block <> ".block")) (rm . fromString . unpack) action

withTmpFileIO :: Text -> (Text -> IO a) -> IO a
withTmpFileIO block action = bracket (pure (block <> ".block")) (rm . fromString . unpack) action

linesSink :: ConduitM ByteString c IO [Text]
linesSink = CT.decodeUtf8 .| CT.lines .| CL.consume

shellLines :: MonadIO m => Shell Line -> m [Text]
shellLines sh = Sh.fold (fmap lineToText sh) Fold.list

listBlockClosure :: Text -> RIO env [Text]
listBlockClosure block =
    shellLines (inproc "ipfs" ["refs", "-r", "-u", block] empty)

listBlockRefs :: Text -> RIO env [Text]
listBlockRefs block = do
    shellLines (inproc "ipfs" ["refs", "-u", block] empty)

--------------------------------------------------------------------------------------------------------------

data LogLevel = DEBUG
  deriving (Show)

data Log = Note Text
         | IpfsPinnedBlocks BlockSet
         | S3PinnedBlocks BlockSet
  deriving (Show)

disp :: Log -> Text
disp (Note t) = t
disp l        = tshow l

log :: MonadIO m => LogLevel -> Log -> m ()
log lvl l = putStrLn (tshow lvl <> " " <> disp l)

-- | All the pinned roots on the local IPFS node.
ipfsPinnedBlocks ∷ HasConfig env => RIO env BlockSet
ipfsPinnedBlocks = initialize (configL . cPinnedL) listPins
                     where listPins =
                             do log DEBUG (Note "Listing Pinned Blocks")
                                roots  ← shellLines (inproc "ipfs" ["pin", "ls", "-q", "--type=recursive"] empty)
                                direct ← shellLines (inproc "ipfs" ["pin", "ls", "-q", "--type=direct"] empty)
                                let blockSet = setFromList (roots <> direct)
                                log DEBUG (IpfsPinnedBlocks blockSet)
                                pure (setFromList (roots <> direct))

-- | All the pinned roots on the local IPFS node.
ipfsAllBlocks ∷ HasConfig env => RIO env BlockSet
ipfsAllBlocks = initialize (configL . cPinnedL) listPins
                  where listPins =
                          do log DEBUG (Note "Listing all blocks on IPFS.")
                             blocks ← shellLines (inproc "ipfs" ["pin", "ls", "-q", "--type=all"] empty)
                             log DEBUG (Note "Finished")
                             pure (setFromList blocks)

looksLikeAnIpfsBlockHash :: Text -> Bool
looksLikeAnIpfsBlockHash t = Text.take 2 t == "Qm"

-- | All the persisted roots on s3.
s3PinnedBlocks ∷ HasConfig env => RIO env BlockSet
s3PinnedBlocks = initialize (configL . cPersistedL) listPins
                   where listPins =
                           do log DEBUG (Note "Listing all blocks on S3.")
                              bucket ← view (configL . cBucketL)
                              let s3PinDir = "s3://" <> bucket <> "/pin/"
                              result ← shellLines (inproc "/usr/bin/aws" ["s3", "ls", s3PinDir] empty)
                              log DEBUG (Note "Finished")
                              pure $ setFromList (filter looksLikeAnIpfsBlockHash result)

-- | All the persisted roots on s3.
s3AllBlocks ∷ HasConfig env => RIO env BlockSet
s3AllBlocks = initialize (configL . cPersistedL) listPins
                where listPins =
                        do bucket ← view (configL . cBucketL)
                           let s3BlockDir = ("s3://" <> bucket <> "/block/") :: Text
                           log DEBUG (Note "Listing all blocks on S3.")
                           log DEBUG (Note $ unwords ["/usr/bin/aws", "s3", "ls", s3BlockDir])
                           result ← shellLines (inproc "/usr/bin/aws" ["s3", "ls", s3BlockDir] empty)
                           log DEBUG (Note "Finished")
                           pure $ setFromList (filter looksLikeAnIpfsBlockHash result)

--------------------------------------------------------------------------------------------------------------

blockPinnedIpfs, blockOnIpfs, blockPinnedS3, blockOnS3 :: HasConfig env => MultiHash -> RIO env Bool

blockPinnedIpfs b = member b <$> ipfsPinnedBlocks
blockOnIpfs b     = member b <$> ipfsAllBlocks
blockPinnedS3 b   = member b <$> s3PinnedBlocks
blockOnS3 b       = member b <$> s3AllBlocks

--------------------------------------------------------------------------------------------------------------

persistBlock :: HasConfig env => Text -> RIO env ()
persistBlock block =
  do log DEBUG (Note $ "persist: " <> block)
     unlessM (blockOnS3 block) $
       withTmpFile block $ \tmp ->
         do bucket ← view (configL . cBucketL)
            let s3Url = "s3://" <> bucket <> "/block/" <> block
            log DEBUG (Note $ unwords ["$ ipfs", "block", "get", block])
            shell ("ipfs block get " <> block <> " >" <> tmp) empty
            log DEBUG $ Note $ unwords ["$ aws", "s3", "cp", tmp, s3Url]
            void $ proc "/usr/bin/aws" ["s3", "cp", tmp, s3Url] empty

restoreBlock :: HasConfig env => Text -> RIO env ()
restoreBlock block =
  do log DEBUG (Note $ "restore: " <> block)
     unlessM (blockOnIpfs block) $
       withTmpFile block $ \tmp ->
         do bucket ← view (configL . cBucketL)
            let s3Url = "s3://" <> bucket <> "/block/" <> block
            log DEBUG $ Note ("Restoring: " <> block)
            log DEBUG (Note $ unwords ["$ aws", "s3", "cp", s3Url, tmp])
            proc "/usr/bin/aws" ["s3", "cp", s3Url, tmp] empty
            log DEBUG $ Note ("$ ipfs block put <" <> tmp)
            void $ shell ("ipfs block put <" <> tmp) empty

--------------------------------------------------------------------------------------------------------------

s3pin :: HasConfig env => Text -> RIO env ()
s3pin block =
  do log DEBUG (Note $ "pin on s3: " <> block)
     unlessM (blockPinnedS3 block) $
       do bucket ← view (configL . cBucketL)
          let pinUrl = "s3://" <> bucket <> "/pin/" <> block
          writeFile (unpack block) (encodeUtf8 block)
          proc "/usr/bin/aws" ["s3", "cp", block, pinUrl] empty
          rm (fromString $ unpack block)

ipfsPinBlock :: HasConfig env => Text -> RIO env ()
ipfsPinBlock block =
  do log DEBUG (Note $ "pin on ipfs: " <> block)
     unlessM (blockPinnedIpfs block) $
       void $ proc "ipfs" ["pin", "add", block] empty

-- S3 Block Store --------------------------------------------------------------------------------------------

s3BlockStore :: forall env. HasConfig env => BlockStore env
s3BlockStore =
  BlockStore
    { bsGet = \hash → do
         do s3Url ← do bucket ← view (configL . cBucketL)
                       pure ("s3://" <> bucket <> "/block/" <> hash)
            withTmpFile hash $ \tmpfile -> do
                void $ proc "/usr/bin/aws" ["s3", "cp", s3Url, tmpfile] empty
                bytes ← readFile (unpack tmpfile)
                pure Block{bData=bytes, bHash=hash}

    , bsPut = \bytes → do
          hash <- bHash <$> bsPut ipfsBlockStore bytes -- TODO Hash it ourselves

          s3Url ← do bucket ← view (configL . cBucketL)
                     pure ("s3://" <> bucket <> "/block/" <> hash)

          withTmpFile hash $ \tmpfile -> do
              writeFile (unpack tmpfile) bytes
              void $ proc "/usr/bin/aws" ["s3", "cp", tmpfile, s3Url] empty

          pure Block{bHash=hash, bData=bytes}

    , bsPin = \hash →
        withTmpFile hash $ \tmpfile -> do
            writeFile (unpack tmpfile) (encodeUtf8 hash)
            pinUrl ← do bucket <- view (configL . cBucketL)
                        pure ("s3://" <> bucket <> "/pin/" <> hash)
            void $ proc "/usr/bin/aws" ["s3", "cp", hash, pinUrl] empty

    , bsPins = do
          s3PinDir ← do bucket ← view (configL . cBucketL)
                        pure ("s3://" <> bucket <> "/pin/")
          result ← shellLines (inproc "/usr/bin/aws" ["s3", "ls", s3PinDir] empty)
          pure $ setFromList (filter looksLikeAnIpfsBlockHash result)
    }

-- IPFS Block Store ------------------------------------------------------------------------------------------

ipfsBlockStore :: BlockStore env
ipfsBlockStore = BlockStore {..}
  where
    bsGet :: MultiHash -> RIO env Block
    bsGet hash =
      withTmpFile hash $ \tmpfile -> do
          shell ("ipfs block get " <> hash <> " >" <> tmpfile) empty
          bs <- readFile (unpack tmpfile)
          pure (Block hash bs) -- TODO Verify hash

    bsPut :: ByteString -> RIO env Block
    bsPut bytes =
      withTmpFile "whateverthefuck" $ \tmpfile -> do
          writeFile (unpack tmpfile) bytes
          [hash] ← shellLines (inshell ("ipfs block put <" <> tmpfile) empty)
          pure (Block { bData=bytes, bHash=hash })

    bsPin :: MultiHash -> RIO env ()
    bsPin hash = void $ proc "ipfs" ["pin", "add", hash] empty

    bsPins :: RIO env (Set MultiHash)
    bsPins = do
        roots  ← shellLines (inproc "ipfs" ["pin", "ls", "-q", "--type=recursive"] empty)
        direct ← shellLines (inproc "ipfs" ["pin", "ls", "-q", "--type=direct"] empty)
        pure (setFromList (roots <> direct))

-- Avoid duplicating work. If we add a block twice, then we already know it's there, for example. ------------

-- TODO
memoized :: BlockStore env -> RIO env (BlockStore env)
memoized x = pure x

-- Utilities for Manual Testing ------------------------------------------------------------------------------

putGetTest :: BlockStore env -> Block -> RIO env Bool
putGetTest bs block1 = do
  block2 <- bsPut bs (bData block1)
  block3 <- bsGet bs (bHash block2)
  pure (all (block1 ==) [block2, block3])

pinTest :: BlockStore env -> MultiHash -> RIO env Bool
pinTest bs hash = do
    bsPin bs hash
    pinSet <- bsPins bs
    pure (member hash pinSet)

createFileBlock :: BlockStore env -> Text -> RIO env Block
createFileBlock bs txt = do
    writeFile ".tmptmp" (encodeUtf8 txt)
    [hash] <- shellLines (inproc "ipfs" ["add", "-Q", ".tmptmp"] empty)
    rm ".tmptmp"
    bsGet bs hash

hackyTestRoutine txt = runApp $ do
    let ipfs = ipfsBlockStore
    traceM "createFileBlock ipfs"
    blk <- createFileBlock ipfs txt
    print blk
    traceM "putGetTest ipfs"
    putGetTest ipfs blk >>= print
    traceM "pinTest ipfs"
    pinTest ipfs (bHash blk) >>= print

    let s3 = ipfsBlockStore
    traceM "createFileBlock s3"
    blk <- createFileBlock s3 txt
    print blk
    traceM "putGetTest s3"
    putGetTest s3 blk >>= print
    traceM "pinTest s3"
    pinTest s3 (bHash blk) >>= print

    pure ()

-- Application-Level Logic -----------------------------------------------------------------------------------

persistClosure block =
  do log DEBUG (Note $ "persisting the closure of: " <> block)
     persistBlock block
     s3pin block
     listBlockClosure block >>= traverse_ persistBlock

restoreClosure :: HasConfig env => Text -> RIO env ()
restoreClosure block =
  do log DEBUG (Note $ "restoring the closure of: " <> block)
     restoreBlock block
     listBlockRefs block >>= traverse_ restoreClosure

persistPinned :: HasConfig env => RIO env ()
persistPinned = ipfsPinnedBlocks >>= traverse_ persistClosure

restorePinned =
  do s3AllBlocks    >>= traverse_ restoreBlock
     s3PinnedBlocks >>= traverse_ ipfsPinBlock

--------------------------------------------------------------------------------------------------------------

runApp :: RIO Config a -> IO a
runApp action =
  do config <-
       do cPinned    <- newTVarIO Nothing
          cPersisted <- newTVarIO Nothing
          cBucket    <- pure "ipfs-archive-backups"
          pure (Config {..})
     runRIO config action

main :: IO ()
main =
  runApp $
    liftIO getArgs >>= \case
      "persist-all"   : [] → persistPinned
      "restore-all"   : [] → restorePinned
      "persist-tree"  : bs → for_ bs persistClosure
      "restore-tree"  : bs → for_ bs restoreClosure
      "persist-block" : bs → for_ bs persistClosure
      "restore-block" : bs → for_ bs restoreClosure
