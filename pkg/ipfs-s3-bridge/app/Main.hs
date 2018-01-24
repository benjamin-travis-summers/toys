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

type BlockId = Text
type BlockSet = Set BlockId

data Config = Config { _pinnedL    :: TVar (Maybe BlockSet)
                     , _persistedL :: TVar (Maybe BlockSet)
                     , _bucketL    :: !Text
                     }

makeLenses ''Config

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
ipfsPinnedBlocks = initialize (configL . pinnedL) listPins
                     where listPins =
                             do log DEBUG (Note "Listing Pinned Blocks")
                                roots  ← shellLines (inproc "ipfs" ["pin", "ls", "-q", "--type=recursive"] empty)
                                direct ← shellLines (inproc "ipfs" ["pin", "ls", "-q", "--type=direct"] empty)
                                let blockSet = setFromList (roots <> direct)
                                log DEBUG (IpfsPinnedBlocks blockSet)
                                pure (setFromList (roots <> direct))

-- | All the pinned roots on the local IPFS node.
ipfsAllBlocks ∷ HasConfig env => RIO env BlockSet
ipfsAllBlocks = initialize (configL . pinnedL) listPins
                  where listPins =
                          do log DEBUG (Note "Listing all blocks on IPFS.")
                             blocks ← shellLines (inproc "ipfs" ["pin", "ls", "-q", "--type=all"] empty)
                             log DEBUG (Note "Finished")
                             pure (setFromList blocks)

looksLikeAnIpfsBlockHash :: Text -> Bool
looksLikeAnIpfsBlockHash t = Text.take 2 t == "Qm"

-- | All the persisted roots on s3.
s3PinnedBlocks ∷ HasConfig env => RIO env BlockSet
s3PinnedBlocks = initialize (configL . persistedL) listPins
                   where listPins =
                           do log DEBUG (Note "Listing all blocks on S3.")
                              bucket ← view (configL . bucketL)
                              let s3PinDir = "s3://" <> bucket <> "/pin/"
                              result ← shellLines (inproc "/usr/bin/aws" ["s3", "ls", s3PinDir] empty)
                              log DEBUG (Note "Finished")
                              pure $ setFromList (filter looksLikeAnIpfsBlockHash result)

-- | All the persisted roots on s3.
s3AllBlocks ∷ HasConfig env => RIO env BlockSet
s3AllBlocks = initialize (configL . persistedL) listPins
                where listPins =
                        do bucket ← view (configL . bucketL)
                           let s3BlockDir = ("s3://" <> bucket <> "/block/") :: Text
                           log DEBUG (Note "Listing all blocks on S3.")
                           log DEBUG (Note $ unwords ["/usr/bin/aws", "s3", "ls", s3BlockDir])
                           result ← shellLines (inproc "/usr/bin/aws" ["s3", "ls", s3BlockDir] empty)
                           log DEBUG (Note "Finished")
                           pure $ setFromList (filter looksLikeAnIpfsBlockHash result)

--------------------------------------------------------------------------------------------------------------

blockPinnedIpfs, blockOnIpfs, blockPinnedS3, blockOnS3 :: HasConfig env => BlockId -> RIO env Bool

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
         do bucket ← view (configL . bucketL)
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
         do bucket ← view (configL . bucketL)
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
       do bucket ← view (configL . bucketL)
          let pinUrl = "s3://" <> bucket <> "/pin/" <> block
          writeFile (unpack block) (encodeUtf8 block)
          proc "/usr/bin/aws" ["s3", "cp", block, pinUrl] empty
          rm (fromString $ unpack block)

ipfsPinBlock :: HasConfig env => Text -> RIO env ()
ipfsPinBlock block =
  do log DEBUG (Note $ "pin on ipfs: " <> block)
     unlessM (blockPinnedIpfs block) $
       void $ proc "ipfs" ["pin", "add", block] empty

--------------------------------------------------------------------------------------------------------------

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

main :: IO ()
main =
  do config <-
       do _pinnedL    <- newTVarIO Nothing
          _persistedL <- newTVarIO Nothing
          _bucketL    <- pure "ipfs-archive-backups"
          pure (Config {..})

     runRIO config $
       liftIO getArgs >>= \case
         "persist-all"   : [] → persistPinned
         "restore-all"   : [] → restorePinned
         "persist-tree"  : bs → for_ bs persistClosure
         "restore-tree"  : bs → for_ bs restoreClosure
         "persist-block" : bs → for_ bs persistClosure
         "restore-block" : bs → for_ bs restoreClosure
