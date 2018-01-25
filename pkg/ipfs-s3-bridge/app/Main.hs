{-# LANGUAGE StrictData #-}

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

data BlockStore e = BlockStore { bsGet    :: MultiHash -> RIO e Block
                               , bsPut    :: Block     -> RIO e Block
                               , bsPin    :: MultiHash  -> RIO e ()
                               , bsPins   :: RIO e (Set MultiHash)
                               , bsBlocks :: RIO e (Set MultiHash)
                               }

makeFields ''Block
makeFields ''BlockStore

-- Logging ---------------------------------------------------------------------------------------------------

data LogLevel = TRACE | DEBUG
  deriving (Show)

data Log = Note Text
         | IpfsPinnedBlocks BlockSet
         | S3PinnedBlocks BlockSet
  deriving (Show)

newtype Logger = Logger (LogLevel -> Log -> IO ())

class HasLogger e where
  loggerL :: Lens' e Logger

logIO :: Logger -> LogLevel -> Log -> IO ()
logIO (Logger logger) lvl log = logger lvl log

log :: HasLogger e => LogLevel -> Log -> RIO e ()
log lvl log = do logger <- view loggerL
                 liftIO (logIO logger lvl log)

logNote :: HasLogger e => LogLevel -> Text -> RIO e ()
logNote lvl note = log lvl (Note note)

traceNote :: HasLogger e => Text -> RIO e ()
traceNote = logNote TRACE

simpleLogger :: Logger
simpleLogger = Logger (\lvl log → putStrLn (tshow lvl <> " " <> fmtLog log))
                 where fmtLog :: Log -> Text
                       fmtLog (Note t) = t
                       fmtLog l        = tshow l

-- Config ----------------------------------------------------------------------------------------------------

data Config = Config { cBucket :: Text }

makeFields ''Config

class HasConfig e where
  configL :: Lens' e Config

instance HasConfig Config where
  configL = id

-- Env -------------------------------------------------------------------------------------------------------

data Env = Env { eLogger :: Logger
               , eConfig :: Config
               , eS3     :: BlockStore Env
               , eIpfs   :: BlockStore Env
               }

makeFields ''Env

class (HasLogger e, HasConfig e) => HasEnv e where
  envL :: Lens' e Env

instance HasEnv Env where
  envL = id

instance HasLogger Env where
  loggerL = lens eLogger (\x y -> x { eLogger = y })

instance HasConfig Env where
  configL = lens eConfig (\x y -> x { eConfig = y })

--instance (HasLogger e, HasConfig e) => HasEnv e where
-- envL :: Lens' e Env

-- Utilities -------------------------------------------------------------------------------------------------

withTmpFile :: Text -> (Text -> RIO e a) -> RIO e a
withTmpFile block action = bracket (pure (block <> ".block")) (rm . fromString . unpack) action

shellLines :: MonadIO m => Shell Line -> m [Text]
shellLines sh = Sh.fold (fmap lineToText sh) Fold.list

-- IPFS Utilities --------------------------------------------------------------------------------------------

-- TODO Assumes this is already available in the local IPFS store.
blockClosure :: MonadIO m => MultiHash -> m [MultiHash]
blockClosure hash = shellLines (inproc "ipfs" ["refs", "-r", "-u", hash] empty)

-- TODO Assumes this is already available in the local IPFS store.
blockRefs :: MonadIO m => MultiHash -> m [MultiHash]
blockRefs hash = shellLines (inproc "ipfs" ["refs", "-u", hash] empty)

-- S3 Block Store --------------------------------------------------------------------------------------------

s3BlockStore :: forall e. (HasLogger e, HasConfig e) => BlockStore e
s3BlockStore =
  let looksLikeAnIpfsBlockHash :: Text -> Bool
      looksLikeAnIpfsBlockHash t = Text.take 2 t == "Qm"
  in BlockStore
    { bsGet = \hash → do
         do s3Url ← do bucket ← view (configL . cBucketL)
                       pure ("s3://" <> bucket <> "/block/" <> hash)
            withTmpFile hash $ \tmpfile -> do
                void $ proc "/usr/bin/aws" ["s3", "cp", s3Url, tmpfile] empty
                bytes ← readFile (unpack tmpfile)
                pure Block{bData=bytes, bHash=hash}

    , bsPut = \Block{bHash,bData} → do
          s3Url ← do bucket ← view (configL . cBucketL)
                     pure ("s3://" <> bucket <> "/block/" <> bHash)

          withTmpFile bHash $ \tmpfile -> do
              writeFile (unpack tmpfile) bData
              void $ proc "/usr/bin/aws" ["s3", "cp", tmpfile, s3Url] empty

          pure Block{bHash=bHash, bData=bData}

    , bsPin = \hash →
        withTmpFile hash $ \tmpfile -> do
            writeFile (unpack tmpfile) (encodeUtf8 hash)
            pinUrl ← do bucket <- view (configL . cBucketL)
                        pure ("s3://" <> bucket <> "/pin/" <> hash)
            void $ proc "/usr/bin/aws" ["s3", "cp", tmpfile, pinUrl] empty

    , bsBlocks = do
          s3BlockDir ← do bucket ← view (configL . cBucketL)
                          pure ("s3://" <> bucket <> "/block/")
          result ← shellLines ( empty
                              & inproc "/usr/bin/aws" ["s3", "ls", s3BlockDir]
                              & inshell "sed 's/^[^Q]*Qm/Qm/'"
                              )
          pure $ setFromList (filter looksLikeAnIpfsBlockHash result)

    , bsPins = do
          s3PinDir ← do bucket ← view (configL . cBucketL)
                        pure ("s3://" <> bucket <> "/pin/")
          result ← shellLines ( empty
                              & inproc "/usr/bin/aws" ["s3", "ls", s3PinDir]
                              & inshell "sed 's/^[^Q]*Qm/Qm/'"
                              )
          pure $ setFromList (filter looksLikeAnIpfsBlockHash result)
    }

-- IPFS Block Store ------------------------------------------------------------------------------------------

ipfsBlockStore :: BlockStore e
ipfsBlockStore = BlockStore {..}
  where
    bsGet :: MultiHash -> RIO e Block
    bsGet hash =
      withTmpFile hash $ \tmpfile ->
        do shell ("ipfs block get " <> hash <> " >" <> tmpfile) empty
           bs <- readFile (unpack tmpfile)
           pure (Block hash bs) -- TODO Verify hash

    bsPut :: Block -> RIO e Block
    bsPut blk =
      withTmpFile (bHash blk) $ \tmpfile ->
        do writeFile (unpack tmpfile) (bData blk)
           [hash] ← shellLines (inshell ("ipfs block put <" <> tmpfile) empty)
           unless (hash == bHash blk) $
             error $ unpack ("BLOCK HASH WAS WROOOOONG!\n" <>
                             "We were expecting '" <> bHash blk <>
                             "', but got got '" <> hash <> "' instead")
           pure (Block { bData=bData blk, bHash=hash })

    bsPin :: MultiHash -> RIO e ()
    bsPin hash = void $ proc "ipfs" ["pin", "add", hash] empty

    bsPins :: RIO e (Set MultiHash)
    bsPins =
      do roots  ← shellLines (inproc "ipfs" ["pin", "ls", "-q", "--type=recursive"] empty)
         direct ← shellLines (inproc "ipfs" ["pin", "ls", "-q", "--type=direct"] empty)
         pure (setFromList (roots <> direct))

    bsBlocks :: RIO e (Set MultiHash)
    bsBlocks =
      do setFromList <$> shellLines (inproc "ipfs" ["pin", "ls", "-q", "--type=all"] empty)

-- Trace the execution through a BlockStore ------------------------------------------------------------------

tracedBS ∷ HasLogger e => Text → BlockStore e → BlockStore e
tracedBS nm bs =
  BlockStore
    { bsGet    = \hash  → traceNote (nm <> ".bsGet(" <> hash <> ")") >> bsGet bs hash
    , bsPut    = \blk   → traceNote (nm <> ".bsPut(...)")            >> bsPut bs blk
    , bsPin    = \hash  → traceNote (nm <> ".bsPin(" <> hash <> ")") >> bsPin bs hash
    , bsPins   =          traceNote (nm <> ".bsPins()")              >> bsPins bs
    , bsBlocks =          traceNote (nm <> ".bsBlocks()")            >> bsBlocks bs
    }

-- Don't create blocks or pins that already exists. ----------------------------------------------------------

cacheAction :: MonadIO m => IORef (Maybe a) -> m a -> m a
cacheAction var action = liftIO (readIORef var) >>= \case
                     Just val → pure val
                     Nothing  → do val ← action
                                   liftIO (writeIORef var (Just val))
                                   pure val

cacheBS ∷ ∀e. BlockStore e → IO (BlockStore e)
cacheBS bs = do vAllPins        ← newIORef (Nothing ∷ Maybe BlockSet)
                vAllBlocks      ← newIORef (Nothing ∷ Maybe BlockSet)
                vUploadedBlocks ← newIORef (mempty ∷ BlockSet)
                vPinnedBlocks   ← newIORef (mempty ∷ BlockSet)

                let listPins = cacheAction vAllPins (bsPins bs)

                    listBlocks :: RIO e BlockSet
                    listBlocks = cacheAction vAllBlocks (bsBlocks bs)

                    pinExists :: MultiHash → RIO e Bool
                    pinExists hash = (member hash <$> liftIO (readIORef vPinnedBlocks)) >>= \case
                                       True  → pure True
                                       False → member hash <$> listPins

                    blockExists :: MultiHash → RIO e Bool
                    blockExists hash = (member hash <$> liftIO (readIORef vUploadedBlocks)) >>= \case
                                         True  → pure True
                                         False → member hash <$> listBlocks

                    pinBlock hash = unlessM (pinExists hash) $
                                      do bsPin bs hash
                                         liftIO $ modifyIORef vPinnedBlocks (insertSet hash)

                    putBlock blk =
                      blockExists (bHash blk) >>=
                        \case True  → pure blk
                              False → do newBlk ← bsPut bs blk
                                         liftIO $ modifyIORef vUploadedBlocks (insertSet (bHash blk))
                                         pure newBlk

                pure $ BlockStore
                         { bsGet    = \hash → bsGet bs hash
                         , bsPut    = putBlock
                         , bsPin    = pinBlock
                         , bsPins   = listPins
                         , bsBlocks = listBlocks
                         }

-- Utilities for Manual Testing ------------------------------------------------------------------------------

runApp :: RIO Env a -> IO a
runApp action =
  do env <- do cBucket <- pure "ipfs-archive-backups"
               eConfig <- pure Config{..}
               eLogger <- pure simpleLogger
               eIpfs   <- tracedBS "ipfs" <$> cacheBS (tracedBS "[ipfs]" ipfsBlockStore)
               eS3     <- tracedBS "s3"   <$> cacheBS (tracedBS "[s3]"   s3BlockStore)
               pure Env{..}
     runRIO env action

putGetTest :: BlockStore e -> Block -> RIO e Bool
putGetTest bs block1 = do
  block2 <- bsPut bs block1
  block3 <- bsGet bs (bHash block2)
  pure (all (block1 ==) [block2, block3])

pinTest :: BlockStore e -> MultiHash -> RIO e Bool
pinTest bs hash = do
    bsPin bs hash
    pinSet <- bsPins bs
    pure (member hash pinSet)

createFileBlock :: HasLogger e => BlockStore e -> Text -> RIO e Block
createFileBlock bs txt = do
    writeFile ".tmptmp" (encodeUtf8 txt)
    [hash] <- shellLines (inproc "ipfs" ["add", "-Q", ".tmptmp"] empty)
    rm ".tmptmp"
    let ipfs = tracedBS "ipfs" ipfsBlockStore
    newBlock <- bsGet ipfs hash
    bsPut bs newBlock

hackyTestRoutine txt =
  do ipfs ← view (envL.eIpfsL)
     traceNote "createFileBlock ipfs"
     blk <- createFileBlock ipfs txt
     traceNote (tshow blk)
     traceNote "putGetTest ipfs"
     putGetTest ipfs blk >>= traceNote.tshow
     traceNote "pinTest ipfs"
     pinTest ipfs (bHash blk) >>= traceNote.tshow

     void (bsPins ipfs)
     void (bsBlocks ipfs)

     s3 ← view (envL.eS3L)
     traceNote "createFileBlock s3"
     blk <- createFileBlock s3 txt
     traceNote (tshow blk)
     traceNote "putGetTest s3"
     putGetTest s3 blk >>= traceNote.tshow
     traceNote "pinTest s3"
     pinTest s3 (bHash blk) >>= traceNote.tshow

     void (bsPins s3)
     void (bsBlocks s3)

     pure ()

-- Application-Level Logic -----------------------------------------------------------------------------------

hasPin bs b   = member b <$> bsPins bs
hasBlock bs b = member b <$> bsBlocks bs

copyBlock :: BlockStore Env -> BlockStore Env -> MultiHash -> RIO Env ()
copyBlock from to hash = void (bsGet from hash >>= bsPut to)

persistBlock, restoreBlock :: MultiHash -> RIO Env ()
persistBlock hash = do s3   ← view (envL . eS3L)
                       ipfs ← view (envL . eIpfsL)
                       copyBlock ipfs s3 hash

restoreBlock hash = do s3   ← view (envL . eS3L)
                       ipfs ← view (envL . eIpfsL)
                       copyBlock s3 ipfs hash

persistClosure :: Text -> RIO Env ()
persistClosure block =
  do traceNote ("copy block from ipfs to s3: " <> block)
     persistBlock block
     s3 ← view (envL . eS3L)
     bsPin s3 block
     blockClosure block >>= traverse_ persistBlock

restoreClosure :: Text -> RIO Env ()
restoreClosure block =
  do traceNote ("copy block from s3 to ipfs: " <> block)
     restoreBlock block
     blockRefs block >>= traverse_ restoreClosure

persistPinned :: RIO Env ()
persistPinned = do ipfs ← view (envL . eIpfsL)
                   bsPins ipfs >>= traverse_ persistClosure

restorePinned :: RIO Env ()
restorePinned = do s3   ← view (envL . eS3L)
                   ipfs ← view (envL . eIpfsL)
                   bsBlocks s3 >>= traverse_ restoreBlock
                   bsPins s3   >>= traverse_ (bsPin ipfs)

--------------------------------------------------------------------------------------------------------------

main :: IO ()
main =
  runApp $
    liftIO getArgs >>= \case
      ["persist-all"]      → persistPinned
      ["restore-all"]      → restorePinned
      ["test", withStr]    → hackyTestRoutine withStr
      "persist-tree"  : bs → for_ bs persistClosure
      "restore-tree"  : bs → for_ bs restoreClosure
      "persist-block" : bs → for_ bs persistClosure
      "restore-block" : bs → for_ bs restoreClosure
