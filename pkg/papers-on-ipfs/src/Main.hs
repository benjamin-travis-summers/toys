import ClassyPrelude hiding (head, fromList)
import Control.Lens
import Web.Scotty
import Data.List.NonEmpty
import FVar
import Data.Binary.Orphans ()

{-
  Three operations
    ✓ Browse the current collection on ipfs.io
    - Submit a new IPFS hash to merge with.
    - Browse the revision history.

  TODO
    ✓ Persist the new hashes (using FVar).
    - Allow users to push new hashes.
    - Instead of replacing the existing hash, merge the two directories.
-}

type RevHistory = NonEmpty Text
type RevDb      = FVar RevHistory

newRevDb :: Text -> IO RevDb
newRevDb hash = loadFVar "papers-archive" (fromList [hash])

latestRev :: RevDb -> IO Text
latestRev ref = head <$> readFVar ref

main = do
  hashVar <- newRevDb "QmPnbgmwGrxEx1B9CpStJAsxTex4QZGoCpC7VbJtoFHmeb"

  scotty 3000 $ do
    get (regex "^/ipfs/.*$") $ do
      path <- param "0"
      redirect ("https://ipfs.io" <> path)

    get "/" $ do
      hash <- fromStrict <$> liftIO (latestRev hashVar)
      let collection = "<span>view collection <a href=\"/ipfs/"<> hash <> "\">here</a></span>"
      let submission = "Add to collection: <input type=\"text\" style=\"width: 333px\" placeholder=\""
                    <> hash
                    <> "\"></input>"
      html ("<body>" ++ collection ++ "<br />" ++ submission ++ "</body>")

    post "^/new" $ do
      html "TODO"
