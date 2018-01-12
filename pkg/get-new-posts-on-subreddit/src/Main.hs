import ClassyPrelude
import Control.Lens
import Data.Aeson.Lens
import Data.Aeson
import Control.Monad.Trans.Resource
import Web.Authenticate.OAuth
import Network.HTTP.Client
import qualified Network.Wreq as Wreq

import qualified Network.HTTP.Conduit as NetConduit
import qualified Web.Tumblr           as Tumblr
import qualified Web.Tumblr.Types     as Tumblr

import Web.Tumblr (tumblrOAuth)
import Network.URL (exportURL, importURL)

import qualified Reddit                 as Reddit
import qualified Reddit.Actions.Search  as Reddit
import qualified Reddit.Types.Subreddit as Reddit
import qualified Reddit.Types.Options   as Reddit
import qualified Reddit.Types.Listing   as Reddit
import qualified Reddit.Types.Post      as RedditPost

--------------------------------------------------------------------------------------------------------------

getTumblrInfo ∷ Manager → Tumblr.BaseHostname → OAuth → IO Tumblr.BlogInfo
getTumblrInfo mgr hostname creds =
  runResourceT $ runReaderT (Tumblr.tumblrInfo hostname mgr) creds

tumblrMain ∷ IO ()
tumblrMain = do
  f ← readFileUtf8 "/home/benjamin/secrets/tumblr.json"

  let (Just (secrets∷Value)) = f ^? _JSON

  let (Just tokenKey)       = map encodeUtf8 (secrets ^? key "token"           . _String)
  let (Just tokenSecret)    = map encodeUtf8 (secrets ^? key "token_secret"    . _String)
  let (Just consumerKey)    = map encodeUtf8 (secrets ^? key "consumer_key"    . _String)
  let (Just consumerSecret) = map encodeUtf8 (secrets ^? key "consumer_secret" . _String)

  mgr ← newManager defaultManagerSettings

  let cred         = newCredential tokenKey tokenSecret
  let oauth        = tumblrOAuth consumerKey consumerSecret
  let blogHostname = "story-of-my-food.tumblr.com"

  let photoHash       = "QmPnbgmwGrxEx1B9CpStJAsxTex4QZGoCpC7VbJtoFHmeb"
  let (Just photoUrl) = importURL ("https://ipfs.io/ipfs/" <> photoHash)

  let url = "https://api.tumblr.com/v2/blog/" <> blogHostname <> "/post"

  let opts = Wreq.defaults & Wreq.param "type"    .~ ["photo"]
                           & Wreq.param "source"  .~ [pack (exportURL photoUrl)]
                           & Wreq.param "api_key" .~ [decodeUtf8 consumerKey]

  print opts
  print url

  resp <- Wreq.postWith opts url ("" :: ByteString)
  let body = resp ^. Wreq.responseBody
  print body

main ∷ IO ()
main = do
  r <- liftIO (Reddit.runRedditAnon getHaskellPosts)
  case r of
    Left err -> print err
    Right listing ->
      for_ (Reddit.contents listing) $ \post → do
        let (RedditPost.PostID id) = RedditPost.postID post
        let title                  = RedditPost.title post
        putStrLn ("[" <> id <> "]: " <> title)

getHaskellPosts ∷ Reddit.RedditT IO Reddit.PostListing
getHaskellPosts = do
  let haskell = Reddit.R "haskell"
  let opts    = Reddit.Options Nothing (Just 50)
  Reddit.getPosts' opts Reddit.New (Just haskell)
