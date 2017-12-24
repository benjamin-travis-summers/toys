#!/usr/bin/env stack
-- stack script --resolver lts-10.0

{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE UnicodeSyntax       #-}
{-# LANGUAGE ScopedTypeVariables #-}

--------------------------------------------------------------------------------------------------------------

import ClassyPrelude
import Control.Lens
import Data.Aeson.Lens
import Data.Aeson
import Control.Monad.Trans.Resource
import Web.Authenticate.OAuth
import Network.HTTP.Client

import qualified Network.HTTP.Conduit as NetConduit
import           Web.Tumblr (tumblrOAuth)
import qualified Web.Tumblr           as Tumblr
import qualified Web.Tumblr.Types     as Tumblr

getTumblrInfo ∷ Manager → Tumblr.BaseHostname → OAuth → IO Tumblr.BlogInfo
getTumblrInfo mgr hostname creds =
  runResourceT $ runReaderT (Tumblr.tumblrInfo hostname mgr) creds

main ∷ IO ()
main = do
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

  val <- runResourceT $ runReaderT (Tumblr.tumblrInfo blogHostname mgr) oauth

  print val
