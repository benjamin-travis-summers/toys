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
import qualified Web.Tumblr           as Tumblr
import qualified Web.Tumblr.Types     as Tumblr

getTumblrInfo ∷ Manager → Tumblr.BaseHostname → OAuth → IO Tumblr.BlogInfo
getTumblrInfo mgr hostname creds =
  runResourceT $ runReaderT (Tumblr.tumblrInfo hostname mgr) creds

tumblrOAuth :: ByteString -- ^ The Tumblr API key
            -> ByteString -- ^ The Tumblr API secret to use
            -> OAuth
tumblrOAuth key secret = newOAuth { oauthServerName = "tumblr"
                                  , oauthRequestUri = "http://www.tumblr.com/oauth/request_token"
                                  , oauthAccessTokenUri = "http://www.tumblr.com/oauth/access_token"
                                  , oauthAuthorizeUri = "http://www.tumblr.com/oauth/authorize"
                                  , oauthConsumerKey = key
                                  , oauthConsumerSecret = secret }

main ∷ IO ()
main = do
  f ← readFileUtf8 "/home/benjamin/secrets/tumblr.json"

  Just (secrets        ∷ Value)      ← pure (f ^? _JSON)

  Just (tokenKey       ∷ ByteString) ← pure $ secrets ^? key "token"           . _String . to encodeUtf8
  Just (tokenSecret    ∷ ByteString) ← pure $ secrets ^? key "token_secret"    . _String . to encodeUtf8
  Just (consumerKey    ∷ ByteString) ← pure $ secrets ^? key "consumer_key"    . _String . to encodeUtf8
  Just (consumerSecret ∷ ByteString) ← pure $ secrets ^? key "consumer_secret" . _String . to encodeUtf8

  mgr ← NetConduit.newManager defaultManagerSettings

  let cred = newCredential tokenKey tokenSecret

  let oauth = tumblrOAuth consumerKey consumerSecret

  val ← getTumblrInfo mgr "story-of-my-food.tumblr.com" oauth

  print val
