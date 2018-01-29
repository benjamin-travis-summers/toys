{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HaskellScript
  ( module ClassyPrelude
  , module Control.DeepSeq
  , module Control.Exception
  , module Control.Exception.Lens
  , module Control.Lens
  , module Data.Time.Clock
  , module System.Directory
  , module System.IO.Error.Lens
  , module Text.Printf
  , module Turtle
  , read, show
  , sleepUntil
  ) where

--------------------------------------------------------------------------------------------------------------

import ClassyPrelude
  hiding (show, tshow, handle)
import Control.Lens
  hiding (Index, (<.>), snoc, (<|), index, uncons, unsnoc, cons)
import Turtle
  hiding ((<>), FilePath, stderr, stdin, stdout, (<.>), (</>), fork, fold, stripPrefix, find, strict, view,
          noneOf, has, Fold, snoc, contains, printf, sort, sortBy, sortOn, Parser)
import System.Directory
  hiding (readable, writable, executable, isSymbolicLink, Permissions)
import Data.Time.Clock
import Text.Printf
import Data.Time.Clock
import Control.Exception.Lens
import System.IO.Error.Lens
import Control.DeepSeq
import Control.Exception (evaluate)

import System.Directory (getHomeDirectory)

import qualified ClassyPrelude
import qualified Control.Lens as Lens
import qualified Prelude
import qualified Turtle

-- read/show without String. ---------------------------------------------------------------------------------

read ∷ Read a ⇒ Text → a
read str = Prelude.read (unpack str)

show ∷ Show a ⇒ a -> Text
show = ClassyPrelude.tshow

-- Utilities for Working with Time ---------------------------------------------------------------------------

sleepUntil ∷ UTCTime -> IO ()
sleepUntil wakeUpTime = do
  now <- getCurrentTime

  let diff = wakeUpTime `diffUTCTime` now

  when (diff >= 0) $ do
    let microsecs = truncate (diff * 1000000)
    threadDelay (fromIntegral microsecs)
