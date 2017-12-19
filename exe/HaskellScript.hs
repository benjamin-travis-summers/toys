{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HaskellScript
  ( module ClassyPrelude
  , module Control.Lens
  , module Turtle
  , module Text.Printf
  , module Data.Time.Clock
  , module System.Directory
  , read, show
  , sleepUntil
  ) where

--------------------------------------------------------------------------------------------------------------

import ClassyPrelude hiding (show, tshow)
import Control.Lens hiding (Index, (<.>), snoc, (<|), index, uncons, unsnoc, cons)
import Turtle       hiding ((<>), FilePath, stderr, stdin, stdout, (<.>), (</>), fork, fold, stripPrefix,
                            find, strict, view, noneOf, has, Fold, snoc, contains, printf, sort, sortBy, sortOn)
import Data.Time.Clock
import Text.Printf

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
