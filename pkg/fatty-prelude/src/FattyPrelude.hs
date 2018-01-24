module FattyPrelude
    ( module ClassyPrelude.Conduit
    , module Turtle
    , module Control.Lens
    , module Control.Lens.TH
    , module RIO
    ) where

import ClassyPrelude.Conduit
  hiding (log, bracket, FilePath, (<.>), (</>))

import Turtle
  hiding (view, find, stripPrefix, sortOn, (<>), sort, sortBy, stderr, stdin, stdout,
          fork, fold, Fold, contains, has, noneOf, strict)

import Control.Lens
  hiding (Index, (<.>), (<|), index, uncons, unsnoc, cons, snoc)

import Control.Lens.TH

import RIO (RIO, runRIO, bracket)
