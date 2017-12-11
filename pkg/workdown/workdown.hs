#!/usr/bin/env stack
-- stack script --resolver lts-9.13

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

--------------------------------------------------------------------------------------------------------------

import ClassyPrelude
import Control.Lens
import Data.Tree as RoseTree

-- Types -----------------------------------------------------------------------------------------------------

type Workflowy = RoseTree.Forest (Text, Maybe Text)

--------------------------------------------------------------------------------------------------------------

parse :: Text -> Either Text Workflowy
parse = undefined

main :: IO ()
main = pure ()
