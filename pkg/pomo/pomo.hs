#!/usr/bin/env stack
-- stack script --resolver lts-9.13

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE UnicodeSyntax     #-}
{-# LANGUAGE FlexibleContexts  #-}

--------------------------------------------------------------------------------------------------------------

import ClassyPrelude
import Control.Lens
import Data.Tree as RoseTree
import Text.Printf
import Data.Time.Clock

minutesToDiffTime m = secondsToDiffTime (60 * m)

diffTimeToMicroseconds t = diffTimeToPicoseconds t `div` 1000000

sleepUntil :: UTCTime -> IO ()
sleepUntil wakeUpAt = do
  now <- getCurrentTime

  let nominalDiff = wakeUpAt `diffUTCTime` now
  let diff        = fromRational @DiffTime (toRational nominalDiff)

  when (diff >= 0) $ do
    let us = fromIntegral (diffTimeToMicroseconds diff)
    putStrLn ("sleeping for " <> tshow us)
    threadDelay us

diffTimeToNominalDiffTime :: DiffTime -> NominalDiffTime
diffTimeToNominalDiffTime = fromRational . toRational

main ∷ IO ()
main = do
  startTime <- getCurrentTime

  let endTime = addUTCTime (diffTimeToNominalDiffTime (minutesToDiffTime 50)) startTime

  putStrLn (tshow startTime <> " -> " <> tshow endTime)

  let loop now = do
        let nextTick = addUTCTime (diffTimeToNominalDiffTime (secondsToDiffTime 1)) now

        putStrLn (tshow now <> " -> " <> tshow nextTick)

        let timeLeft = endTime `diffUTCTime` now :: NominalDiffTime
        let secsLeft = truncate timeLeft :: Integer
        let secs = secsLeft `mod` 60
            mins = secsLeft `div` 60

        writeFileUtf8 "/home/benjamin/POMODORO" (pack (printf "%02d:%02d" mins secs))
        sleepUntil nextTick
        loop nextTick

  loop startTime
