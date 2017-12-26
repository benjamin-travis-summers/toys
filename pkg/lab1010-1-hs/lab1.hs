#!/usr/bin/env stack
-- stack script --resolver lts-9.13

-- Computes and outputs the date for Easter for the year indicated by the
-- year parameter in MM/DD/YYYY format. All of the computation is done with
-- a formula taken, almost verbatim, from:
--
--     http://aa.usno.navy.mil/faq/docs/easter.html (near the bottom of the page).

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

--------------------------------------------------------------------------------------------------------------

import Foundation

import Text.Printf (printf)

import qualified Prelude

--------------------------------------------------------------------------------------------------------------

main = do
  putStr "Enter a year: "

  year <- Prelude.read @Int <$> Prelude.getLine

  let (/) = div

  let c = year / 100;
  let n = year - 19 * ( year / 19 );
  let k = ( c - 17 ) / 25;

  let i1 = c - c / 4 - ( c - k ) / 3 + 19 * n + 15;
  let i2 = i1 - 30 * ( i1 / 30 );
  let i3 = i2 - ( i2 / 28 ) * ( 1 - ( i2 / 28 ) * ( 29 / ( i2 + 1 ) ) * ( ( 21 - n ) / 11 ) );

  let j1 = year + year / 4 + i3 + 2 - c + c / 4;
  let j2 = j1 - 7 * ( j1 / 7 );

  let l = i3 - j2;

  let month = 3 + ( l + 40 ) / 44;
  let day = l + 28 - 31 * ( month / 4 );

  printf "%02d/%02d/%04d\n" month day year
