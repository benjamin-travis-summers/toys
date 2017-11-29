#!/usr/bin/env stack
-- stack script --resolver nightly-2017-07-31

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE MultiWayIf                #-}
{-# LANGUAGE TypeSynonymInstances      #-}

import Data.Foldable
import Data.Function
import Data.List
import Data.Monoid
import Data.String.Utils        (strip)
import Data.Traversable
import System.Console.Haskeline
import System.Exit
import System.IO
import System.Random.Shuffle    (shuffleM)
import Text.Tabular
import Text.Tabular.AsciiArt    (render)

-- Basic Vowel Harmoney and Consonant Softening Logic --------------------------------------------------------

harmonizeA vowel | vowel `elem` "aıou" = 'a'
                 | vowel `elem` "eiöü" = 'e'

harmonizeU vowel | vowel `elem` "aı" = 'ı'
                 | vowel `elem` "ei" = 'i'
                 | vowel `elem` "ou" = 'u'
                 | vowel `elem` "öü" = 'ü'

isVowel c = c `elem` "aeiıoöuü"

isHardConsonant c = c `elem` "fstkhpçş"

lastVowel kök = last (filter isVowel kök)

harmonize 'a' kök = harmonizeA (lastVowel kök)
harmonize 'u' kök = harmonizeU (lastVowel kök)

soften 'k' = 'ğ'
soften 't' = 'd'
soften 'p' = 'b'
soften x   = x

harden 'd' = 't'
harden x   = x

-- Add Suffixes using Suffix Templates -----------------------------------------------------------------------

ekiEkle :: String -> String -> String
ekiEkle ek kök =
    kökHead <> [softTail] <> finalEk
  where
    kökHead  = init kök
    kökTail  = last kök
    softTail = kökTail & (if head ek == 'y' then soften else id)
    hardEk   = ek      & mapHead (if isHardConsonant kökTail then harden else id)
    finalEk  = concatMap adaptEkChar hardEk

    adaptEkChar 'y' | isVowel kökTail = ['y']
    adaptEkChar 'y'                   = []
    adaptEkChar 'a'                   = [harmonize 'a' kök]
    adaptEkChar 'u'                   = [harmonize 'u' kök]
    adaptEkChar c                     = [c]

    mapHead f []     = []
    mapHead f (x:xs) = f x : xs

--------------------------------------------------------------------------------------------------------------

data Time = Time Int Int

data TimeQuestion = TimeQuestion String (Time -> String)

data Ek = Ek String (String -> String)

data Question =
  forall a b.
    (Disp a, Disp b) =>
    Question a b (a -> b -> String)

instance Disp Time where
  disp (Time 0 m) = show 12 <> ":" <> show m
  disp (Time h m) = show h <> ":" <> show m

instance Disp TimeQuestion where disp (TimeQuestion nm _) = nm

instance Disp Ek where disp (Ek nm _) = nm

tqFn (TimeQuestion _ f) = f

ekFn (Ek _ f) = f

--------------------------------------------------------------------------------------------------------------

lokatif  = Ek "lokatif (at)"   (ekiEkle "da")
datif    = Ek "datif (to)"     (ekiEkle "ya")
akuzatif = Ek "akuzatif (obj)" (ekiEkle "yu")
ablatif  = Ek "ablatif (from)" (ekiEkle "dan")
ile      = Ek "ile (with)"     (ekiEkle "yla")

ekler = [datif, akuzatif, lokatif, ablatif, ile]

--------------------------------------------------------------------------------------------------------------

haneler = ["sıfır", "bir", "iki", "üç", "dört", "beş", "altı", "yedi", "sekiz", "dokuz"]

showDigit d = haneler !! d

showHour n = hours !! (n `mod` 12)
  where
    hours = ["on iki"] <> drop 1 haneler <> ["on", "on bir"]

tensPlace = ["sıfır", "on", "yirmi", "otuz", "kırk", "elli", "altmuş", "yetmiş", "seksen", "doksan"]

showTensPlace t = tensPlace !! t

showNum m =
  case (m `div` 10, m `mod` 10) of
    (0, d) -> showDigit d
    (t, 0) -> showTensPlace t
    (t, d) -> unwords [showTensPlace t, showDigit d]

saatKaç = TimeQuestion "saat kaç" $ \case
  Time h 0          -> unwords ["saat", showHour h]
  Time h 15         -> unwords ["saat", ekFn akuzatif (showHour h), "çeyrek", "geçiyor"]
  Time h 30         -> unwords ["saat", showHour h, "buçuk"]
  Time h 45         -> unwords ["saat", ekFn datif (showHour (h+1)), "çeyrek", "var"]
  Time h m | m < 30 -> unwords ["saat", ekFn akuzatif (showHour h), showNum m, "geçiyor"]
  Time h m | m > 30 -> unwords ["saat", ekFn datif (showHour (h+1)), showNum (60-m), "var"]

saatKaçta = TimeQuestion "saat kaçta" $ \case
  Time h 0          -> unwords ["saat", ekFn lokatif (showHour h)]
  Time h 15         -> unwords ["saat", ekFn akuzatif (showHour h), "çeyrek", "geçe"]
  Time h 30         -> unwords ["saat", showHour h, ekFn lokatif "buçuk"]
  Time h 45         -> unwords ["saat", ekFn datif (showHour (h+1)), "çeyrek", "kala"]
  Time h m | m < 30 -> unwords ["saat", ekFn akuzatif (showHour h), showNum m, "geçe"]
  Time h m | m > 30 -> unwords ["saat", ekFn datif (showHour (h+1)), showNum (60-m), "kala"]

timeQuestions = [saatKaç, saatKaçta]

--------------------------------------------------------------------------------------------------------------

class Disp a where
  disp :: a -> String

instance Disp String where
  disp s = s

--------------------------------------------------------------------------------------------------------------

quickTable :: (Disp r, Disp c) => (r -> c -> String) -> [r] -> [c] -> IO ()
quickTable mix rows cols =
  putStrLn $ render id id id $
    Table (Group NoLine     (Header . disp <$> cols))
          (Group SingleLine (Header . disp <$> rows))
          [ [mix r c | r <- rows] | c <- cols ]

--------------------------------------------------------------------------------------------------------------

quiz :: Question -> InputT IO Bool
quiz question@(Question q x mix) = do
  let rightAns = mix q x
      prompt   = mconcat [disp x, " + ", disp q, "?"]

  outputStrLn prompt

  getInputLine "> " >>= \case
    Nothing  -> pure False
    Just ""  -> quiz question
    Just "?" -> do outputStrLn ("It was \"" <> rightAns <> "\"\n")
                   pure True
    Just ln  -> if strip ln == rightAns
                  then do outputStrLn "\nRIGHT!\n"
                          pure True
                  else do outputStrLn "\nWRONG!\n"
                          quiz question

traverseWhile_ :: Monad m => (a -> m Bool) -> [a] -> m ()
traverseWhile_ f []     = pure ()
traverseWhile_ f (x:xs) = do
  shouldContinue <- f x
  if shouldContinue
    then traverseWhile_ f xs
    else pure ()

main = do
  hours      <- take 5 <$> shuffleM [0..23]
  normalMins <- take 2 <$> shuffleM [0, 15, 30, 45]
  randMins   <- take 4 <$> shuffleM [0..59]
  minutes    <- take 4 <$> shuffleM (nub (normalMins <> randMins))

  let someTimes = [ Time h m           | h <- hours,          m <- minutes   ]
      ekQs      = [ Question ek s ekFn | ek <- ekler,         s <- haneler   ]
      timeQs    = [ Question tq t tqFn | tq <- timeQuestions, t <- someTimes ]

  quickTable ekFn ekler         haneler
  quickTable tqFn timeQuestions someTimes

  qs <- shuffleM (ekQs <> timeQs)
  runInputT defaultSettings (traverseWhile_ quiz qs)
