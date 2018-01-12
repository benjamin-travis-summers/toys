{-# LANGUAGE OverloadedStrings #-}

import FVar

main :: IO ()
main = do
  v <- loadFVar "testvar" (0 :: Int)
  x <- readFVar v
  writeFVar v (x+1 :: Int)
  readFVar v >>= (\x -> putStrLn ("\n== " ++ show x ++ " =="))
