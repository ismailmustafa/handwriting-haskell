{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network.Handwriting

import System.Environment (getEnv)

main :: IO ()
main = do
  key <- getEnv "KEY"
  secret <- getEnv "SECRET"
  let hId = "81Y6NR5000CX"
      creds = Credentials key secret
  handwriting <- getHandwriting hId creds
  print handwriting
  --handwritings <- getHandwritings creds
  --mapM_ print handwritings

