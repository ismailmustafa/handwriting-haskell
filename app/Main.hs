module Main where

import Network.Handwriting

import System.Environment (getEnv)

main :: IO ()
main = do
  key <- getEnv "KEY"
  secret <- getEnv "SECRET"
  
  let creds = Credentials key secret
  handwritings <- getHandwritings creds
  print $ length handwritings

  let id = "81Y6NR5000CX"
  handwriting <- getHandwriting id creds
  print handwriting
