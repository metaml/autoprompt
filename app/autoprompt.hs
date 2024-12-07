module Main where

import Etc.Autoprompt (port, run)

main :: IO ()
main = do
  putStrLn ("listening on port: " <> show port)
  run
