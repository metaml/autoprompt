module Main where

import Etc.Autoprompt (port, run)

main :: IO ()
main = putStrLn ("listening on port: " <> show port) >> run
