module Main where
import Etc.Autoprompt (run)

main :: IO ()
main = putStrLn "autoprompt starting" >> run
