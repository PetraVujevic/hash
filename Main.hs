module Main (
   main
 , script
) where
import Hash 

main :: IO ()
main = runInteractive 

script :: IO ()
script = runScript "Script.hs" 