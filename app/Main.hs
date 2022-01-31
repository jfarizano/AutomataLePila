module Main where

import Lib
import Parse
import System.FilePath ( splitExtension )
import System.Environment (getArgs)

main :: IO ()
main = do args <- getArgs
          let filename = head args
          let (_, ext) = splitExtension filename
          if ext /= ".pda"
          then putStrLn "Invalid file extension"
          else do text <- readFile filename
                  let pda = parsePDA $ lexer text
                  putStrLn $ show pda
