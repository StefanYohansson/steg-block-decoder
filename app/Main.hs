module Main (main) where

import Lib
import System.Environment

main :: IO ()
main = do
    args <- getArgs
    case args of
        [file, output_file] -> do
            content <- readFile file
            stegDecoder content output_file
        _ -> putStrLn "Usage: steg-block-decoder <file> <output_file>"