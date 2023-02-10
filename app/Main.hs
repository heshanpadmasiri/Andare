module Main (main) where

import Parsing

main :: IO ()
main = do 
    result <- readConfig "./test/data/linear1.toml"
    print result
