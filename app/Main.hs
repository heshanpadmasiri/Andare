module Main (main) where

import Parsing (readConfig)
import Lib
import System.Environment (getArgs)


data RuntimeErr = FailedToReadConfig|FailedToParseConfig deriving(Show)
main :: IO ()
main = do 
    args <- getArgs
    c <-readConfig (head args)
    let result = case c of
            Left _-> Left FailedToReadConfig
            Right config -> case parseQuest config of
                                Left _ -> Left FailedToParseConfig 
                                Right q -> Right q
    case result of
        Left err -> print err
        Right quest -> runSubQuest (starting_quest quest)
    putStrLn "END"

runSubQuest:: SubQuest -> IO () 
runSubQuest (TerminalSubQuest p) = mapM_ putStrLn p
runSubQuest (ContinueSubQuest p nq) = do
        mapM_ putStrLn p
        putStrLn "press enter to continue"
        _ <- getLine
        runSubQuest nq
                            
