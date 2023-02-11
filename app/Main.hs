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
runSubQuest (BranchingSubQuest p c) = do
    mapM_ putStrLn p
    nq <- getChoice c
    runSubQuest nq
                            
getChoice:: [(String, SubQuest)] -> IO SubQuest
getChoice choices = do
    putStrLn "PICK:"
    let prompts = createPrompts choices 0
    mapM_ putStrLn prompts
    i <- repeatedGetInt $ length choices
    return (snd (choices !! i))
    
repeatedGetInt:: Int -> IO Int
repeatedGetInt m = do
    putStrLn ("select 0 -- " ++ show (m-1))
    val <- getLine
    let x = (read val :: Int)
    if x < m 
        then return x
        else 
            repeatedGetInt m

createPrompts:: [(String, SubQuest)] -> Int -> [String]
createPrompts [] _ = []
createPrompts (x:xs) i = res where
    l = "\t[" ++ show i ++ "]: " ++ fst x
    res = l : createPrompts xs (i+1)
    
