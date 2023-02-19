module Main (main) where

import Parsing (readConfig)
import Lib
import System.Environment (getArgs)
import System.Random


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
        Right quest -> runQuest quest
    putStrLn "END"

runQuest:: Quest -> IO ()
runQuest q = runSubQuest (starting_quest q)

runSubQuest:: SubQuest -> IO ()
runSubQuest (TerminalSubQuest p) = printPlot p
runSubQuest (ContinueSubQuest p nq) = do
    printPlot p
    putStrLn "press enter to continue"
    _ <- getLine
    runSubQuest nq
runSubQuest (BranchingSubQuest p c) = do
    printPlot p
    nq <- getChoice c
    runSubQuest nq
runSubQuest (RandomizedSubQuest p c) = do
    printPlot p
    putStrLn "random event press enter to continue"
    seed <- randomIO :: IO Double
    runSubQuest (pickChoice c seed)

printPlot:: [String] -> IO ()
printPlot p  = mapM_ putStrLn p

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
    
pickChoice:: [(Double, SubQuest)] -> Double -> SubQuest
pickChoice [] _ = error "UNEXPECTED: failed to randomly pick a sub quest"
pickChoice (x:xs) v = if cur >= v then curQuest else pickChoice xs (v-cur) where
    cur = fst x
    curQuest = snd x
