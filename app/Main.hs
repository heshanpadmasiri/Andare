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
runQuest q = do
    f <- case vars q of
                Nothing -> pickVar ["!NO_FILLER_GIVEN"]
                Just v -> pickVar v
    runSubQuest (starting_quest q) f

pickVar:: [String] -> IO String
pickVar v = do
    index <- randomRIO (0, ((length v) - 1)) :: IO Int
    return (v !! index)


runSubQuest:: SubQuest -> String -> IO ()
runSubQuest (TerminalSubQuest p) f = printPlot p f
runSubQuest (ContinueSubQuest p nq) f = do
    printPlot p f
    putStrLn "press enter to continue"
    _ <- getLine
    runSubQuest nq f
runSubQuest (BranchingSubQuest p c) f = do
    printPlot p f
    nq <- getChoice c
    runSubQuest nq f
runSubQuest (RandomizedSubQuest p c) f = do
    printPlot p f
    putStrLn "random event press enter to continue"
    seed <- randomIO :: IO Double
    runSubQuest (pickChoice c seed) f

printPlot:: [String] -> String -> IO ()
printPlot p f = mapM_ putStrLn plot_lines where
    plot_lines = fillPlot f p

fillPlot:: String -> [String] -> [String]
fillPlot f = map (fillLine f)

fillLine:: String -> String -> String
fillLine f l = unwords $ fillWords (words l) f

fillWords:: [String] -> String -> [String]
fillWords [] _ = []
fillWords (x:xs) w = if x == "{var}" then w : fillWords xs w else x : fillWords xs w

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
