{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}

module Lib
    ( someFunc,
      readConfig,
      Quest (..),
      SubQuest (..)
    ) where
import TOML (DecodeTOML, tomlDecoder, getField, decodeFile, Decoder, TOMLError, getFieldOpt)
import GHC.RTS.Flags ()

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data Quest = Quest {
   title:: String,
   starting_quest:: String,
   ending_quests:: [String],
   sub_quests:: [SubQuest]
} deriving (Show, Eq)

data SubQuest = SubQuest {
    name:: QuestId,
    plot:: [String],
    end_type:: String,
    next:: Maybe QuestId
} deriving (Show, Eq)

type QuestId = String;

instance DecodeTOML Quest where
    tomlDecoder :: Decoder Quest
    tomlDecoder = 
        Quest
            <$> getField "title"
            <*> getField "starting_quest"
            <*> getField "ending_quests"
            <*> getField "subquests"

instance DecodeTOML SubQuest where
    tomlDecoder :: Decoder SubQuest
    tomlDecoder = 
        SubQuest
            <$> getField "name"
            <*> getField "plot"
            <*> getField "end_type"
            <*> getFieldOpt "next"

readConfig:: FilePath -> IO (Either Quest TOMLError)
readConfig path = do
    result <- decodeFile path
    case result of
        Right cfg -> return (Left (cfg :: Quest))
        Left e -> return (Right e)
