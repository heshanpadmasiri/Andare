{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}

module Lib
    ( someFunc,
      readConfig,
      Quest (..),
      SubQuest (..)
    ) where
import TOML (DecodeTOML, tomlDecoder, getField, decodeFile, Decoder, TOMLError)
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
    name:: String,
    plot:: [String],
    -- TODO: end type must be enum
    end_type:: String,
    -- TODO: should be optional
    next:: String
} deriving (Show, Eq)

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
            <*> getField "next"

readConfig:: FilePath -> IO (Either Quest TOMLError)
readConfig path = do
    result <- decodeFile path
    case result of
        Right cfg -> return (Left (cfg :: Quest))
        Left e -> return (Right e)
