{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}

module Parsing
    ( readConfig,
      QuestData (..),
      SubQuestData (..)
    ) where
import TOML (DecodeTOML, tomlDecoder, getField, decodeFile, Decoder, TOMLError, getFieldOpt)
import GHC.RTS.Flags ()

data QuestData = QuestData {
   title:: String,
   starting_quest:: String,
   ending_quests:: [String],
   sub_quests:: [SubQuestData]
} deriving (Show, Eq)

data SubQuestData = SubQuestData {
    name:: String,
    plot:: [String],
    end_type:: String,
    next:: Maybe String
} deriving (Show, Eq)

instance DecodeTOML QuestData where
    tomlDecoder :: Decoder QuestData
    tomlDecoder = 
        QuestData
            <$> getField "title"
            <*> getField "starting_quest"
            <*> getField "ending_quests"
            <*> getField "subquests"

instance DecodeTOML SubQuestData where
    tomlDecoder :: Decoder SubQuestData
    tomlDecoder = 
        SubQuestData
            <$> getField "name"
            <*> getField "plot"
            <*> getField "end_type"
            <*> getFieldOpt "next"

readConfig:: FilePath -> IO (Either TOMLError QuestData)
readConfig path = do
    result <- decodeFile path
    case result of
        Right cfg -> return (Right (cfg :: QuestData))
        Left e -> return (Left e)
