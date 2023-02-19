{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}

module Parsing
    ( readConfig,
      QuestData (..),
      SubQuestData (..),
      name
    ) where
import TOML (DecodeTOML, tomlDecoder, getField, decodeFile, Decoder, TOMLError, getFieldOpt)
import GHC.RTS.Flags ()

data QuestData = QuestData {
   title:: String,
   starting_quest:: String,
   ending_quests:: [String],
   vars:: Maybe [String],
   sub_quests:: [SubQuestData]
} deriving (Show, Eq)

data SubQuestData = ContinueSubQuestData String [String] String
                    | TerminalSubQuestData String [String]
                    | BranchingSubQuestData String [String] [(String, String)] 
                    | RandomizedSubQuestData String [String] [(Double, String)] deriving(Show, Eq)

name:: SubQuestData -> String
name sq = case sq of 
    TerminalSubQuestData n _ -> n
    ContinueSubQuestData n _ _ -> n
    BranchingSubQuestData n _ _ -> n
    RandomizedSubQuestData n _ _ -> n

instance DecodeTOML QuestData where
    tomlDecoder :: Decoder QuestData
    tomlDecoder = 
        QuestData
            <$> getField "title"
            <*> getField "starting_quest"
            <*> getField "ending_quests"
            <*> getFieldOpt "vars"
            <*> getField "subquests"

instance DecodeTOML SubQuestData where
    tomlDecoder:: Decoder SubQuestData
    tomlDecoder = do
        end_type <- getField "end_type"
        case end_type of
            "continue" -> ContinueSubQuestData 
                            <$> getField "name"
                            <*> getField "plot"
                            <*> getField "next"
            "player_choice" -> BranchingSubQuestData
                            <$> getField "name"
                            <*> getField "plot"
                            <*> getField "next"
            "random_event" -> RandomizedSubQuestData
                            <$> getField "name"
                            <*> getField "plot"
                            <*> getField "next"
            "terminal" -> TerminalSubQuestData
                            <$> getField "name"
                            <*> getField "plot"
            _ -> fail $ "Invalid end_type " <> end_type

readConfig:: FilePath -> IO (Either TOMLError QuestData)
readConfig path = do
    result <- decodeFile path
    case result of
        Right cfg -> return (Right (cfg :: QuestData))
        Left e -> return (Left e)
