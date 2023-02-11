module Lib
    (
        parseQuest,
        parseSubQuests,
        Quest (..),
        SubQuest (..),
        ParseError (..)
    ) where

import Parsing
import Data.HashMap.Internal (HashMap)
import Data.HashMap.Strict (fromList, lookup)

data Quest = Quest {
    title:: String,
    starting_quest:: SubQuest
} deriving (Show, Eq)

data SubQuest = ContinueSubQuest [String] SubQuest | TerminalSubQuest [String] deriving(Show, Eq)

parseQuest:: QuestData -> Either ParseError Quest
parseQuest quest_data = quest
    where
    sub_quest_data = sub_quests quest_data
    parsed_sub_quests = parseSubQuests sub_quest_data
    quest = case findSubQuestByTitle (Parsing.starting_quest quest_data) (zip sub_quest_data parsed_sub_quests) of
        Left err -> Left err
        Right sq -> Right (Quest (Parsing.title quest_data) sq)

findSubQuestByTitle:: String -> [(SubQuestData, Either ParseError SubQuest)] -> Either ParseError SubQuest
findSubQuestByTitle _ [] = Left InvalidQuestId
findSubQuestByTitle expected_title (x:xs) = case x of
                (_, Left err) -> Left err
                (TerminalSubQuestData cur_title _, quest) -> if cur_title == expected_title then quest else findSubQuestByTitle expected_title xs
                (ContinueSubQuestData cur_title _ _, quest) -> if cur_title == expected_title then quest else findSubQuestByTitle expected_title xs

data ParseError = InvalidQuestId | FailedToParseSubQuest | FailedToParseNextQuest deriving(Show, Eq)

parseSubQuests:: [SubQuestData] -> [Either ParseError SubQuest]
parseSubQuests sub_quests_data = parsed_quests
    where
    quest_map = fromList (map (\each -> (name each, each)) sub_quests_data)
    quest_parser = parseSubQuest quest_map
    parsed_quests = map quest_parser sub_quests_data

parseSubQuest :: Data.HashMap.Internal.HashMap String SubQuestData -> SubQuestData -> Either ParseError SubQuest
parseSubQuest  quest_map sub_quest_data = sub_quest
    where
    sub_quest = case sub_quest_data of
        ContinueSubQuestData _ p next_id -> next_lookup where
            next_lookup = case Data.HashMap.Strict.lookup next_id quest_map of
                            Nothing -> Left InvalidQuestId
                            Just next_quest -> r1 where
                                r1 = case parseSubQuest quest_map next_quest of
                                    Left _ -> Left FailedToParseNextQuest
                                    Right s -> Right (ContinueSubQuest p s)
        TerminalSubQuestData _ p -> Right (TerminalSubQuest p)
