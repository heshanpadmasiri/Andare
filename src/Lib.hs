module Lib
    (
        parseSubQuests,
        SubQuest (..),
        ParseError (..)
    ) where

import Parsing
import Data.HashMap.Internal (HashMap)
import Data.HashMap.Strict (fromList, lookup)

-- data Quest = Quest {
--     title:: String,
--     starting_quest:: SubQuest
-- }

data SubQuest = SubQuest {
    plot:: [String],
    end_type:: String,
    next:: Maybe SubQuest
} deriving (Show, Eq)

data ParseError = InvalidQuestId | FailedToParseSubQuest | FailedToParseNextQuest deriving(Show, Eq)

parseSubQuests:: [SubQuestData] -> [Either ParseError SubQuest]
parseSubQuests sub_quests_data = parsed_quests
    where
    quest_map = fromList (map (\each -> (name each, each)) sub_quests_data)
    -- directly do a list comprihension here  (plot and end_types are already known)
    quest_parser = parseSubQuest quest_map
    parsed_quests = map quest_parser sub_quests_data

-- return a proper error type
parseSubQuest :: Data.HashMap.Internal.HashMap String SubQuestData -> SubQuestData -> Either ParseError SubQuest
parseSubQuest  quest_map sub_quest_data = sub_quest
    where
    sub_quest = case sub_quest_data of
        SubQuestData _ p "continue"  (Just next_id) -> next_lookup where
            next_lookup = case Data.HashMap.Strict.lookup next_id quest_map of
                            Nothing -> Left InvalidQuestId
                            Just next_quest -> r1 where
                                r1 = case parseSubQuest quest_map next_quest of
                                    Left _ -> Left FailedToParseNextQuest
                                    Right s -> Right (SubQuest p "continue" (Just s))
        SubQuestData _ p "terminal" Nothing -> Right (SubQuest p "terminal" Nothing)
        _ -> Left FailedToParseSubQuest
