module Lib
    (
        parseSubQuests,
        SubQuest (..)
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

parseSubQuests:: [SubQuestData] -> [SubQuest]
parseSubQuests sub_quests_data = parsed_quests
    where
    quest_map = fromList (map (\each -> (name each, each)) sub_quests_data)
    -- directly do a list comprihension here  (plot and end_types are already known)
    quest_parser = parseSubQuest quest_map
    parsed_quests = map quest_parser sub_quests_data

-- return a proper error type
parseSubQuest :: Data.HashMap.Internal.HashMap String SubQuestData -> SubQuestData -> SubQuest
parseSubQuest  quest_map sub_quest_data = sub_quest
    where
    sub_quest = case sub_quest_data of
        SubQuestData _ p "continue"  (Just next_id) -> SubQuest p "continue" (Just (parseSubQuest quest_map child_quest_data)) 
            where child_quest_data = case Data.HashMap.Strict.lookup next_id quest_map of
                    Nothing -> error "failed to find quest by id"
                    Just x -> x
        SubQuestData _ p "terminal"  _ -> SubQuest p "terminal" Nothing
        _ -> error "failed to parse subquest"
