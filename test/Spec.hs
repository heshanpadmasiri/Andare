import Test.Hspec
import Parsing
import Lib
main :: IO ()
main = hspec $ do
    describe "parsing config files" $ do
        it "can read fixed linear quest" $ do
            let configPath = "./test/data/linear1.toml"
            config <- readConfig configPath
            config `shouldBe` Right (QuestData "TITLE" "subquest_0" ["subquest_1"] [
                                        SubQuestData "subquest_0" ["str_1", "str_2"] "continue" (Just "subquest_1"),
                                        SubQuestData "subquest_1" ["str_3", "str_4"] "terminal" Nothing
                                    ])
            let result = case config of
                            Right (QuestData _ _ _ sq) -> Just (parseSubQuests sq)
                            _ -> Nothing
            let sub_quest_1 = SubQuest ["str_3", "str_4"] "terminal" Nothing
            let sub_quest_0 = SubQuest ["str_1", "str_2"] "continue" (Just sub_quest_1)
            result `shouldBe` Just [Right sub_quest_0, Right sub_quest_1]
            let quest = case config of
                            Right q -> Just (parseQuest q)
                            _ -> Nothing
            print quest
            quest `shouldBe` Just (Right (Quest "TITLE" sub_quest_0))

        it "can parse fixed linear quest sub quests" $ do
            let sq = [
                        SubQuestData "subquest_0" ["str_1", "str_2"] "continue" (Just "subquest_1"),
                        SubQuestData "subquest_1" ["str_3", "str_4"] "terminal" Nothing
                     ]
            let sub_quest_1 = SubQuest ["str_3", "str_4"] "terminal" Nothing
            let sub_quest_0 = SubQuest ["str_1", "str_2"] "continue" (Just sub_quest_1)
            parseSubQuests sq `shouldBe` [Right sub_quest_0, Right sub_quest_1]

        it "correctly detect terminal cases with next quests" $ do
           let sq = [
                        SubQuestData "subquest_0" ["str_1", "str_2"] "terminal" (Just "subquest_1"),
                        SubQuestData "subquest_1" ["str_3", "str_4"] "terminal" Nothing
                    ]
           let sub_quest_0 = FailedToParseSubQuest
           let sub_quest_1 = SubQuest ["str_3", "str_4"] "terminal" Nothing
           parseSubQuests sq `shouldBe` [Left sub_quest_0, Right sub_quest_1]

        it "correctly detect continue cases with invalid next quest ids" $ do
           let sq = [
                        SubQuestData "subquest_0" ["str_1", "str_2"] "continue" (Just "subquest_non_existing"),
                        SubQuestData "subquest_1" ["str_3", "str_4"] "terminal" Nothing
                    ]
           let sub_quest_0 = InvalidQuestId
           let sub_quest_1 = SubQuest ["str_3", "str_4"] "terminal" Nothing
           parseSubQuests sq `shouldBe` [Left sub_quest_0, Right sub_quest_1]

        it "correctly cascade error" $ do
           let sq = [
                        SubQuestData "subquest_0" ["str_1", "str_2"] "continue" (Just "subquest_non_existing"),
                        SubQuestData "subquest_1" ["str_3", "str_4"] "continue" (Just "subquest_0")
                    ]
           let sub_quest_0 = InvalidQuestId
           let sub_quest_1 = FailedToParseNextQuest
           parseSubQuests sq `shouldBe` [Left sub_quest_0, Left sub_quest_1]

        it "correctly parse quest data" $ do
           let qd = QuestData "TITLE" "subquest_0" ["subquest_1"] [SubQuestData "subquest_0" ["str_1", "str_2"] "continue" (Just "subquest_1"),
                                                                   SubQuestData "subquest_1" ["str_3", "str_4"] "terminal" Nothing]
           let sub_quest_1 = SubQuest ["str_3", "str_4"] "terminal" Nothing
           let sub_quest_0 = SubQuest ["str_1", "str_2"] "continue" (Just sub_quest_1)
           parseQuest qd `shouldBe` Right (Quest "TITLE" sub_quest_0)

        it "correctly cascade errors to parsing quests" $ do
           let qd = QuestData "TITLE" "subquest_0" ["subquest_1"] [SubQuestData "subquest_0" ["str_1", "str_2"] "continue" (Just "subquest_1"),
                                                                   SubQuestData "subquest_1" ["str_3", "str_4"] "terminal" (Just "nonexisting")]
           parseQuest qd `shouldBe` Left FailedToParseNextQuest
           let qd_1 = QuestData "TITLE" "subquest_0" ["subquest_1"] [SubQuestData "subquest_0" ["str_1", "str_2"] "continue" (Just "subquest_1"),
                                                                     SubQuestData "subquest_1" ["str_3", "str_4"] "continue" (Just "nonexisting")]
           parseQuest qd_1 `shouldBe` Left FailedToParseNextQuest

        it "correctly detect invalid starting quest ids" $ do
           let qd = QuestData "TITLE" "none_existing" ["subquest_1"] [SubQuestData "subquest_0" ["str_1", "str_2"] "continue" (Just "subquest_1"),
                                                                      SubQuestData "subquest_1" ["str_3", "str_4"] "terminal" Nothing]
           parseQuest qd `shouldBe` Left InvalidQuestId
