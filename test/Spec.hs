import Test.Hspec
import Parsing
import Lib
main :: IO ()
main = hspec $ do
    describe "parsing config files" $ do
        it "can read fixed linear quest" $ do
            let configPath = "./test/data/linear1.toml"
            config <- readConfig configPath
            config `shouldBe` Right (QuestData "TITLE" "quest_0" ["quest_1"] [
                                        SubQuestData "subquest_0" ["str_1", "str_2"] "continue" (Just "subquest_1"),
                                        SubQuestData "subquest_1" ["str_3", "str_4"] "terminal" Nothing
                                    ])
            let result = case config of
                            Right (QuestData _ _ _ sq) -> Just (parseSubQuests sq)
                            _ -> Nothing
            let sub_quest_1 = SubQuest ["str_3", "str_4"] "terminal" Nothing
            let sub_quest_0 = SubQuest ["str_1", "str_2"] "continue" (Just sub_quest_1)
            result `shouldBe` Just [Right sub_quest_0, Right sub_quest_1]

        it "can parse fixed linear quest" $ do
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
