import Test.Hspec
import Parsing
import Lib
main :: IO ()
main = hspec $ do
    describe "parsing config files" $ do
        it "can parse fixed linear quest" $ do
            let configPath = "./test/data/linear1.toml"
            config <- readConfig configPath
            config `shouldBe` Left (QuestData "TITLE" "quest_0" ["quest_1"] [
                                        (SubQuestData "subquest_0" ["str_1", "str_2"] "continue" (Just "subquest_1")),
                                        (SubQuestData "subquest_1" ["str_3", "str_4"] "terminal" Nothing)
                                    ])
            let result = case config of
                            Left (QuestData _ _ _ sq) -> Just (parseSubQuests sq)
                            _ -> Nothing
            let sub_quest_1 = SubQuest ["str_3", "str_4"] "terminal" Nothing
            let sub_quest_0 = SubQuest ["str_1", "str_2"] "continue" (Just sub_quest_1)
            result `shouldBe` Just [sub_quest_0, sub_quest_1]
