import Test.Hspec
import Parsing
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
