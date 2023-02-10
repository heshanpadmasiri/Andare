import Test.Hspec
import Lib
main :: IO ()
main = hspec $ do
    describe "parsing config files" $ do
        it "can parse fixed linear quest" $ do
            let configPath = "./test/data/linear1.toml"
            config <- readConfig configPath
            config `shouldBe` Left (Quest "TITLE" "quest_0" ["quest_1"] [
                                        (SubQuest "subquest_0" ["str_1", "str_2"] "continue" (Just "subquest_1")),
                                        (SubQuest "subquest_1" ["str_3", "str_4"] "terminal" Nothing)
                                    ])
