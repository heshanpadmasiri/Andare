import Test.Hspec
import Lib
main :: IO ()
main = hspec $ do
    describe "parsing config files" $ do
        it "can parse fixed linear quest" $ do
            result <- readConfig "./test/data/linear1.toml"
            result `shouldBe` Left (Quest "TITLE")
