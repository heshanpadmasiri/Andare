import Test.Hspec
import Parsing
import Lib
main :: IO ()
main = hspec $ do
    describe "parsing vars in config" $ do
        it "can read config file with vars" $ do
            config <- readConfig "./test/data/var_sub.toml"
            config `shouldBe` Right (QuestData "TITLE" "subquest_0" ["subquest_1"] (Just  ["a", "b"]) [
                                        ContinueSubQuestData "subquest_0" ["{var}", "str_2 {var}"] "subquest_1",
                                        TerminalSubQuestData "subquest_1" ["str_3 {var} str_3_rest", "{var} str_4"]
                                    ])
        it "can parse config with vars" $ do
            let qd = QuestData "TITLE" "subquest_0" ["subquest_1"] (Just  ["a"]) [
                        ContinueSubQuestData "subquest_0" ["{var}", "str_2 {var}"] "subquest_1",
                        TerminalSubQuestData "subquest_1" ["str_3 {var} str_3_rest", "{var} str_4"]
                     ]
            parseQuest qd `shouldBe` Right (Quest "TITLE" (Just ["a"]) (ContinueSubQuest ["{var}", "str_2 {var}"] (TerminalSubQuest ["str_3 {var} str_3_rest", "{var} str_4"])))

    describe "parsing random configs" $ do
        it "can read random config file" $ do
            config <- readConfig "./test/data/random1.toml"
            config `shouldBe` Right (QuestData "TITLE" "subquest_0" ["subquest_3"] Nothing [
                                        RandomizedSubQuestData "subquest_0" ["..", ".."] [(0.5, "subquest_1"),
                                                                                          (0.5, "subquest_2")],
                                        ContinueSubQuestData "subquest_1" ["1"] "subquest_3",
                                        ContinueSubQuestData "subquest_2" ["2"] "subquest_3",
                                        TerminalSubQuestData "subquest_3" ["end"]
                                    ])
        it "can parse random config" $ do
            let sq = [RandomizedSubQuestData "subquest_0" ["..", ".."] [(0.5, "subquest_1"),
                                                                        (0.5, "subquest_2")],
                      ContinueSubQuestData "subquest_1" [".."] "subquest_3",
                      ContinueSubQuestData "subquest_2" [".."] "subquest_3",
                      TerminalSubQuestData "subquest_3" [".."]]
            let sq_3 = TerminalSubQuest [".."]
            let sq_1 = ContinueSubQuest [".."] sq_3
            let sq_2 = ContinueSubQuest [".."] sq_3
            let sq_0 = RandomizedSubQuest ["..", ".."] [(0.5, sq_1), (0.5, sq_2)]
            parseSubQuests sq `shouldBe` [Right sq_0, Right sq_1, Right sq_2, Right sq_3]
        it "can detect when sum of probablities is less than 1" $ do
            let sq = [RandomizedSubQuestData "subquest_0" ["..", ".."] [(0.5, "subquest_1"),
                                                                        (0.4, "subquest_2")],
                      ContinueSubQuestData "subquest_1" [".."] "subquest_3",
                      ContinueSubQuestData "subquest_2" [".."] "subquest_3",
                      TerminalSubQuestData "subquest_3" [".."]]
            let sq_3 = TerminalSubQuest [".."]
            let sq_1 = ContinueSubQuest [".."] sq_3
            let sq_2 = ContinueSubQuest [".."] sq_3
            parseSubQuests sq `shouldBe` [Left RandomEventProbablitiesInvalid, Right sq_1, Right sq_2, Right sq_3]

        it "can detect when probablities are invalid" $ do
            let sq = [RandomizedSubQuestData "subquest_0" ["..", ".."] [(-0.5, "subquest_1"),
                                                                        (1.0, "subquest_2")],
                      ContinueSubQuestData "subquest_1" [".."] "subquest_3",
                      ContinueSubQuestData "subquest_2" [".."] "subquest_3",
                      TerminalSubQuestData "subquest_3" [".."]]
            let sq_3 = TerminalSubQuest [".."]
            let sq_1 = ContinueSubQuest [".."] sq_3
            let sq_2 = ContinueSubQuest [".."] sq_3
            parseSubQuests sq `shouldBe` [Left RandomEventProbablitiesInvalid, Right sq_1, Right sq_2, Right sq_3]


    describe "parsing branching configs" $ do
        it "can read branching config file" $ do
            config <- readConfig "./test/data/branching1.toml"
            config `shouldBe` Right (QuestData "TITLE" "subquest_0" ["subquest_3"] Nothing [
                                        BranchingSubQuestData "subquest_0" ["..", ".."] [("prompt_1", "subquest_1"),
                                                                                         ("prompt_2", "subquest_2")],
                                        ContinueSubQuestData "subquest_1" [".."] "subquest_3",
                                        ContinueSubQuestData "subquest_2" [".."] "subquest_3",
                                        TerminalSubQuestData "subquest_3" [".."]
                                    ])
        it "can parse branching config" $ do
            let sq = [BranchingSubQuestData "subquest_0" ["..", ".."] [("prompt_1", "subquest_1"),
                                                                       ("prompt_2", "subquest_2")],
                      ContinueSubQuestData "subquest_1" [".."] "subquest_3",
                      ContinueSubQuestData "subquest_2" [".."] "subquest_3",
                      TerminalSubQuestData "subquest_3" [".."]]
            let sq_3 = TerminalSubQuest [".."]
            let sq_1 = ContinueSubQuest [".."] sq_3
            let sq_2 = ContinueSubQuest [".."] sq_3
            let sq_0 = BranchingSubQuest ["..", ".."] [("prompt_1", sq_1), ("prompt_2", sq_2)]
            parseSubQuests sq `shouldBe` [Right sq_0, Right sq_1, Right sq_2, Right sq_3]

    describe "parsing linear configs" $ do
        it "can read fixed linear quest" $ do
            let configPath = "./test/data/linear1.toml"
            config <- readConfig configPath
            config `shouldBe` Right (QuestData "TITLE" "subquest_0" ["subquest_1"] Nothing [
                                        ContinueSubQuestData "subquest_0" ["str_1", "str_2"] "subquest_1",
                                        TerminalSubQuestData "subquest_1" ["str_3", "str_4"]
                                    ])
            let result = case config of
                            Right (QuestData _ _ _ _ sq) -> Just (parseSubQuests sq)
                            _ -> Nothing
            let sub_quest_1 = TerminalSubQuest ["str_3", "str_4"]
            let sub_quest_0 = ContinueSubQuest ["str_1", "str_2"] sub_quest_1
            result `shouldBe` Just [Right sub_quest_0, Right sub_quest_1]
            let quest = case config of
                            Right q -> Just (parseQuest q)
                            _ -> Nothing
            quest `shouldBe` Just (Right (Quest "TITLE" Nothing sub_quest_0))

        it "can parse fixed linear quest sub quests" $ do
            let sq = [
                        ContinueSubQuestData "subquest_0" ["str_1", "str_2"] "subquest_1",
                        TerminalSubQuestData "subquest_1" ["str_3", "str_4"]
                     ]
            let sub_quest_1 = TerminalSubQuest ["str_3", "str_4"]
            let sub_quest_0 = ContinueSubQuest ["str_1", "str_2"] sub_quest_1
            parseSubQuests sq `shouldBe` [Right sub_quest_0, Right sub_quest_1]

        it "correctly detect continue cases with invalid next quest ids" $ do
           let sq = [
                        ContinueSubQuestData "subquest_0" ["str_1", "str_2"] "subquest_non_existing",
                        TerminalSubQuestData "subquest_1" ["str_3", "str_4"]
                    ]
           let sub_quest_0 = FailedToParseNextQuest
           let sub_quest_1 = TerminalSubQuest ["str_3", "str_4"]
           parseSubQuests sq `shouldBe` [Left sub_quest_0, Right sub_quest_1]

        it "correctly cascade error" $ do
           let sq = [
                        ContinueSubQuestData "subquest_0" ["str_1", "str_2"] "subquest_non_existing",
                        ContinueSubQuestData "subquest_1" ["str_3", "str_4"] "subquest_0"
                    ]
           let sub_quest_0 = FailedToParseNextQuest
           let sub_quest_1 = FailedToParseNextQuest
           parseSubQuests sq `shouldBe` [Left sub_quest_0, Left sub_quest_1]

        it "correctly parse quest data" $ do
           let qd = QuestData "TITLE" "subquest_0" ["subquest_1"] Nothing [ContinueSubQuestData "subquest_0" ["str_1", "str_2"] "subquest_1",
                                                                           TerminalSubQuestData "subquest_1" ["str_3", "str_4"]]
           let sub_quest_1 = TerminalSubQuest ["str_3", "str_4"]
           let sub_quest_0 = ContinueSubQuest ["str_1", "str_2"] sub_quest_1
           parseQuest qd `shouldBe` Right (Quest "TITLE" Nothing sub_quest_0)

        it "correctly cascade errors to parsing quests" $ do
           let qd_1 = QuestData "TITLE" "subquest_0" ["subquest_1"] Nothing [ContinueSubQuestData "subquest_0" ["str_1", "str_2"] "subquest_1",
                                                                             ContinueSubQuestData "subquest_1" ["str_3", "str_4"] "nonexisting"]
           parseQuest qd_1 `shouldBe` Left FailedToParseNextQuest

        it "correctly detect invalid starting quest ids" $ do
           let qd = QuestData "TITLE" "none_existing" ["subquest_1"] Nothing [ContinueSubQuestData "subquest_0" ["str_1", "str_2"] "subquest_1",
                                                                              TerminalSubQuestData "subquest_1" ["str_3", "str_4"]]
           parseQuest qd `shouldBe` Left InvalidQuestId
