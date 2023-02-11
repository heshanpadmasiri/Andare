import Test.Hspec
import Parsing
import Lib
main :: IO ()
main = hspec $ do
    describe "parsing branching configs" $ do
        it "can parse branching config file" $ do
            config <- readConfig "./test/data/branching1.toml"
            config `shouldBe` Right (QuestData "TITLE" "subquest_0" ["subquest_3"] [
                                        BranchingSubQuestData "subquest_0" ["..", ".."] [("prompt_1", "subquest_1"),
                                                                                         ("prompt_2", "subquest_2")],
                                        ContinueSubQuestData "subquest_1" [".."] "subquest_3",
                                        ContinueSubQuestData "subquest_2" [".."] "subquest_3",
                                        TerminalSubQuestData "subquest_3" [".."]
                                    ])

    describe "parsing linear configs" $ do
        it "can read fixed linear quest" $ do
            let configPath = "./test/data/linear1.toml"
            config <- readConfig configPath
            config `shouldBe` Right (QuestData "TITLE" "subquest_0" ["subquest_1"] [
                                        ContinueSubQuestData "subquest_0" ["str_1", "str_2"] "subquest_1",
                                        TerminalSubQuestData "subquest_1" ["str_3", "str_4"]
                                    ])
            let result = case config of
                            Right (QuestData _ _ _ sq) -> Just (parseSubQuests sq)
                            _ -> Nothing
            let sub_quest_1 = TerminalSubQuest ["str_3", "str_4"]
            let sub_quest_0 = ContinueSubQuest ["str_1", "str_2"] sub_quest_1
            result `shouldBe` Just [Right sub_quest_0, Right sub_quest_1]
            let quest = case config of
                            Right q -> Just (parseQuest q)
                            _ -> Nothing
            quest `shouldBe` Just (Right (Quest "TITLE" sub_quest_0))

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
           let sub_quest_0 = InvalidQuestId
           let sub_quest_1 = TerminalSubQuest ["str_3", "str_4"]
           parseSubQuests sq `shouldBe` [Left sub_quest_0, Right sub_quest_1]

        it "correctly cascade error" $ do
           let sq = [
                        ContinueSubQuestData "subquest_0" ["str_1", "str_2"] "subquest_non_existing",
                        ContinueSubQuestData "subquest_1" ["str_3", "str_4"] "subquest_0"
                    ]
           let sub_quest_0 = InvalidQuestId
           let sub_quest_1 = FailedToParseNextQuest
           parseSubQuests sq `shouldBe` [Left sub_quest_0, Left sub_quest_1]

        it "correctly parse quest data" $ do
           let qd = QuestData "TITLE" "subquest_0" ["subquest_1"] [ContinueSubQuestData "subquest_0" ["str_1", "str_2"] "subquest_1",
                                                                   TerminalSubQuestData "subquest_1" ["str_3", "str_4"]]
           let sub_quest_1 = TerminalSubQuest ["str_3", "str_4"]
           let sub_quest_0 = ContinueSubQuest ["str_1", "str_2"] sub_quest_1
           parseQuest qd `shouldBe` Right (Quest "TITLE" sub_quest_0)

        it "correctly cascade errors to parsing quests" $ do
           let qd_1 = QuestData "TITLE" "subquest_0" ["subquest_1"] [ContinueSubQuestData "subquest_0" ["str_1", "str_2"] "subquest_1",
                                                                     ContinueSubQuestData "subquest_1" ["str_3", "str_4"] "nonexisting"]
           parseQuest qd_1 `shouldBe` Left FailedToParseNextQuest

        it "correctly detect invalid starting quest ids" $ do
           let qd = QuestData "TITLE" "none_existing" ["subquest_1"] [ContinueSubQuestData "subquest_0" ["str_1", "str_2"] "subquest_1",
                                                                      TerminalSubQuestData "subquest_1" ["str_3", "str_4"]]
           parseQuest qd `shouldBe` Left InvalidQuestId
