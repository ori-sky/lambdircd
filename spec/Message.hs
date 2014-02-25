import Test.Hspec
import IRC.Message
import IRC.Prefix

main :: IO ()
main = hspec $ do
    describe "IRC.Message" $ do
        it "parses IRC message parameters" $ do
            ircParams "one two three four" `shouldBe` ["one", "two", "three", "four"]
            ircParams "one two :three four" `shouldBe` ["one", "two", "three four"]
            ircParams ":one two three four" `shouldBe` ["one two three four"]
            ircParams ":one two :three four" `shouldBe` ["one two :three four"]
            ircParams "one" `shouldBe` ["one"]
            ircParams "one " `shouldBe` ["one"]
            ircParams "one  " `shouldBe` ["one", ""]
            ircParams " one" `shouldBe` ["", "one"]
            ircParams ":" `shouldBe` [""]
            ircParams "" `shouldBe` []
        it "parses IRC messages" $ do
            parseMessage "one two three four" `shouldBe` Message Nothing "one" ["two", "three", "four"]
            parseMessage "one two :three four" `shouldBe` Message Nothing "one" ["two", "three four"]
            parseMessage ":one two three four"
                `shouldBe` Message (Just (StringPrefix "one")) "two" ["three", "four"]
            parseMessage ":one two :three four"
                `shouldBe` Message (Just (StringPrefix "one")) "two" ["three four"]
            -- TODO: the following tests fail
            parseMessage ":one :two three four"
                `shouldBe` Message (Just (StringPrefix "one")) ":two" ["three", "four"]
            parseMessage ": two three four"
                `shouldBe` Message (Just (StringPrefix "")) "two" ["three", "four"]
            parseMessage ": :two :three four"
                `shouldBe` Message (Just (StringPrefix "")) ":two" ["three four"]
            -- these ones should not fail hopefully
            parseMessage ":one two" `shouldBe` Message (Just (StringPrefix "one")) "two" []
            parseMessage "two" `shouldBe` Message Nothing "two" []
            parseMessage ":one" `shouldBe` Message (Just (StringPrefix "one")) "" []
            parseMessage "" `shouldBe` Message Nothing "" []
