import Test.Hspec
import IRC.Message

main :: IO ()
main = hspec $ do
    describe "ircParams" $ do
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
