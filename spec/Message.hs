import Test.Hspec
import IRC.Hostmask
import IRC.Prefix
import IRC.Message

main :: IO ()
main = hspec $ do
    describe "IRC.Hostmask" $ do
        it "can be represented as a string" $ do
            show (Hostmask "nick" "user" "host")    `shouldBe` "nick!user@host"
            show (Hostmask "nick" "user" "")        `shouldBe` "nick!user@"
            show (Hostmask "nick" "" "")            `shouldBe` "nick!@"
            show (Hostmask "" "" "")                `shouldBe` "!@"
    describe "IRC.Prefix" $ do
        it "can be represented as a string" $ do
            show (StringPrefix "prefix") `shouldBe` "prefix"
            show (StringPrefix "") `shouldBe` ""
            show (MaskPrefix (Hostmask "nick" "user" "host")) `shouldBe` "nick!user@host"
            show (MaskPrefix (Hostmask "" "" "")) `shouldBe` "!@"
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
            parseMessage "one two three four" `shouldBe` Message Nothing "ONE" ["two", "three", "four"]
            parseMessage "one two :three four" `shouldBe` Message Nothing "ONE" ["two", "three four"]
            parseMessage ":one two three four"
                `shouldBe` Message (Just (StringPrefix "one")) "TWO" ["three", "four"]
            parseMessage ":one two :three four"
                `shouldBe` Message (Just (StringPrefix "one")) "TWO" ["three four"]
            parseMessage ":one :two three four"
                `shouldBe` Message (Just (StringPrefix "one")) "TWO" ["three", "four"]
            parseMessage ": two three four" `shouldBe` Message Nothing "TWO" ["three", "four"]
            parseMessage ": :two :three four" `shouldBe` Message Nothing "TWO" ["three four"]
            parseMessage ":one two" `shouldBe` Message (Just (StringPrefix "one")) "TWO" []
            parseMessage "two" `shouldBe` Message Nothing "TWO" []
            parseMessage ":one" `shouldBe` Message (Just (StringPrefix "one")) "" []
            parseMessage "" `shouldBe` Message Nothing "" []
            parseMessage ":" `shouldBe` Message Nothing "" []
            parseMessage " " `shouldBe` Message Nothing "" []
            parseMessage "   " `shouldBe` Message Nothing "" []
            parseMessage ":   " `shouldBe` Message Nothing "" []
