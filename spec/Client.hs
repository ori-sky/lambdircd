import Test.Hspec
import IRC.Server.Client

main :: IO ()
main = hspec $ do
    describe "IRC.Server.Client" $ do
        it "can be registered" $ do
            isClientReady defaultClient `shouldBe` False
            isClientReady defaultClient {nick=Just "nick"} `shouldBe` False
            isClientReady defaultClient {user=Just "user"} `shouldBe` False
            isClientReady defaultClient {realName=Just "real"} `shouldBe` False
            isClientReady defaultClient {nick=Just "nick", user=Just "user"} `shouldBe` False
            isClientReady defaultClient {nick=Just "nick", realName=Just "real"} `shouldBe` False
            isClientReady defaultClient {user=Just "user", realName=Just "real"} `shouldBe` False
            isClientReady defaultClient {nick=Just "nick", user=Just "user", realName=Just "real"} `shouldBe` True
