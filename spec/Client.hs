import Test.Hspec
import IRC.Server.Client

main :: IO ()
main = hspec $ do
    describe "IRC.Server.Client" $ do
        it "can be registered" $ do
            isClientRegistered defaultClient `shouldBe` False
            isClientRegistered defaultClient {nick=Just "nick"} `shouldBe` False
            isClientRegistered defaultClient {user=Just "user"} `shouldBe` False
            isClientRegistered defaultClient {realName=Just "real"} `shouldBe` False
            isClientRegistered defaultClient {nick=Just "nick", user=Just "user"} `shouldBe` False
            isClientRegistered defaultClient {nick=Just "nick", realName=Just "real"} `shouldBe` False
            isClientRegistered defaultClient {user=Just "user", realName=Just "real"} `shouldBe` False
            isClientRegistered defaultClient {nick=Just "nick", user=Just "user", realName=Just "real"} `shouldBe` True
