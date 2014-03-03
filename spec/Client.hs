import Test.Hspec
import IRC.Server.Client

main :: IO ()
main = hspec $ do
    describe "IRC.Server.Client" $ do
        it "can be registered" $ do
            isClientRegistered (Client Nothing Nothing Nothing Nothing Nothing Nothing) `shouldBe` False
            isClientRegistered (Client Nothing Nothing (Just "nick") Nothing Nothing Nothing) `shouldBe` False
            isClientRegistered (Client Nothing Nothing Nothing (Just "user") Nothing Nothing) `shouldBe` False
            isClientRegistered (Client Nothing Nothing Nothing Nothing (Just "real") Nothing) `shouldBe` False
            isClientRegistered (Client Nothing Nothing (Just "nick") (Just "user") Nothing Nothing) `shouldBe` False
            isClientRegistered (Client Nothing Nothing (Just "nick") Nothing (Just "real") Nothing) `shouldBe` False
            isClientRegistered (Client Nothing Nothing Nothing (Just "user") (Just "real") Nothing) `shouldBe` False
            isClientRegistered (Client Nothing Nothing (Just "nick") (Just "user") (Just "real") Nothing) `shouldBe` True
