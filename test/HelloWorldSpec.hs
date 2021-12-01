module HelloWorldSpec where
import           Test.Tasty.Hspec

spec_prelude :: Spec
spec_prelude = describe "Prelude.head" $ do
  it "returns the first element of a list" $ do
    head [23 ..] `shouldBe` (23 :: Int)
