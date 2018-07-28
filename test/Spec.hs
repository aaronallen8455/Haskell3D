import Test.Hspec
import Lib
import Primitives
import Debug.Trace

main :: IO ()
main = hspec $ do
  describe "Distance" $ do
    it "should find the diagonal of a unit cube" $ do
      distance (Coord 0 0 0) (Coord 1 1 1) / 100 * 100 `shouldBe` sqrt 3 / 100 * 100
  describe "PointCircle" $ do
    let (Just pts) = circlePoints 1 6
    it "should have the correct number of points" $ do
      length pts `shouldBe` 6
  describe "Circle" $ do
    let (Just c) = circle 1 6
    it "should have the correct number of vertices" $ do
      length c `shouldBe` 6
  --describe "Sphere" $ do
  --  let (Just s) = sphere 2 6 4
  --  it "should have the correct number of vertices" $ do
  --    length s `shouldBe` 26
    --it "should be centered correctly" $ do
    --  fmap ((*100) . (/100)) (getCenter s) `shouldBe` Coord 0 1 0