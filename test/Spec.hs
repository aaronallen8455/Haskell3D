import Test.Hspec
import Lib

main :: IO ()
main = hspec $ do
  describe "Plane and line intersection" $ do
    it "should find intersection with origin plane" $ do
      let pl1 = Plane (Point 0 0 0) (Point 0 1 0)
          ln1 = Line (Point 1 1 1) (Point 1 1 1)
      planeVectorIntersect pl1 ln1 `shouldBe` Just (Point 0 0 0)
    it "should fail if the line doesn't intersect" $ do
      let pl = Plane (Point 0 0 0) (Point 0 1 0)
          ln = Line (Point 1 1 1) (Point 1 0 1)
      planeVectorIntersect pl ln `shouldBe` Nothing
  describe "Distance" $ do
    it "should find the diagonal of a unit cube" $ do
      distance (Point 0 0 0) (Point 1 1 1) / 100 * 100 `shouldBe` sqrt 3 / 100 * 100
  describe "Rotate" $ do
    it "should rotate 90deg on y-axis" $ do
      rotate (Point 0 0 1) (Point 0 0 0) 0 0 0 (pi / 2) `shouldBe` Point (-1) 0 0