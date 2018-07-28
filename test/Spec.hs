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
    --trace (show c) return ()
    it "should have the correct number of vertices" $ do
      length c `shouldBe` 6
  describe "Unit Cube" $ do
    let uc = meshFromEdges [
                            (Point 0 0 0, 0, [1,3,4])
                           ,(Point 0 0 1, 1, [0,2,5])
                           ,(Point 1 0 1, 2, [1,3,6])
                           ,(Point 1 0 0, 3, [0,2,7])
                           ,(Point 0 1 0, 4, [5,7,0])
                           ,(Point 0 1 1, 5, [4,6,1])
                           ,(Point 1 1 1, 6, [5,7,2])
                           ,(Point 1 1 0, 7, [6,4,3])]
    it "should have the correct number of vertices" $ do
      length uc `shouldBe` 8
  --describe "Sphere" $ do
  --  let (Just s) = sphere 2 4 2
  --  trace (show s) return ()
  --  it "should have the correct number of vertices" $ do
  --    length s `shouldBe` 10
    --it "should be centered correctly" $ do
    --  fmap ((*100) . (/100)) (getCenter s) `shouldBe` Coord 0 1 0