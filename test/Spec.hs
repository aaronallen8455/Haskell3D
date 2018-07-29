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
    let uc = meshFromEdges [(Coord 0 0 0, 0, [1,3,4])
                           ,(Coord 0 0 1, 1, [0,2,5])
                           ,(Coord 1 0 1, 2, [1,3,6])
                           ,(Coord 1 0 0, 3, [0,2,7])
                           ,(Coord 0 1 0, 4, [5,7,0])
                           ,(Coord 0 1 1, 5, [4,6,1])
                           ,(Coord 1 1 1, 6, [5,7,2])
                           ,(Coord 1 1 0, 7, [6,4,3])]
        (Mesh v t) = uc
    it "should have the correct number of vertices" $ do
      length uc `shouldBe` 8
    it "should find the center" $ do
      getCenter uc `shouldBe` Coord 0.5 0.5 0.5
  describe "Sphere" $ do
    let (Just s) = sphere 1 6 4
        (Mesh v t) = s
    trace (show t) return ()
    it "should have the correct number of vertices" $ do
      length s `shouldBe` 26
    it "should be centered correctly" $ do
      getCenter s `shouldBe` Coord 0 1 0