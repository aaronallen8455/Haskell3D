import Test.Hspec
import Lib
import Primitives
import VectorZipper
import Debug.Trace
import qualified Data.Set as S
import qualified Data.Vector as V
import Control.Comonad

main :: IO ()
main = hspec $ do
  describe "Distance" $ do
    it "should find the diagonal of a unit cube" $ do
      distance (coord 0 0 0) (coord 1 1 1) / 100 * 100 `shouldBe` sqrt 3 / 100 * 100

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
    let uc = meshFromEdges [(coord 0 0 0, 0, [1,3,4])
                           ,(coord 0 0 1, 1, [0,2,5])
                           ,(coord 1 0 1, 2, [1,3,6])
                           ,(coord 1 0 0, 3, [0,2,7])
                           ,(coord 0 1 0, 4, [5,7,0])
                           ,(coord 0 1 1, 5, [4,6,1])
                           ,(coord 1 1 1, 6, [5,7,2])
                           ,(coord 1 1 0, 7, [6,4,3])]
        (Mesh v t) = uc
    it "should have the correct number of vertices" $ do
      length uc `shouldBe` 8
    it "should find the center" $ do
      getCenter uc `shouldBe` coord 0.5 0.5 0.5

  describe "Sphere" $ do
    let (Just s) = sphere 1 6 4
        (Mesh v t) = s
    --trace (show t) return ()
    it "should have the correct number of vertices" $ do
      length s `shouldBe` 26
    it "should be centered correctly" $ do
      getCenter s `shouldBe` coord 0 1 0

  describe "VectorZipper" $ do
    let initConway = fromList' origLst
        origLst = [[[S.member (x, y, z) glider | x <- [0..7]] | y <- [0..14]] | z <- [0..14] ]
        glider :: S.Set (Int, Int, Int)
        glider = S.fromList [
            (0,0,0), (1,0,0),
            (0,1,0), (1,1,0),
            (0,2,0), (1,2,0),
            (0,2,1), (1,2,1),
            (0,1,2), (1,1,2),

            (3,4,3), (3,5,3),
            (3,4,4), (3,5,4),
            (3,4,5), (3,5,5),
            (4,4,5), (4,5,5),
            (5,4,4), (5,5,4)
          ]
        lst = toList' initConway
    it "internal vector should match flattened list" $ do
      V.toList (vect initConway) `shouldBe` (concat $ concat origLst)
    it "should convert to and from list correctly" $ do
      lst `shouldBe` origLst
    it "should index correctly" $ sequence_ . map (flip shouldBe True) . map (initConway !) $ S.toList glider
    it "should index correctly2" $ do
      (univ ! ((0,1,0)::(Int,Int,Int))) `shouldBe` True
    it "should have the right index after shifting" $ do
      (ind shift4) `shouldBe` (0,1,0) 
      (ind shift6) `shouldBe` (0,0,1)
    it "should shift correctly" $ do
      (extract shift1) `shouldBe` True
      (extract shift2) `shouldBe` True
      (extract shift3) `shouldBe` True
      (extract shift4) `shouldBe` (univ ! ind shift4)
      (extract shift5) `shouldBe` True
      (extract shift6) `shouldBe` False
  describe "Conways Game of Life" $ do
    it "should have correct initial state" $ do
      (univ ! ((1,1,1)::(Int,Int,Int))) `shouldBe` False
      (univ ! ((2,1,0)::(Int,Int,Int))) `shouldBe` True
    it "should get correct neighborhood count" $ do
      (toList' counts) `shouldBe` [[[5, 5, 5], [5,5,5], [6,6,6]], [[6,6,6],[6,6,6],[6,6,6]], [[6,6,6],[6,6,6],[6,6,6]]]
    it "should apply rule correctly" $ do
      (stepped ! ((1,1,1)::(Int,Int,Int))) `shouldBe` True
      (stepped ! ((2,1,0)::(Int,Int,Int))) `shouldBe` True
      (stepped ! ((2,0,0)::(Int,Int,Int))) `shouldBe` True

univ = fromList' [[[True, True, True], [True,True,True], [False,False,False]], [[False,False,False],[False,False,False],[False,False,False]], [[False,False,False],[False,False,False],[False,False,False]]]
stepped = lifeStep' univ
counts = getCount univ
shift1 = shift' 1 0 0 univ
shift2 = shift' 2 0 0 univ
shift3 = shift' 3 0 0 univ
shift4 = shift' 0 1 0 univ
shift5 = shift' 1 1 0 univ
shift6 = shift' 0 0 1 univ
