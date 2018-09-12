{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module VectorZipper 
  ( fromList
  , toList
  , lifeStep
  , lifeAnimation
  , getCells
  , ZZZ
  , VectorZipper3D(..)
  , fromList'
  , toList'
  , lifeStep'
  , lifeStep''
  , getCells'
  , Indexable(..)
  , getCount
  , shift' 
  , getCells'' ) where

import           Control.Comonad
import           Control.Comonad.Trans.Class
import           Control.Monad               (guard, join)
import           Control.Parallel.Strategies
import           Data.Maybe                  (catMaybes, fromJust, isJust)
import qualified Data.Vector                 as V
import qualified Data.Array                  as A

class Indexable m i where
  (!) :: m a -> i -> a

data VectorZipper a = VectorZipper { vector :: V.Vector a, index :: Int } deriving (Show, Functor)

shift :: Int -> VectorZipper a -> VectorZipper a
shift i z@VectorZipper{..} = z { index = i' } where
  i' = (index + i) `mod` V.length vector

instance Indexable VectorZipper Int where
  VectorZipper{..} ! i = vector V.! i' where
    i' = (index + i) `mod` V.length vector

instance Comonad VectorZipper where
  extract = (! (0 :: Int))
  extend f z@VectorZipper{..} = z { vector = vector' } where
    vector' = V.imap (\i _ -> f $ shift i z) vector

-- | Comonad transformer version
newtype VectorZipperT w a = VectorZipperT { runZipperT :: w (VectorZipper a) } deriving Functor

shiftT :: Functor w => Int -> VectorZipperT w a -> VectorZipperT w a
shiftT i = VectorZipperT . fmap (shift i) . runZipperT

instance Comonad w => Indexable (VectorZipperT w) Int where
  z ! i = vec V.! i' where
    (VectorZipper vec ind) = extract $ runZipperT z
    i' = (i + ind) `mod` V.length vec

instance Comonad w => Comonad (VectorZipperT w) where
  extract = extract . extract . runZipperT

  extend f = VectorZipperT . extend go . runZipperT where
    f' = f . VectorZipperT
    go wz = VectorZipper vs' i where
      (VectorZipper vs i) = extract wz
      vs' = f' <$> V.imap (\i _ -> fmap (shift i) wz) vs

instance ComonadTrans VectorZipperT where
  lower = fmap extract . runZipperT

type ZZZ a = VectorZipperT (VectorZipperT VectorZipper) a

instance Indexable (VectorZipperT (VectorZipperT VectorZipper)) (Int, Int, Int) where
  zzz ! (x, y, z) = extract . extract . extract . shift z . runZipperT . shiftT y . runZipperT $ shiftT x zzz

fromList :: [[[a]]] -> ZZZ a
fromList = VectorZipperT . VectorZipperT . flip VectorZipper 0 . V.fromList
         . map (flip VectorZipper 0 . V.fromList)
         . map (map (flip VectorZipper 0 . V.fromList))


test :: ZZZ a -> VectorZipperT VectorZipper a
test = lower

test2 :: ZZZ a -> VectorZipperT VectorZipper (VectorZipper a)
test2 = runZipperT

test3 :: ZZZ a -> VectorZipper (VectorZipper (VectorZipper a))
test3 = runZipperT . runZipperT

toList :: ZZZ a -> [[[a]]]
toList = V.toList . vector . fmap (V.toList . vector . fmap (V.toList . vector))
       . runZipperT . runZipperT

-- using 5766 rule
conway :: ZZZ Bool -> Bool
conway zzz = case count of
  5 -> extract zzz
  7 -> extract zzz
  6 -> True
  _ -> False
  where
    vals = withStrategy (parList rdeepseq) $ do
      x <- [-1..1]
      y <- [-1..1]
      z <- [-1..1]
      let c = (x, y, z) :: (Int, Int, Int)
      guard $ c /= (0, 0, 0)
      return $ zzz ! c
    count = length $ filter id vals


lifeStep :: ZZZ Bool -> ZZZ Bool
lifeStep = extend conway

lifeAnimation :: ZZZ Bool -> [ZZZ Bool]
lifeAnimation = iterate lifeStep

-- | Get a list of the living cell coordinates.
getCells :: ZZZ Bool -> [(Int, Int, Int)]
getCells = V.toList . join
         . V.imap f . vector . runZipperT . runZipperT where
  f x = join . V.imap (g x) . vector
  g x y = V.map fromJust . V.filter isJust . V.imap (h x y) . vector
  h x y z b = if b then Just (x, y, z) else Nothing

-- | Will a flattened representation be faster? YES
data VectorZipper3D a = VectorZipper3D
  { xDim :: Int
  , yDim :: Int
  , zDim :: Int
  , vect :: V.Vector a
  , ind  :: (Int, Int, Int)} deriving Functor

shift' :: Int -> Int -> Int -> VectorZipper3D a -> VectorZipper3D a
shift' x y z vz@VectorZipper3D{..} = vz{ind = (x', y', z')} where
  (oldX, oldY, oldZ) = ind
  x' = (oldX + x) `mod` xDim
  y' = (oldY + y) `mod` yDim
  z' = (oldZ + z) `mod` zDim

instance Indexable VectorZipper3D (Int, Int, Int) where
  VectorZipper3D{..} ! (x, y, z) = vect V.! (x' + y' * xDim + z' * xDim * yDim) where
    (ix, iy, iz) = ind
    x' = (ix + x) `mod` xDim
    y' = (iy + y) `mod` yDim
    z' = (iz + z) `mod` zDim

instance Comonad VectorZipper3D where
  extract z = z ! (0::Int,0::Int,0::Int)
  extend f vz@VectorZipper3D{..} = vz{vect = vect'} where
    vect' = V.imap g vect
    g i _ = f $ shift' x y z vz where
      (z, yr) = quotRem i (xDim * yDim)
      (y, x) = quotRem yr xDim

getCells' :: VectorZipper3D Bool -> [(Int, Int, Int)]
getCells' vz@VectorZipper3D{..} = [(x, y, z) | z <- [0..zDim-1], y <- [0..yDim-1], x <- [0..xDim-1], vz ! (x,y,z)]

fromList' :: [[[a]]] -> VectorZipper3D a
fromList' vs = VectorZipper3D x y z v (0, 0, 0) where
  z = length vs
  y = length $ head vs
  x = length . head $ head vs
  v = V.fromList . concat $ concat vs

toList' :: VectorZipper3D a -> [[[a]]]
toList' vz@VectorZipper3D{..} =
  (map . map . map) (vz !) [[[(x, y, z) | x <- [0..xDim-1]] | y <- [0..yDim-1]] | z <- [0..zDim-1]]

getCount :: VectorZipper3D Bool -> VectorZipper3D Int
getCount = extend go where
  go :: VectorZipper3D Bool -> Int
  go vz = length . filter id $ map (vz !) [(x, y, z) :: (Int, Int, Int) | x <- [-1..1], y <- [-1..1], z <- [-1..1], (x,y,z) /= (0,0,0)]

conway' :: VectorZipper3D Bool -> Bool
conway' vz = case count of
  5 -> extract vz
  7 -> extract vz
  6 -> True
  _ -> False
  where
  count = length . filter id $ map (vz !) [(x, y, z) :: (Int, Int, Int) | x <- [-1..1], y <- [-1..1], z <- [-1..1], (x,y,z) /= (0,0,0)]

lifeStep' :: VectorZipper3D Bool -> VectorZipper3D Bool
lifeStep' = extend conway'

-- | Better to not use Comonad at all?
lifeStep'' :: A.Array (Int, Int, Int) Bool -> A.Array (Int, Int, Int) Bool
lifeStep'' a = A.listArray bnds $ map go (A.indices a) where
  bnds@(_, (maxX, maxY, maxZ)) = A.bounds a
  go i@(ix, iy, iz) = case count of
    5 -> a A.! i
    7 -> a A.! i
    6 -> True
    _ -> False
    where
      count = length $ filter id coords
      coords = do
        dx <- [-1..1]
        dy <- [-1..1]
        dz <- [-1..1]
        let x = (ix + dx) `mod` (maxX + 1)
            y = (iy + dy) `mod` (maxY + 1)
            z = (iz + dz) `mod` (maxZ + 1)
        guard . not $ x == ix && y == iy && z == iz
        return $ a A.! (x, y, z)

getCells'' :: A.Array (Int, Int, Int) Bool -> [(Int, Int, Int)]
getCells'' a = filter (a A.!) $ A.indices a