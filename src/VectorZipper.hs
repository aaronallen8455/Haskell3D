{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module VectorZipper (fromList, toList, lifeStep, lifeAnimation, getCells, ZZZ) where

import Control.Monad (guard, join)
import Control.Comonad
import Control.Comonad.Trans.Class
import qualified Data.Vector as V
import Data.Maybe (catMaybes, isJust, fromJust)
import           Control.Parallel.Strategies

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