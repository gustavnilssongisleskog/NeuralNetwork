{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Utils where

mapTuple :: (a -> b) -> (a, a) -> (b, b)
mapTuple f (a, b) = (f a, f b)

sigmoid :: Double -> Double
sigmoid = (1/) . (1+) . exp . negate

invSigmoid :: Double -> Double
invSigmoid = negate . log . subtract 1 . (1/)

dSigmoid :: Double -> Double
dSigmoid = (1/) . (2+) . sum . take 2 . iterate (1/) . exp

partitionNbrPieces :: Int -> [a] -> [[a]]
partitionNbrPieces pieces xs = partitionPieceLength (length xs `div` pieces) xs

partitionPieceLength :: Int -> [a] -> [[a]]
partitionPieceLength _ [] = []
partitionPieceLength n xs = take n xs : partitionPieceLength n (drop n xs)