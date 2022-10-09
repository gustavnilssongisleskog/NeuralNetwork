module Gradient where
import Mat (Matrix, Vector, matAdd, matMap, transpose, vectorToMatrix, matMul, matrixToVector)
import Output (zAndActivations)
import Utils (dSigmoid, mapTuple)
import Control.DeepSeq(deepseq)
import GHC.Conc(par, pseq)

{-oldorGradientOneExample ::  Matrix -> Int -> [Matrix] -> [Matrix] -> ([Matrix], [Matrix])
oldGradientOneExample inp correct weights biases = (dCostdWList, dCostdBList) where

    w :: Int -> Int -> Int -> Double
    w l j k = weights !! (l - 1) !! j !! k
        --b l j = head $ biases !! (l - 1) !! j

    aList :: [Vector]
    zList :: [Vector]
    (zList, aList) = unzip $ zAndActivations inp weights biases
    z :: Int -> Int -> Double
    z l j = zList !! l !! j
    a :: Int -> Int -> Double
    a l j = aList !! l !! j

    numLayers :: Int
    numLayers = length aList

    layersize :: Int -> Int
    layersize = length . (aList !!)


    dZdW :: Int -> Int -> Int -> Int -> Int -> Double
    dZdW zl zj wl wj wk
        | zl == wl && zj /= wj = 0
        | zl < wl = 0
        | zl == wl && zj == wj = a (zl - 1) wk
        | otherwise = sum $ map (\m -> w zl zj m * dAdWGet (zl - 1) m wl wj wk) [0..layersize (zl - 1) - 1]

    dZdWList :: [[[Matrix]]]
    dZdWList = [[[[[dZdW zl zj wl wj wk | wk <- [0..layersize (wl - 1) - 1]] | wj <- [0..layersize wl - 1]] | wl <- [1..numLayers - 1]] | zj <- [0..layersize zl - 1]] | zl <- [1..numLayers - 1]]

    dZdWGet :: Int -> Int -> Int -> Int -> Int -> Double
    dZdWGet zl zj wl wj wk = dZdWList !! (zl - 1) !! zj !! (wl - 1) !! wj !! wk


    dAdW :: Int -> Int -> Int -> Int -> Int -> Double
    dAdW al aj wl wj wk
        | al == 0 = 0
        | otherwise = dSigmoid (z al aj) * dZdWGet al aj wl wj wk

    dAdWList :: [[[Matrix]]]
    dAdWList = [[[[[dAdW al aj wl wj wk | wk <- [0..layersize (wl - 1) - 1]] | wj <- [0..layersize wl - 1]] | wl <- [1..numLayers - 1]] | aj <- [0..layersize al - 1]] | al <- [1..numLayers - 1]]

    dAdWGet :: Int -> Int -> Int -> Int -> Int -> Double
    dAdWGet al aj wl wj wk = dAdWList !! (al - 1) !! aj !! (wl - 1) !! wj !! wk


    dZdB :: Int -> Int -> Int -> Int -> Double
    dZdB zl zj bl bj
        | zl < bl = 0
        | zl == bl && zj /= bj = 0
        | zl == bl && zj == bj = 1
        | otherwise = sum $ map (\m -> w zl zj m * dAdBGet (zl - 1) m bl bj) [0..layersize (zl - 1) - 1]

    dZdBList :: [[Matrix]]
    dZdBList = [[[[dZdB zl zj bl bj | bj <- [0..layersize bl - 1]] | bl <- [1..numLayers - 1]] | zj <- [0..layersize zl - 1]] | zl <- [1..numLayers - 1]]

    dZdBGet :: Int -> Int -> Int -> Int -> Double
    dZdBGet zl zj bl bj = dZdBList !! (zl - 1) !! zj !! (bl - 1) !! bj


    dAdB :: Int -> Int -> Int -> Int -> Double
    dAdB al aj bl bj
        | al == 0 = 0
        | otherwise = dSigmoid (z al aj) * dZdBGet al aj bl bj

    dAdBList :: [[Matrix]]
    dAdBList = [[[[dAdB al aj bl bj | bj <- [0..layersize bl - 1]] | bl <- [1..numLayers - 1]] | aj <- [0..layersize al - 1]] | al <- [1..numLayers - 1]]

    dAdBGet :: Int -> Int -> Int -> Int -> Double
    dAdBGet al aj bl bj = dAdBList !! (al - 1) !! aj !! (bl - 1) !! bj


    y :: Int -> Double
    y j = if j == correct then 1 else 0

    dCdW :: Int -> Int -> Int -> Int -> Double
    dCdW cj wl wj wk = 2 * (a (numLayers - 1) cj - y cj) * dAdWGet (numLayers - 1) cj wl wj wk

    dCostdW :: Int -> Int -> Int -> Double
    dCostdW wl wj wk = sum [dCdW cj wl wj wk | cj <-[0..layersize (numLayers - 1) - 1]]

    dCostdWList :: [Matrix]
    dCostdWList = [[[dCostdW wl wj wk | wk <- [0..layersize (wl - 1) - 1]] | wj <- [0..layersize wl - 1]] | wl <- [1..numLayers - 1]]


    dCdB :: Int -> Int -> Int -> Double
    dCdB cj bl bj = 2 * (a (numLayers - 1) cj - y cj) * dAdBGet (numLayers - 1) cj bl bj

    dCostdB :: Int -> Int -> Double
    dCostdB bl bj = sum [dCdB cj bl bj | cj <- [0..layersize (numLayers - 1) - 1]]

    dCostdBList :: [Matrix]
    dCostdBList = [[[dCostdB bl bj] | bj <- [0..layersize bl - 1]] | bl <- [1..numLayers - 1]]

oldgradientOneExample ::  Matrix -> Int -> [Matrix] -> [Matrix] -> ([Matrix], [Matrix])
oldgradientOneExample inp correct weights biases = (dCostdWList, dCostdBList) where

    w :: Int -> Int -> Int -> Double
    w l j k = weights !! (l - 1) !! j !! k
        --b l j = head $ biases !! (l - 1) !! j

    aList :: [Vector]
    zList :: [Vector]
    (zList, aList) = unzip $ zAndActivations inp weights biases
    z :: Int -> Int -> Double
    z l j = zList !! l !! j
    a :: Int -> Int -> Double
    a l j = aList !! l !! j

    numLayers :: Int
    numLayers = length aList

    layersize :: Int -> Int
    layersize = length . (aList !!)

    y :: Int -> Double
    y j = if j == correct then 1 else 0

    dCostdA :: Int -> Int -> Double
    dCostdA al ak
        | al == numLayers - 1 = 2 * (a al ak - y ak)
        | otherwise = sum [dCostdAGet (al + 1) aj * dSigmoid (z (al + 1) aj) * w (al + 1) aj ak | aj <- [0..layersize (al + 1) - 1]]
        -- | otherwise = sum $ map (\aj -> dCostdAGet (al + 1) aj * dSigmoid (z (al + 1) aj) * w (al + 1) aj ak) [0..layersize (al + 1) - 1]


    dCostdAList :: Matrix
    dCostdAList = [[dCostdA al ak | ak <- [0..layersize al - 1]] | al <- [1..numLayers - 1]]
    --dCostdAList = [                                   | al <- [1..numLayers - 1]]

    dCostdAGet :: Int -> Int -> Double
    dCostdAGet al ak = dCostdAList !! (al - 1) !! ak

    dCostdW :: Int -> Int -> Int -> Double
    dCostdW wl wj wk = dCostdAGet wl wj * dSigmoid (z wl wj) * a (wl - 1) wk

    dCostdWList :: [Matrix]
    dCostdWList = [[[dCostdW wl wj wk | wk <- [0..layersize (wl - 1) - 1]] | wj <- [0..layersize wl - 1]] | wl <- [1..numLayers - 1]]


    dCostdB :: Int -> Int -> Double
    dCostdB bl bj = dCostdAGet bl bj * dSigmoid (z bl bj)
    --dCostdB = curry $ product . zipWith ($) [uncurry dCostdAGet, uncurry ((dSigmoid .) . z)] . repeat

    dCostdBList :: [Matrix]
    dCostdBList = [[[dCostdB bl bj] | bj <- [0..layersize bl - 1]] | bl <- [1..numLayers - 1]]

-}

gradientManyExamples' :: [Matrix] -> [Int] -> [Matrix] -> [Matrix] -> [([Matrix], [Matrix])]
gradientManyExamples' (inp:inps) (correct:corrects) weights biases = grad `deepseq` (grad:grads) where--               deepseq grad `par` (deepseq grads `pseq` grad:grads) where
    grad = gradientOneExample inp correct weights biases
    grads = gradientManyExamples' inps corrects weights biases
gradientManyExamples' _ _ _ _ = []

combinedGradients :: ([Matrix], [Matrix]) -> ([Matrix], [Matrix]) -> ([Matrix], [Matrix])
combinedGradients (w1,b1) (w2,b2) = (zipWith matAdd w1 w2, zipWith matAdd b1 b2)

gradientManyExamples :: [Matrix] -> [Int] -> [Matrix] -> [Matrix] -> ([Matrix], [Matrix])
gradientManyExamples inps corrects weights biases = mulGradientByScalar (1 / fromIntegral (length inps)) $ foldr1 combinedGradients $ gradientManyExamples' inps corrects weights biases

mapGradient :: (Double -> Double) -> ([Matrix], [Matrix]) -> ([Matrix], [Matrix])
mapGradient = mapTuple . map . matMap

negateGradient :: ([Matrix], [Matrix]) -> ([Matrix], [Matrix])
negateGradient = mapGradient negate

mulGradientByScalar :: Double -> ([Matrix], [Matrix]) -> ([Matrix], [Matrix])
mulGradientByScalar = mapGradient . (*)

gradientDistanceFromOrigin :: ([Matrix], [Matrix]) -> Double
gradientDistanceFromOrigin = sqrt . uncurry (+) . mapTuple (sum . map (sum . map (sum . map (^2))))

normGradient :: ([Matrix], [Matrix]) -> ([Matrix], [Matrix])
normGradient grad = mulGradientByScalar (1 / gradientDistanceFromOrigin grad) grad




gradientOneExample ::  Matrix -> Int -> [Matrix] -> [Matrix] -> ([Matrix], [Matrix])
gradientOneExample inp correct weights biases = (dCostdWList, dCostdBList) where

    aList :: [Vector]
    zList :: [Vector]
    (zList, aList) = unzip $ zAndActivations inp weights biases
    a :: Int -> Int -> Double
    a l j = aList !! l !! j

    numLayers :: Int
    numLayers = length aList

    layersize :: Int -> Int
    layersize = length . (aList !!)

    y :: Int -> Double
    y j = if j == correct then 1 else 0

    dCostdA :: Int -> Vector
    dCostdA al
        | al == numLayers - 1 = [2 * (a al ak - y ak) | ak <- [0..layersize al - 1]]
        | otherwise = matrixToVector $ transpose (weights !! al) `matMul` vectorToMatrix (zipWith (*) (dCostdAGet (al + 1)) (map dSigmoid (zList !! (al + 1))))

    dCostdAList :: Matrix
    dCostdAList = [dCostdA al | al <- [1..numLayers - 1]]

    dCostdAGet :: Int -> Vector
    dCostdAGet al = dCostdAList !! (al - 1)

    dCostdWList = [vectorToMatrix (zipWith (*) (dCostdAGet l) (map dSigmoid (zList !! l))) `matMul` [aList !! (l - 1)] | l <- [1..numLayers - 1]]
    dCostdBList = [vectorToMatrix (zipWith (*) (dCostdAGet l) (map dSigmoid (zList !! l))) | l <- [1..numLayers - 1]]