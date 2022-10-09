module Gradient where
import Mat (Matrix, Vector, matAdd, matMap, transpose, vectorToMatrix, matMul, matrixToVector)
import Output (zAndActivations)
import Utils (dSigmoid, mapTuple)
import Control.DeepSeq(deepseq)
import GHC.Conc(par, pseq)

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