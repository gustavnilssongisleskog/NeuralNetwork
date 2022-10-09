module Training where
import Mat (Matrix)
import Gradient (combinedGradients, gradientManyExamples, negateGradient)
import Output (outputManyExamples)


trainOneBatch :: [Matrix] -> [Int] -> [Matrix] -> [Matrix] -> ([Matrix], [Matrix])
trainOneBatch imgs corrects weights biases = combinedGradients (weights,biases) (negateGradient $ gradientManyExamples imgs corrects weights biases)

evaluateNetwork :: [Matrix] -> [Int] -> [Matrix] -> [Matrix] -> Double
evaluateNetwork imgs corrects weights biases = (*100) $ (/ fromIntegral (length imgs)) $ sum $ map (\b -> if b then 1 else 0) $ zipWith (==) corrects (outputManyExamples imgs weights biases)

trainManyBatches :: [[Matrix]] -> [[Int]] -> [Matrix] -> [Matrix] ->([Matrix], [Matrix])
trainManyBatches (imgs:imgss) (corrects:correctss) weights biases = trainManyBatches imgss correctss nextW nextB where
    (nextW, nextB) = trainOneBatch imgs corrects weights biases
trainManyBatches _ _ weights biases = (weights, biases)