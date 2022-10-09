{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Output where
import Mat
    ( Matrix, matMul, matAdd, matrixSigmoid, matrixToVector, Vector )
import Utils (mapTuple)

zAndActivations' :: Matrix -> Matrix -> [Matrix] -> [Matrix] -> [(Matrix, Matrix)]
zAndActivations' z inp (w:weights) (b:biases) = (z, inp) : zAndActivations' new_z (matrixSigmoid new_z) weights biases where
    new_z = matAdd (matMul w inp) b
zAndActivations' z inp [] [] = [(z,inp)]
zAndActivations' _ _ _ _ = []

zAndActivations :: Matrix -> [Matrix] -> [Matrix] -> [(Vector, Vector)]
zAndActivations inp weights biases = map (mapTuple matrixToVector) $ zAndActivations' [] inp weights biases

activations :: Matrix -> [Matrix] -> [Matrix] -> [Vector]
activations inp weights biases = map snd $ zAndActivations inp weights biases

output :: Matrix -> [Matrix] -> [Matrix] -> Int
output inp weights biases = snd $ maximum $ zip (last $ activations inp weights biases) [0..9]

outputManyExamples :: [Matrix] -> [Matrix] -> [Matrix] -> [Int]
outputManyExamples (inp:inps) weights biases = output inp weights biases : outputManyExamples inps weights biases
outputManyExamples _ _ _ = []

cost :: Matrix -> Int -> [Matrix] -> [Matrix] -> Double
cost inp correct weights biases = sum $ zipWith (((^2) .) . (-)) [if j == correct then 1 else 0 | j <- [0..9]] (last $ activations inp weights biases)

costManyExamples :: [Matrix] -> [Int] -> [Matrix] -> [Matrix] -> Double
costManyExamples inps corrects weights biases = sum (zipWith (\inp correct -> cost inp correct weights biases) inps corrects) / fromIntegral (length inps)