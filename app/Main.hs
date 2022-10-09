module Main (main) where
import Images(imagesFromFile, labelsFromFile, randomEpoch)
import Mat (Matrix)
import Training (evaluateNetwork, trainOneBatch, trainManyBatches)
import WeightsBiases (saveParametersToFile, parametersFromFile, randomEverything, randomWeightsZeroBiases)
import Output(output, costManyExamples, activations)
import Utils (partitionPieceLength)
import Rando(shuffle)
import Control.Monad (replicateM)
import Data.Foldable(foldlM)



testImagePath :: String
testImagePath = "C:\\Users\\ggisl\\Desktop\\Haskell\\NeuralNetwork\\data\\t10k-images.idx3-ubyte"
testLabelPath :: String
testLabelPath = "C:\\Users\\ggisl\\Desktop\\Haskell\\NeuralNetwork\\data\\t10k-labels.idx1-ubyte"
trainImagePath :: String
trainImagePath = "C:\\Users\\ggisl\\Desktop\\Haskell\\NeuralNetwork\\data\\train-images.idx3-ubyte"
trainLabelPath :: String
trainLabelPath = "C:\\Users\\ggisl\\Desktop\\Haskell\\NeuralNetwork\\data\\train-labels.idx1-ubyte"

main :: IO ()
main = do

    putStrLn "Hello"

    --saveNewParameters "C:\\Users\\ggisl\\Desktop\\Haskell\\NeuralNetwork\\parameters\\div\\0.txt" 
    --improveFromFileToFile "C:\\Users\\ggisl\\Desktop\\Haskell\\NeuralNetwork\\parameters\\div\\19.txt" "C:\\Users\\ggisl\\Desktop\\Haskell\\NeuralNetwork\\parameters\\div\\20.txt" 
    --testFile "C:\\Users\\ggisl\\Desktop\\Haskell\\NeuralNetwork\\parameters\\several\\0.txt"

    superNetwork "several" 3

    putStrLn "Goodbye"

saveNewParameters :: FilePath -> IO ()
saveNewParameters f = do
    (w,b) <- randomEverything [784,16,16,10]
    --(w,b) <- randomWeightsZeroBiases [784,16,16,10]
    saveParametersToFile f (w,b)

improveFromFileToFile :: FilePath -> FilePath -> IO ()
improveFromFileToFile f1 f2 = do

    (w0,b0) <- parametersFromFile f1

    images <- imagesFromFile trainImagePath
    corrects <- labelsFromFile trainLabelPath
    ic <- randomEpoch images corrects

    let (iss,css) = unzip $ map unzip ic
    let (w1,b1) = trainManyBatches iss css w0 b0

    saveParametersToFile f2 (w1,b1)

testFile :: FilePath -> IO ()
testFile f = do
    (w,b) <- parametersFromFile f

    images <- imagesFromFile testImagePath
    corrects <- labelsFromFile testLabelPath
    print (evaluateNetwork images corrects w b)
    print (costManyExamples images corrects w b)

superNetwork :: String -> Int -> IO ()
superNetwork subDirectory numEpochs = do
    trainImages <- imagesFromFile trainImagePath
    trainCorrects <- labelsFromFile trainLabelPath
    testImages <- imagesFromFile testImagePath
    testCorrects <- labelsFromFile testLabelPath

    newTrained ("C:\\Users\\ggisl\\Desktop\\Haskell\\NeuralNetwork\\parameters\\" ++ subDirectory ++ "\\") numEpochs trainImages trainCorrects testImages testCorrects

newTrained :: FilePath -> Int -> [Matrix] -> [Int] -> [Matrix] -> [Int] -> IO ()
newTrained directory numEpochs trainImages trainCorrects testImages testCorrects = do

    (w,b) <- randomEverything [784,16,16,10]

    saveParametersToFile (directory ++ "0.txt") (w,b)
    putStrLn ("0: " ++ take 5 (show (evaluateNetwork testImages testCorrects w b)) ++ ", " ++ show (costManyExamples testImages testCorrects w b) ++ "\n\n")
    
    _ <- nextEpochToFile 1 w b

    putStrLn "All done!"

    where

        nextEpochToFile :: Int -> [Matrix] -> [Matrix] -> IO ([Matrix],[Matrix])
        nextEpochToFile current w0 b0 = do
            ic <- randomEpoch trainImages trainCorrects
            let (iss,css) = unzip $ map unzip ic

            let (w1,b1) = trainManyBatches iss css w0 b0

            saveParametersToFile (directory ++ show current ++ ".txt") (w1,b1)
            putStrLn (show current ++ ": " ++ take 5 (show (evaluateNetwork testImages testCorrects w1 b1)) ++ ", " ++ show (costManyExamples testImages testCorrects w1 b1) ++ "\n\n")

            if current == numEpochs then return (w1,b1) else nextEpochToFile (current + 1) w1 b1
        