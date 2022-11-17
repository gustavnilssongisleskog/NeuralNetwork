module Main (main) where
import Images(imagesFromFile, labelsFromFile, randomEpoch)
import Mat (Matrix, matrixToVector)
import Training (evaluateNetwork, trainOneBatch, trainManyBatches)
import WeightsBiases (saveParametersToFile, parametersFromFile, randomEverything, randomWeightsZeroBiases)
import Output(output, costManyExamples, activations)
import Utils (partitionPieceLength)
import Rando(shuffle)
import Control.Monad (replicateM)
import Data.Foldable(foldlM)
import Data.List (intercalate)



testImagePath :: String
testImagePath = "data\\t10k-images.idx3-ubyte"
testLabelPath :: String
testLabelPath = "data\\t10k-labels.idx1-ubyte"
trainImagePath :: String
trainImagePath = "data\\train-images.idx3-ubyte"
trainLabelPath :: String
trainLabelPath = "data\\train-labels.idx1-ubyte"

main :: IO ()
main = do

    putStrLn "Hello"

    putStrLn "Write 1 to train a new network, 2 to test an existing network on all test cases at once, or 3 to test the network on one case at a time, and see the written digit and the prediction of the network"
    reply <- getLine
    if reply == "1" then ioTrain else (if reply == "2" then ioTest else (if reply == "3" then ioTestOneByOne else putStrLn "I suspect you didn't write 1, 2 or 3"))

    putStrLn "Goodbye"

saveNewParameters :: FilePath -> IO ()
saveNewParameters f = do
    (w,b) <- randomEverything [784,16,16,10]
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

ioTestOneByOne :: IO ()
ioTestOneByOne = do
    putStrLn "What is the file name of the network you want to test?"
    file <- getLine
    (w,b) <- parametersFromFile file
    images <- imagesFromFile testImagePath
    corrects <- labelsFromFile testLabelPath
    testOneByOne w b images corrects

    where
        testOneByOne w b (i:is) (c:cs) = do
            printImage i
            putStrLn $ "Answer according to the network: " ++ show (output i w b)
            putStrLn $ "Correct answer: " ++ show c
            putStrLn "Press enter to continue"
            _ <- getLine

            testOneByOne w b is cs
        testOneByOne _ _ _ _ = putStrLn "Out of test cases :("



printImage :: Matrix -> IO ()
printImage i = putStrLn $ intercalate "\n" $ partitionPieceLength 28 $ map (\x -> if x > 0.5 then '#' else if x > 0.25 then '.' else ' ') $ matrixToVector i

ioTest :: IO ()
ioTest = do
    putStrLn "What is the file name of the network you want to test?"
    file <- getLine
    testFile file

testFile :: FilePath -> IO ()
testFile f = do
    (w,b) <- parametersFromFile f

    images <- imagesFromFile testImagePath
    corrects <- labelsFromFile testLabelPath
    putStrLn $ "Correct in " ++ show (evaluateNetwork images corrects w b) ++ "% of test cases"
    putStrLn $ "Average of cost function over all test cases: " ++ show (costManyExamples images corrects w b)

ioTrain :: IO ()
ioTrain = do
    putStrLn "How many epochs do you want to train?"
    epochs <- fmap (read :: String -> Int) getLine
    putStrLn "What should be the name of the new network? Note that you must create a folder with the name of the network, inside the \"networks\" folder"
    subDirectory <- getLine
    superNetwork subDirectory epochs
    putStrLn $ "The network was saved in networks/" ++ subDirectory ++ "/" ++ show epochs ++ ".txt"

superNetwork :: String -> Int -> IO ()
superNetwork subDirectory numEpochs = do
    trainImages <- imagesFromFile trainImagePath
    trainCorrects <- labelsFromFile trainLabelPath
    testImages <- imagesFromFile testImagePath
    testCorrects <- labelsFromFile testLabelPath

    newTrained ("networks\\" ++ subDirectory ++ "\\") numEpochs trainImages trainCorrects testImages testCorrects

newTrained :: FilePath -> Int -> [Matrix] -> [Int] -> [Matrix] -> [Int] -> IO ()
newTrained directory numEpochs trainImages trainCorrects testImages testCorrects = do

    (w,b) <- randomEverything [784,16,16,10]

    saveParametersToFile (directory ++ "0.txt") (w,b)
    putStrLn ("0: " ++ take 5 (show (evaluateNetwork testImages testCorrects w b)) ++ "%, " ++ show (costManyExamples testImages testCorrects w b) ++ "\n\n")

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

            if current >= numEpochs then return (w1,b1) else nextEpochToFile (current + 1) w1 b1
        