module Images where
import Mat (Matrix, vectorToMatrix)
import qualified Data.ByteString as B
import Control.Monad ((<=<), replicateM)
import Utils (partitionPieceLength, sigmoid)
import Rando(shuffle)

byteStringToList :: B.ByteString -> [Int]
byteStringToList = map (fromIntegral . toInteger) . B.unpack

labels :: [Int] -> [Int]
labels = drop 8

images :: [Int] -> [Matrix]
images = map vectorToMatrix . partitionPieceLength (28 * 28) . map ((/255) . fromIntegral) . drop 16

labelsFromFile :: String -> IO [Int]
labelsFromFile = (return . labels . byteStringToList) <=< B.readFile

imagesFromFile :: String -> IO [Matrix]
imagesFromFile = (return . images . byteStringToList) <=< B.readFile

-- randomInts :: Int -> Int -> IO [Int]
-- randomInts n maxNum = mapM (return . flip mod maxNum <=< const randomIO) [1..n]

-- choose :: [Int] -> [a] -> [a]
-- choose (i:is) xs = xs !! i : choose is xs
-- choose _ _ = []

-- randomData :: Int -> [Matrix] -> [Int] -> IO [(Matrix, Int)]
-- randomData n imgs labels = do
--     idxs <- randomInts n (length labels)
--     let chosenLabels = choose idxs labels
--     let chosenImgs = choose idxs imgs

--     return $ zip chosenImgs chosenLabels

-- manyBatches :: Int -> Int -> [Matrix] -> [Int]-> IO [[(Matrix, Int)]]
-- manyBatches batches batchSize imgs labels = replicateM batches (randomData batchSize imgs labels)

randomEpoch :: [Matrix] -> [Int] -> IO [[(Matrix, Int)]]
randomEpoch =  ((fmap $ partitionPieceLength 100) .) . (shuffle .) . zip