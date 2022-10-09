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

randomEpoch :: [Matrix] -> [Int] -> IO [[(Matrix, Int)]]
randomEpoch =  ((fmap $ partitionPieceLength 100) .) . (shuffle .) . zip