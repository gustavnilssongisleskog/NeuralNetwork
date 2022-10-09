module WeightsBiases where
import System.Random (randomIO)
import Mat(Matrix, vectorToMatrix)
import Utils(partitionPieceLength)
import Control.Monad(liftM2, (<=<))

boxMuller :: IO Double
boxMuller = do
    u1 <- randomIO :: IO Double
    u2 <- randomIO :: IO Double
    return (sqrt (-2 * log u1) * cos (2 * pi * u2))

normals :: Int -> IO [Double]
normals = sequence . flip replicate boxMuller

randomWeights :: [Int] -> IO [Matrix]
randomWeights (l1:l2:ls) = liftM2 (:) (randomWeightsOneLayer l1 l2) (randomWeights (l2:ls))
randomWeights _ = return []

randomWeightsOneLayer :: Int -> Int -> IO Matrix
randomWeightsOneLayer l1 l2 = do
    ws <- normals (l1 * l2)
    return (partitionPieceLength l1 ws)

zeroBiases :: [Int] -> [Matrix]
--zeroBiases (_:l2:ls) = vectorToMatrix (replicate l2 0) : zeroBiases (l2:ls)
--zeroBiases _ = []
zeroBiases = map (vectorToMatrix . flip replicate 0) . tail

randomBiases :: [Int] -> IO [Matrix]
randomBiases = mapM ((return . vectorToMatrix) <=<  normals) . tail

randomWeightsZeroBiases :: [Int] -> IO ([Matrix], [Matrix])
randomWeightsZeroBiases ls = do
    w <- randomWeights ls
    let b = zeroBiases ls
    return (w,b)

randomEverything :: [Int] -> IO ([Matrix], [Matrix])
randomEverything ls = do
    w <- randomWeights ls
    b <- randomBiases ls
    return (w,b)

parametersFromFile :: FilePath -> IO ([Matrix], [Matrix])
-- parametersFromFile f = do
--     contents <- readFile f
--     let wb = read contents :: ([Matrix], [Matrix])
--     return wb
parametersFromFile = (return . read) <=< readFile

saveParametersToFile :: FilePath -> ([Matrix], [Matrix]) -> IO ()
saveParametersToFile = flip $ flip writeFile . show