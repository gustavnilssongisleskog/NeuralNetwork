module Mat where
import Utils (sigmoid)
import Data.List (singleton)

type Vector = [Double]
type Matrix = [[Double]]

matMul :: Matrix -> Matrix -> Matrix
matMul xss yss = map (\xs -> map (dotProduct xs) (transpose yss)) xss
--matMul xss yss = [[dotProduct (xss !! (i - 1)) [yss !! (k - 1) !! (j - 1) | k <- [1..(length yss)]] | j <- [1..(length $ head yss)] ] | i <- [1..(length xss)] ]

matAdd :: Matrix -> Matrix -> Matrix
matAdd = zipWith $ zipWith (+)

matMap :: (Double -> Double) -> Matrix -> Matrix
matMap = map . map

matrixSigmoid :: Matrix -> Matrix
matrixSigmoid = matMap sigmoid

transpose :: Matrix -> Matrix
transpose ([]:_) = []
transpose xss = map head xss : transpose (map tail xss)
--transpose xss = [[xss !! (i - 1) !! (j - 1) | i <- [1..(length xss)]] | j <- [1..(length $ head xss)]]

vectorToMatrix :: Vector -> Matrix
vectorToMatrix = map singleton

matrixToVector :: Matrix -> Vector
matrixToVector = map head

dotProduct :: Vector -> Vector -> Double
dotProduct = (sum .) . zipWith (*)

matMulByVector :: Matrix -> Vector -> Vector
matMulByVector xss ys = map (dotProduct ys) xss
--matMulByVector = flip $ (matrixToVector .) . matMul . vectorToMatrix

matSum :: Matrix -> Double
matSum = sum . map sum