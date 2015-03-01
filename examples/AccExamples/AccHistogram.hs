import Data.Array.Accelerate as Acc
import Data.Array.Accelerate.Interpreter
import Data.Int

histogramAcc :: (Int,Int) -> Vector Float -> Acc (Vector Int32)
histogramAcc (m,n) vec =
  let vec'  = use vec
      zeros = generate (constant (Z:. n-m)) (const 0)
      ones  = generate (shape vec') (const 1)
  in permute (+) zeros (\ix -> index1 (Acc.floor (vec' Acc.! ix) :: Exp Int)) ones

main = do let vec = Acc.fromList (Z :. 5) [1,2,3,3,4] :: Vector Float
          print $ run $ histogramAcc (0,5) vec
