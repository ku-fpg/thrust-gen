import Data.Array.Accelerate as Acc
import Data.Array.Accelerate.CUDA
import Data.Int

psum :: Acc (Vector Int) -> Acc (Scalar Int)
psum xs = fold (+) 0 xs
