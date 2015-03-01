module Ion.Base ( L.vector
                , L.load
                , L.transform
                , L.adjdiff
                , L.sort
                , L.cout
                , (L.#)
                , (L.<#>)
                , L.toThrust
                , L.bool
                , L.int
                , L.complex
                , T.Ion
                , C.Complex((:+))
                , (B.&&*)
                , (B.||*)
                , B.true
                , B.false
                ) where

import qualified Ion.Private.Types as T
import qualified Ion.Private.Lang as L
import qualified Data.Complex as C
import qualified Data.Boolean as B
