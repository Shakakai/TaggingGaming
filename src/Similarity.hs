module Similarity where

import Document
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Set as Set
import qualified Data.Vector.Unboxed as V
import Debug.Trace

traceShow' x = traceShow x x

vectorCosineSimilarity fv1 fv2 = (innerProduct fv1 fv2) /
    (sqrt $ sumOfSquares fv1 * (sumOfSquares fv2))

innerProduct fv1 fv2 = V.sum $ V.zipWith (*) fv1 fv2

sumOfSquares fv = V.sum $ V.map (**2) fv