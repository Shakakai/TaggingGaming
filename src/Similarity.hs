--
-- Similarity.hs
-- Pretty much wrote all of this for my SI 650 Final Project (see Similarity.hs there). Extracted the important functions and made them take text as Strings.
-- 
-- Created by Ryan Burton on 2010-12-25.
-- Copyright 2010 Underground Pressure Cooker (http://www.absoluteterritory.org/). All rights reserved.
--

module Similarity where

import Document
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Set as Set
import qualified Data.Vector.Unboxed as V

twoDocVocabulary :: Text -> Text -> Vocabulary
twoDocVocabulary f1 f2 = 
        Set.fromList $ tokenise $ clean $ toLower $ Text.unwords txt
    where txt = [f1, f2]

documentPair :: Vocabulary -> Text -> Text -> (Document, Document)
documentPair vocabulary f1 f2 = (x, y)
    where
        ncs = [f1, f2]
        [x, y] = map (document vocabulary) ncs 

getFeatureVectors vocab doc1 doc2 = (\(d1, d2) -> (f d1, f d2)) $ documentPair vocab doc1 doc2
    where f d = denseFeaturesFromDocument vocab d

-- Cosine Similarity --

vectorCosineSimilarity :: DenseFeatureVector -> DenseFeatureVector -> Double
vectorCosineSimilarity fv1 fv2 = (innerProduct fv1 fv2) /
    (sqrt $ sumOfSquares fv1 * (sumOfSquares fv2))

innerProduct fv1 fv2 = V.sum $ V.zipWith (*) fv1 fv2

sumOfSquares fv = V.sum $ V.map (**2) fv

cosineSimilarity doc1 doc2 = vectorCosineSimilarity fv1 fv2
    where
        v = twoDocVocabulary doc1 doc2
        (fv1, fv2) = getFeatureVectors v doc1 doc2
        
stringCosineSimilarity doc1 doc2 =
        cosineSimilarity (Text.pack doc1) (Text.pack doc2)
