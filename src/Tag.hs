{-# LANGUAGE UnicodeSyntax, OverloadedStrings #-}

import Index
import Document
import Similarity (vectorCosineSimilarity)
import Data.Ord
import Data.List
import Control.Monad
import Control.Arrow
import Control.Applicative
import qualified Data.Text as Text
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Vector.Unboxed as V
import Data.String.Combinators
import System.FilePath
import qualified Data.Text.Encoding as TSE
import qualified Data.ByteString as BS
import Debug.Trace

traceShow' x = traceShow x x
traceSortShow' x = traceShow' $ sortBy (comparing snd) x

corpusFilesPreprocessed dir = do
        paths <- readDirectory dir
        fmap (map (first Text.pack >>> second preprocessDocument)) $
            mapM nameAndContents paths

preprocessDocument = tokenise . clean . toLower

mkTfidfs idf contents = (\word docname → max 0 $ calculateTfidf word docname)
    where
        tfs = Map.fromList $ map (second histogram) contents
        tf word docname = case Map.lookup docname tfs of
            Nothing → 0
            Just tfMap → Map.findWithDefault 0 word tfMap
        calculateTfidf word docname = (tf word docname) * (idf word)

mkIdf contents = (\word → calculateIdf word)
    where
        calculateIdf word   | df' == 0 = 2**32
                            | otherwise = log (n / df')
            where df' = df word
        df word = fromIntegral $ Map.findWithDefault 0 word index
        index = buildIndex contents
        n = fromIntegral $ length contents

tfidfVector vocab tfidfs (name, d) = V.fromList $
    map snd $ zip [1..] $ map (\w -> (tfidfs w name)) $
    Set.toList vocab

bestMatch idf tfidfs submission corpus =
        chooseBest $ map similarity corpus
    where
        similarity gameDoc@(name, gameDocText) = (name, sim)
            where
                vocab = redditVocab `Set.union` gameVocab
                    where gameVocab = Set.fromList gameDocText
                sim = vectorCosineSimilarity submissionVector gameVector
                gameVector = tfidfVector vocab tfidfs gameDoc
                submissionVector = denseFeaturesFromDocument vocab
                    submissionSparseVector
        submissionSparseVector =
            Map.map (max 0.0) $
            Map.mapWithKey (\word tf → tf * (idf word)) $
            histogram $ snd submission
        redditVocab = Map.keysSet submissionSparseVector
        chooseBest xs = choose' m (zscore $ snd m) (zscore $ snd m2)
            where
                m = maximumBy (comparing snd) xs
                m2 = maximumBy (comparing snd) (delete m xs)
                zscore x = (x - mean)/stddev
                stddev = sqrt $ sum $ map (\x → (mean - x) **2) xs'
                mean = (sum xs') / (fromIntegral $ length xs')
                xs' = map snd xs
                choose' (x, sim) zscore1 zscore2 = if zscore1/zscore2 > 1.5
                    then Just ("http://en.wikipedia.org/wiki/" <> x)
                    else Nothing

main = do
    gameCorpusPreprocessed <- corpusFilesPreprocessed "../game_corpus"
    redditCorpusPreprocessed <- corpusFilesPreprocessed "../reddit_examples"
    let idf = mkIdf (gameCorpusPreprocessed ++ redditCorpusPreprocessed)
    let tfidfs =
            mkTfidfs idf (gameCorpusPreprocessed ++ redditCorpusPreprocessed)
    forM_ redditCorpusPreprocessed $ \submission → do
        case bestMatch idf tfidfs submission gameCorpusPreprocessed of
            Nothing → putStrLn "No luck"
            Just x → putStrLn $ dropExtension . Text.unpack $ x