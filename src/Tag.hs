{-# LANGUAGE UnicodeSyntax, OverloadedStrings #-}

import Index hiding (Token)
import Document
import Similarity
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

cleanAndRemoveComments ls = filter (not . Text.null) $ map removeComments ls
    where
        removeComments str = Text.stripEnd $ fst $ Text.breakOn "|" str

readStoplist path = 
    fmap (cleanAndRemoveComments . Text.lines . TSE.decodeUtf8) $
        BS.readFile path

corpusFilesPreprocessed stopSet dir = do
        paths <- readDirectory dir
        fmap (map (second (removeStopWords . preprocessDocument))) $ mapM nameAndContents paths
    where
        removeStopWords = filter (\x → not $ x `Set.member` stopSet)

preprocessDocument = tokenise . clean . toLower

tf = histogram

mkTfidfs contents = (\word docname → calculateTfidf word docname)
    where
        n = fromIntegral $ length contents
        tfs = Map.fromList $ map (second histogram) contents
        tf word docname = case Map.lookup docname tfs of
            Nothing → 0
            Just tfMap → Map.findWithDefault 0 word tfMap
        index = buildIndex contents
        df word = fromIntegral $ Map.findWithDefault 0 word index
        idf word = log (n/(df word))
        calculateTfidf word docname = (tf word docname) * (idf word)

tfidfVector vocab tfidfs d = V.fromList $
    map snd $ zip [1..] $ map (\w -> (tfidfs w d)) $
    Set.toList vocab

bestMatch vocab tfidfs submission corpus = chooseBest $ (traceSortShow' $ map similarity corpus)
    where
        similarity (name, gameDoc) = (name, sim)
            where
                sim = vectorCosineSimilarity v1 v2
                (v1, v2) = getFeatureVectors vocab (traceShow' doc1) doc2
                [doc1, doc2] = map Text.unwords [snd submission, gameDoc]
        chooseBest xs = choose' $ maximumBy (comparing snd) xs
            where
                choose' (x, sim) = if sim > 0.8
                    then Just ("http://en.wikipedia.org/wiki/" <> x)
                    else Nothing

main = do
    wikipediaVocabulary <- vocabulary "../game_corpus"
    redditVocabulary <- vocabulary "../reddit_examples"
    stopSet <- fmap (Set.fromList) $ readStoplist "../english_stop.txt"
    let vocab = Set.union wikipediaVocabulary redditVocabulary
    gameCorpusPreprocessed <- corpusFilesPreprocessed stopSet "../game_corpus"
    redditCorpusPreprocessed <- corpusFilesPreprocessed stopSet "../reddit_examples"
    let tfidfs =
            mkTfidfs $ zip [0..] $ map snd (gameCorpusPreprocessed ++ redditCorpusPreprocessed)
    forM_ redditCorpusPreprocessed $ \submission → do
        case bestMatch vocab tfidfs submission gameCorpusPreprocessed of
            Nothing → putStrLn "No luck"
            Just x → putStrLn $ dropExtension x