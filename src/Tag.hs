{-# LANGUAGE UnicodeSyntax #-}

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

corpusFilesPreprocessed dir = do
    paths <- readDirectory dir
    fmap (map (second preprocessDocument)) $ mapM nameAndContents paths

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
        df word = fromIntegral $ Set.size $
            Map.findWithDefault Set.empty word index
        idf word = log (n/(df word))
        calculateTfidf word docname = (tf word docname) * (idf word)

tfidfVector vocab tfidfs d = V.fromList $
    map snd $ zip [1..] $ map (\w -> (tfidfs w d)) $
    Set.toList vocab

bestMatch vocab tfidfs submission corpus = chooseBest $
        reverse $ sortBy (comparing snd) $ map similarity corpus
    where
        similarity (name, gameDoc) = (name, sim)
            where
                sim = vectorCosineSimilarity v1 v2
                (v1, v2) = getFeatureVectors vocab doc1 doc2
                [doc1, doc2] = map Text.unwords [snd submission, gameDoc]
        chooseBest ((x, sim):xs) =
            if sim > 0.8
                then Just ("http://en.wikipedia.org/wiki/" <> x)
                else Nothing

main = do
    wikipediaVocabulary <- vocabulary "../game_corpus"
    redditVocabulary <- vocabulary "../reddit_examples"
    let vocab = Set.union wikipediaVocabulary redditVocabulary
    gameCorpusPreprocessed <- corpusFilesPreprocessed "../game_corpus"
    redditCorpusPreprocessed <- corpusFilesPreprocessed "../reddit_examples"
    let tfidfs = undefined
            -- mkTfidfs (gameCorpusPreprocessed ++ redditCorpusPreprocessed)
    forM_ redditCorpusPreprocessed $ \submission → do
        case bestMatch vocab tfidfs submission gameCorpusPreprocessed of
            Nothing → putStrLn "No luck"
            Just x → putStrLn $ dropExtension x