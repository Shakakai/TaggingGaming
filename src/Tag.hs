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

corpusFilesPreprocessed dir = do
    paths <- readDirectory dir
    fmap (map (second preprocessDocument)) $ mapM nameAndContents paths

preprocessDocument = tokenise . clean . toLower

tf = histogram

-- tfidfs contents = undefined
--     where
--         vocab = histogram $ concat $ map snd contents
--         n = fromIntegral $ length contents
--         tfs = Map.fromList $ map (second tf) contents
--         tf word docname = case Map.lookup docname tfs of
--             Nothing → 0
--             Just tfMap → Map.findWithDefault 0 word tfMap
--         index = buildIndex $ map histogram contents
--         df word = fromIntegral $ length $ Map.findWithDefault [] word index
--         idf word = (log (n/(df word)))
--         tfidf word docname tfValue = (tf word) * (idf word)

bestMatch submission corpus = chooseBest $
        reverse $ sortBy (comparing snd) $ map similarity corpus
    where
        similarity (name, gameDoc) =
            (name, cosineSimilarity (Text.unwords $ snd submission) (Text.unwords gameDoc))
        chooseBest ((x, sim):xs) = if sim > 0.8 then Just x else Nothing

main = do
    wikipediaVocabulary <- vocabulary "../game_corpus"
    redditVocabulary <- vocabulary "../reddit_examples"
    let vocab = Set.union wikipediaVocabulary redditVocabulary
    gameCorpusPreprocessed <- corpusFilesPreprocessed "../game_corpus"
    redditCorpusPreprocessed <- corpusFilesPreprocessed "../reddit_examples"
    forM_ redditCorpusPreprocessed $ \submission → do
        print $ bestMatch submission gameCorpusPreprocessed