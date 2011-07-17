-- ^ Functions for dealing with documents, corpora and their features
-- Maintainer: Ryan Burton (ryb@umich.edu)

{-# LANGUAGE BangPatterns, TypeSynonymInstances, OverloadedStrings #-}
module Document where

import Control.Monad
import qualified Data.Char as Char
import qualified Data.Text as Text
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Text.Encoding
import Data.Text.Encoding.Error hiding (replace)
import qualified Data.Text.IO as TextIO
import qualified Data.ByteString.Char8 as B
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.IntMap as IntMap
import Data.List
import System.Directory
import System.FilePath hiding (normalise)
import System.IO.Unsafe
import Control.Parallel.Strategies
import Control.DeepSeq
import qualified NLP.Stemmer as Stemmer
import Data.Array.Unboxed
import qualified Data.Vector.Unboxed as V

-- The following are type aliases so we have some compile-time type checking
type DocText = Text.Text
type Token = DocText
type Tokens = [DocText]
type DocID = B.ByteString
-- | A 'Document' is actually a histogram of terms and their frequency (so a bag of words)
type Document = Map.Map DocText Double
type Documents = Map.Map DocID Document
type Feature = Double
type FeatureNumber = Int
type FeatureNumbers = Map.Map Token FeatureNumber
type FeatureVector = IntMap.IntMap Feature
type DenseFeatureVector = V.Vector Feature
type FeatureVectors = Map.Map DocID FeatureVector
type Vocabulary = (Set.Set Token)
-- 
-- instance NFData DenseFeatureVector where
--     rnf x = V.length x `seq` ()
-- 
-- instance NFData B.ByteString where
--     rnf x = B.length x `seq` ()
-- 
-- instance NFData BL.ByteString where
--     rnf x = BL.length x `seq` ()

unseen = "***UNSEEN***"

-- | Append or concatenate Text
(++.) = Text.append

-- | Generates a list of feature vectors with their associated DocID from the vocabulary and a list of documents (bag of words)
featureVectors :: Vocabulary -> Documents -> FeatureVectors
featureVectors vocabulary documents = Map.fromList [(docid, featuresFromDocument vocabulary $ document docid) | docid <- (docids documents)]
    where
        -- Get the 'Document' for a document id
        document docid = case Map.lookup docid documents of
            Nothing -> error "DocID not accounted for"
            Just d -> d

-- | Get a list of DocIDs from a list of documents (term histogram/bag of words)
docids documents = Map.keys documents

-- | Gets a feature from a document
featureFromDocument w d = Map.findWithDefault 0 w d

-- | Gets all the features from a list of documents. Note to self: This isn't actually sparse since I'm storing all the counts, even if they're zero
featuresFromDocument vocabulary d = IntMap.fromList $
                                    zip [1..] $
                                    map (\w -> (featureFromDocument w d)) $
                                    Set.toList vocabulary
                                    
denseFeaturesFromDocument vocabulary d = V.fromList $
                                    map snd $ zip [1..] $
                                    map (\w -> (featureFromDocument w d)) $
                                    Set.toList vocabulary

-- | A strict (eager, non-lazy) version of sum so that the stack doesn't overflow
sum' = foldl' (+) 0

-- | Removes the features that are zero from a feature vector
pruneZeroFeatures :: [(FeatureNumber, Feature)] -> [(FeatureNumber, Feature)]
pruneZeroFeatures xs = filter (\(_, f) -> f > 0) xs

-- | Replace a substring with another
replace s d = Text.intercalate d . Text.splitOn s

-- | Replace a substring with another if the substring to replace (even a character for instance) satisfies a condition
replaceIf p d = Text.intercalate d . Text.split p

space = Text.pack " "
apostrophe = Text.pack "'"
tilde = Text.pack "~"

-- | Remove all characters from a string other than alpha characters and apostrophes
keepLetters = replaceIf (\x -> not (Char.isAlpha x || (x == '\''))) space

-- | Changes, for instance, "Ryan's" with "Ryan~s" so that possessive forms are kept. We actually want to remove all apostropes in the end.
changePossessives w | (not $ Text.null fp) && (not $ Text.null sp) && (Char.isLetter $ shead sp) = changePossessives $ Text.append fp (Text.append tilde sp)
                    | otherwise = Text.append fp asp
    where
        (fp, asp) = Text.breakOn apostrophe w
        sp = stail asp
        stail s | Text.null s = Text.empty
                | otherwise = Text.tail s
        shead s | Text.null s = ' '
                | otherwise = Text.head s

-- | Keep alpha characters, and remove apostrophes while keeping possessive forms (see 'changePossessives')
clean :: DocText -> DocText
clean = replace apostrophe space . changePossessives . keepLetters                                                 

-- | Group elements of a list and get a count of the occurrences of each element of the list
histogram = foldl' (\m w -> Map.insertWith' (+) w 1 m) Map.empty

-- | Splits text into words on whitespace.
tokenise :: DocText -> Tokens
tokenise = Text.words

-- | Converts text to lower case
toLower :: DocText -> DocText
toLower = Text.toLower

-- | Replaces unseen words (those not in a specified vocabulary) with a special token
standardiseUnseen vocabulary = fmap (\w -> if Set.member w vocabulary then w else unseen)

-- | Given a vocabulary and some text, 'document' cleans it up with 'clean', tokenises it with 'tokenise', performs stemming with 'stemWords', replaces unseen words with a special token (see 'standardiseUnseen'), and gives a histogram of the terms.
document :: Vocabulary -> DocText -> Document
document vocabulary t = (histogram . (standardiseUnseen vocabulary) . tokenise . clean . toLower) t

-- | Performs stemming with the Snowball stemmer
stemWords :: Tokens -> Tokens
stemWords = fmap Text.pack . (Stemmer.stemWords Stemmer.English) . (fmap Text.unpack)
    
-- | Reads and returns the contents of a file (decoded [potentially lossily] to UTF-8) and keeps its name
nameAndContents :: FilePath -> IO (FilePath, DocText)
nameAndContents path = do
    bs <- B.readFile path
    return (takeFileName path, decodeUtf8With lenientDecode bs)

-- | Returns the filenames of each file in a directory
readDirectory dir = liftM (map ((</>) dir) . filter (not . (isPrefixOf "."))) $ getDirectoryContents dir

-- | Reads and returns the contents of a file (decoded [potentially lossily] to UTF-8) and throws away its name
justContents :: FilePath -> IO DocText
justContents path = do
    bs <- B.readFile path
    return (decodeUtf8With lenientDecode bs)

-- | Returns all the documents (term histograms) for a directory
documents :: Vocabulary -> FilePath -> IO (Documents)
documents vocabulary dir = let corpusPath = takeDirectory dir in do
    files <- readDirectory dir
    ncs <- mapM nameAndContents files
    let nds = map (\(n, c) -> (B.pack n, document vocabulary c)) ncs
    return (Map.fromList nds)

-- | The vocabulary of a corpus
vocabularyFromFiles :: [FilePath] -> IO Vocabulary
vocabularyFromFiles files = do
    -- read the contents of the files
    txt <- mapM justContents files
    -- Get a 'Set' of terms – lowercased, cleaned, and tokenised.
    return (Set.fromList $ tokenise $ clean $ toLower $ Text.unwords txt)
    
vocabulary :: FilePath -> IO Vocabulary
vocabulary corpusPath = do
    files <- readDirectory corpusPath
    v <- vocabularyFromFiles files
    return v

-- | The union of a list of sets
unions !x = x' `seq` x'
    where x' = foldl' Set.union Set.empty x

-- | If we have a document ID, this gives us a raw feature vector (a list of features)
featureVectorFromDocId fvs docId = case Map.lookup docId fvs of
    Just fvs -> fvs `using` rdeepseq
    _ -> error $ "No feature vector for docid " ++ (show docId)
        
-- | All the feature vectors in a corpus placed in a 'Map' so that we can look them up by document ID
allFeatureVectors :: Vocabulary -> FilePath -> IO (FeatureVectors)
allFeatureVectors vocab corpusPath = do
    ds <- documents vocab corpusPath
    return $ (featureVectors vocab ds)