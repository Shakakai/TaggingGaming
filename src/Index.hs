module Index where

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import Data.List (foldl')

type Token = Text.Text
type Document = (DocID, [Token])
type DocID = Int
type Posting = Int
type Index = Map.Map Token Posting

insert :: Token -> DocID -> Index -> Index
insert token dID index = Map.insertWith post token count index
	where
		count = 1

insertDocument :: Document -> Index -> Index
insertDocument (dID, tokens) index = foldl' (\idx x -> insert x dID idx) index tokens

docID = fst

post :: Posting -> Posting -> Posting
post x posting = x + posting

buildIndex :: [Document] -> Index
buildIndex ds = buildIndex' ds (Map.empty :: Index)
	where
		buildIndex' ds index = foldl' (\idx d -> insertDocument d idx) index ds