module Index where

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import Data.List (foldl')

insert token dID index = Map.insertWith post token count index
	where
		count = 1

insertDocument (dID, tokens) index = foldl' (\idx x -> insert x dID idx) index tokens

docID = fst

post x posting = x + posting

buildIndex ds = buildIndex' ds Map.empty
	where
		buildIndex' ds index = foldl' (\idx d -> insertDocument d idx) index ds