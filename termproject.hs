import qualified Data.Map as M
import Data.Maybe
import System.Environment
import System.IO
import System.Exit
import Prelude hiding (Word)

data Trie = Trie {end :: Bool, children :: M.Map Char Trie}
	deriving (Show)

type Word = String

data Action = Add | Search | Find | Print | Exit | Err 
			deriving (Eq, Show)

main = do
	let trie = empty
	getInput trie

empty :: Trie
empty = Trie {end = False , children = M.empty}

insert :: Word -> Trie -> Trie
insert [] 		t	= 	let ts = children t
						in case isListNull $ M.toList ts of
							True	-> t {end = True , children = M.empty}
							False	-> t {end = True , children = ts}				
insert (x:xs) 	t 	= 	let ts = children t
						in case M.lookup x ts of
							Nothing ->	t {end = (end t) , children = M.insert x (insert xs $ empty) ts }
							Just t' ->	t {end = (end t) , children = M.insert x (insert xs $ t') ts}

insertList :: [Word] -> Trie
insertList []	= empty
insertList (x:xs) = insert x $ insertList xs

search :: Word -> Trie -> Bool
search [] _ 				= 	False																-- Searching for an empty string
search (x:xs)	trie		= 	let m = children trie
								in case M.lookup x m of
									Nothing -> False
									Just t' -> if (isStringNull xs) then end t' else (search xs t')

getWords :: Trie -> [Word]
getWords = undefined

prefix :: Word -> Trie -> Maybe [Word]
prefix = undefined

-- ** Necessary functions for Action handling. **

printActions :: IO ()
printActions = putStrLn "a) Add word\ns) Search word\nf) Find words with prefix\np) Print all words\ne) Exit\nEnter an action:"

getInput :: Trie -> IO Trie
getInput trie = do 
	printActions
	act <- getLine
	newTrie <- doAction (convertAction act) trie
	getInput newTrie

convertAction :: String -> Action
convertAction str
	| str == "a" = Add
	| str == "s" = Search
	| str == "f" = Find
	| str == "p" = Print
	| str == "e" = Exit
	| otherwise  = Err

doAction :: Action -> Trie -> IO Trie 							-- This function's return type is IO Trie, because it must always return the trie in order to operate on
doAction Add trie = addFunction trie 							-- the same trie.
doAction Search trie = searchFunction trie
doAction Find trie = findFunction trie
doAction Print trie = printFunction trie
doAction Exit trie = exit trie
doAction Err trie = error "Not a valid action."

-- ** Necessary functions for Action handling. **


-- ** Action related function implementations. **

addFunction :: Trie -> IO Trie
addFunction trie = do
	putStrLn "Enter word/prefix:"
	line <- getLine
	let resultTrie = insert line trie
	putStrLn ("New word is added! -> " ++ line)
	return resultTrie

searchFunction :: Trie -> IO Trie
searchFunction trie = do
	putStrLn "Enter word/prefix:"
	line <- getLine
	if (search line trie) then putStrLn "Exists in dictionary!" else putStrLn "Doesn't exist!"
	return trie

findFunction :: Trie -> IO Trie
findFunction trie  = do
	putStrLn "Enter word/prefix:"
	line <- getLine
	putStrLn "Find function result will be displayed here."
	return trie

printFunction :: Trie -> IO Trie
printFunction trie = do
	putStrLn "List of words in dictionary:"
	putStrLn "Results\nof\nPrint\nFunction\nwill\nbe\ndisplayed\nhere."
	return trie

exit :: Trie -> IO Trie
exit trie = do
	putStrLn "Program terminated."
	return exitSuccess trie

-- ** Action related function implementations. **


-- ** Null functions to check whether the lists are empty or not. **

isListNull :: [(Char, Trie)] -> Bool
isListNull [] = True
isListNull _  = False

isStringNull :: [Char] -> Bool
isStringNull [] = True
isStringNull _  = False

-- ** Null functions to check whether the lists are empty or not. **









