import qualified Data.Map as M
import Data.Maybe
import System.Environment
import System.IO
import Prelude hiding (Word)

data Trie = Trie {end :: Bool, children :: M.Map Char Trie}

type Word = String

data Action = Add | Search | Find | Print | Exit | Err 
			deriving (Eq, Show)

main = do
	printActions
	act <- getLine
	doAction $ convertAction act

empty :: Trie
empty = Trie {end = False , children = M.empty}

insert :: Word -> Trie -> Trie
insert x t = case x of
	[] 		-> t 													-- This line is not going to finish the recursion, it is only for empty input case.
	[x1]	-> addCharToTrie x1 True t 								-- True means char 'x1' is the last element of the string.
	x:xs 	-> insert xs $ addCharToTrie x False t					-- False means char 'x' is not the last element of the string.

insertList :: [Word] -> Trie
insertList = undefined

search :: Word -> Trie -> Bool
search = undefined

getWords :: Trie -> [Word]
getWords = undefined

prefix :: Word -> Trie -> Maybe [Word]
prefix = undefined

-- ** Necessary functions for Action handling. **

printActions :: IO ()
printActions = putStrLn "a) Add word\ns) Search word\nf) Find words with prefix\np) Print all words\ne) Exit\nEnter an action:"

getInput :: IO ()
getInput = undefined

convertAction :: String -> Action
convertAction str
	| str == "a" = Add
	| str == "s" = Search
	| str == "f" = Find
	| str == "p" = Print
	| str == "e" = Exit
	| otherwise  = Err

doAction :: Action -> IO ()
doAction action
	| action == Err = printErr
	| action == Add = addFunction
	| action == Search = searchFunction
	| action == Find = findFunction
	| action == Print = printFunction
	| action == Exit = exit

printErr :: IO ()
printErr = do putStrLn "Not a valid action."

-- ** Necessary functions for Action handling. **


-- ** Action related function implementations. **

addFunction :: IO ()
addFunction = do
	putStrLn "Enter word/prefix:"
	line <- getLine
	putStrLn ("New word is added! -> " ++ line)

searchFunction :: IO ()
searchFunction = do
	putStrLn "Enter word/prefix:"
	line <- getLine
	putStrLn "Search function result will be displayed here." -- TODO: Another function related to search functionality must be implemented with guards according to the result bool of search function on top.

findFunction :: IO ()
findFunction = do
	putStrLn "Enter word/prefix:"
	line <- getLine
	putStrLn "Find function result will be displayed here." -- TODO: Another function related to find functionality must be implemented with guards according to the result of prefix function on top.

printFunction :: IO ()
printFunction = do
	putStrLn "List of words in dictionary:"
	-- TODO: Write another function that lists and prints all words in dictionary here.
	putStrLn "Results\nof\nPrint\nFunction\nwill\nbe\ndisplayed\nhere."

exit :: IO ()
exit = do
	putStrLn "Program terminated."
	-- TODO: Terminate program here.

-- ** Action related function implementations. **


-- ** Function to add char to the trie. **

addCharToTrie :: Char -> Bool -> Trie -> Trie 					-- This function takes 3 parameters. A Char to add to the trie, a Trie to add a Char to and a Bool that
addCharToTrie c b t = case null $ children t of					-- indicates that char is the last character of the word or not.
	True 	-> undefined
	False	-> undefined

	
-- ** Function to add char to the trie. **











