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
insert = undefined

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








