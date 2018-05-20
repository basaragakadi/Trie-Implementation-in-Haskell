import qualified Data.Map as M
import Data.Maybe
import System.Environment
import System.IO
import Prelude hiding (Word)

data Trie = Trie {end :: Bool, children :: M.Map Char Trie}

type Word = String

data Action = Add | Search | Find | Print | Exit | Err 
			deriving (Eq, Show)


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

doAction :: Action
doAction = undefined

printErr :: IO ()
printErr = putStrLn "Not a valid action."

-- ** Necessary functions for Action handling. **


