import qualified Data.Map as M
import Data.Maybe
import System.Environment
import System.IO
import Prelude hiding (Word)

data Trie = Trie {end :: Bool, children :: M.Map Char Trie}
type Word = String
data Action = Add | Search | Find | Print | Exit

main = undefined

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

-- Necessary functions

getInput :: IO ()
getInput = undefined

convertAction :: String -> Action
convertAction = undefined

doAction :: Action
doAction = undefined






--printActions :: IO ()
--printActions = putStrLn "a) Add word\ns) Search word\nf) Find words with prefix\np) Print all words\ne) Exit\nEnter an action:"

--waitForAction :: IO () String
--waitForAction = do
--	l <- getLine
--	return (read l :: String)

--executeAction :: String -> IO ()
--executeAction input
--	| input == "a" = putStrLn input
--	| otherwise = putStrLn "default"
