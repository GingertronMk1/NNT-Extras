module Utils where

import Data.List (sort, group)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

flatten :: [[a]] -> [a]                     -- Flattening lists of lists
flatten ass = [a | as <- ass, a <- as]

rmDups :: (Eq a, Ord a) => [a] -> [a]       -- Removing duplicate entries in a sortable list
rmDups = map head . group . sort

myReadFile :: FilePath -> IO String
myReadFile = fmap T.unpack . TIO.readFile

stripShit :: String -> String -- Stripping out any characters that might surround an actor or show's name
stripShit s                   -- Whitespace, quotation marks, colons, etc.
  | s == []                             = []
  | elem (head s) [' ', '\"', '[', ':'] = stripShit (tail s)
  | elem (last s) [' ', '\"', ']', ','] = stripShit (init s)
  | otherwise                           = s

