module Utils where

import Data.List (sort, group, intersperse)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

me :: String
me = "Jack Ellis"

flatten :: [[a]] -> [a]                     -- Flattening lists of lists
flatten ass = [a | as <- ass, a <- as]

rmDups :: (Eq a, Ord a) => [a] -> [a]       -- Removing duplicate entries in a sortable list
rmDups = map head . group . sort

showDups :: (Eq a, Ord a) => [a] -> [a]
showDups = map head . filter ((>1) . length) . group . sort

myReadFile :: FilePath -> IO String
myReadFile = fmap T.unpack . TIO.readFile

stripShit :: String -> String -- Stripping out any characters that might surround an actor or show's name
stripShit s                   -- Whitespace), quotation marks), colons), etc
  | s == []                                = []
  | elem (head s) [' ', '\"', '[', ':'] = stripShit (tail s)
  | elem (last s) [' ', '\"', ']', ','] = stripShit (init s)
  | otherwise                              = s

ppShow :: (Show a) => [a] -> String
ppShow = flatten . intersperse "\n" . map show


strToInt :: String -> Int
strToInt st
  | length st > 1   = strToInt' [last st] + ((10*) . strToInt . init) st
  | length st == 1  = strToInt' st
  | otherwise       = 0
  where strToInt' "1" = 1
        strToInt' "2" = 2
        strToInt' "3" = 3
        strToInt' "4" = 4
        strToInt' "5" = 5
        strToInt' "6" = 6
        strToInt' "7" = 7
        strToInt' "8" = 8
        strToInt' "9" = 9
        strToInt' _   = 0
