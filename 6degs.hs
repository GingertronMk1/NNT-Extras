
-------------------------------------------------------------------------------------------------------------------------------------------------------------
-- We're first going to import some things:
-- - Data.List for isInfixOf, sort, and group (isInfixOf is used so much in this)
-- - Data.List.Split to deal with the JSON files
-- - Data.Ord for comparing, and more interesting sorting
-- - System.Directory so we can muck about with files and directories
-- - And Data.Text and Data.Text.IO for stricter file reading
---------------------------------------------------------------------------------------------------------------------------------------------------------------
import Data.List
import Data.List.Split
import Data.Ord
import System.Directory
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

---------------------------------------------------------------------------------------------------------------------------------------------------------------
-- Now defining some types:
-- - Actor and ShowName for type clarity, otherwise it'd be lots of `String -> String` going on
-- - [Details] for the list we're going to generate that contains all the important bits of a show
-- - And Adj, a sort of adjacency list used in the actual finding of the degrees of separation
---------------------------------------------------------------------------------------------------------------------------------------------------------------
type Actor = String
type ShowName = String
type Details = (ShowName, [Actor])
type Adj = ([Actor], Int)

---------------------------------------------------------------------------------------------------------------------------------------------------------------
-- A few test variables now:
-- - limit is how far it should go before giving up on finding a link
-- - showsPath is where the shows are in my copy of the history-project repo
-- - excludedShows is shows that are not taken into account
-- - And myself and some people as test cases for the actual degree-finder
---------------------------------------------------------------------------------------------------------------------------------------------------------------
limit :: Int
limit = 1000
searchJSON :: FilePath
searchJSON = "search.json"
excludedShows :: [ShowName]
excludedShows = ["Charity Gala (2015-16)"] ++ ffGen
ffGen :: [ShowName]
ffGen = ["Freshers' Fringe (" ++ show (n-1) ++ "-" ++ drop 2 (show n) ++ ")" | n <- [2010..2017]]

---------------------------------------------------------------------------------------------------------------------------------------------------------------
-- Helpers!
---------------------------------------------------------------------------------------------------------------------------------------------------------------
flatten :: [[a]] -> [a]                     -- Flattening lists of lists
flatten ass = [a | as <- ass, a <- as]
rmDups :: (Eq a, Ord a) => [a] -> [a]       -- Removing duplicate entries in a sortable list
rmDups = map head . group . sort
allActors :: [Details] -> [Actor]           -- Getting every Actor from a list of Details
allActors = rmDups . flatten . map snd
myReadFile :: FilePath -> IO String
myReadFile = fmap T.unpack . TIO.readFile
stripShit :: String -> String   -- Stripping out any characters that might surround an actor or show's name
stripShit s                     -- Whitespace, quotation marks, colons, etc.
  | hs == ' ' || hs == '\"' || hs == '[' || hs == ':' = stripShit (tail s)
  | ls == ' ' || ls == '\"' || ls == ']' || ls == ',' = stripShit (init s)
  | otherwise                                                       = s
 where hs = head s
       ls = last s

---------------------------------------------------------------------------------------------------------------------------------------------------------------
-- What we can do instead of all that is generate the list of Details from a JSON file, like so:
---------------------------------------------------------------------------------------------------------------------------------------------------------------
sJSONShows :: IO [[String]]
sJSONShows = myReadFile searchJSON >>= return . filter (elem "        \"type\": \"show\",") . map (lines) . splitOn "\n    \n    \n\n    \n    ,"

allDetails' :: [String] -> Details
allDetails' s = (sJSONTitle s ++ " (" ++ sJSONYear s ++ ")", sJSONCast s)
allDetails :: IO [Details]
allDetails = sJSONShows >>= return . filter (\(s,as) -> not (elem s excludedShows)) . map allDetails'

sJSONTitle = stripShit . dropWhile (/=':') . head . filter (isInfixOf "\"title\":")

sJSONYear = flatten . intersperse "-" . splitOn "&ndash;" . stripShit . dropWhile (/=':') . head . filter (isPrefixOf "        \"year_title\": ")

sJSONCast = map stripShit . init . splitOn ", " . dropWhile (/=':') . head . filter (isInfixOf "\"cast\":")

---------------------------------------------------------------------------------------------------------------------------------------------------------------
-- Finally, using everything above here, we can get two Actors, and return a printed String with the shortest link between them.
---------------------------------------------------------------------------------------------------------------------------------------------------------------
baseAdj :: Actor -> [Adj] -- First, a brief function to turn an Actor into the most basic possible Adj WRT that Actor, i.e. themself and a degree of 0
baseAdj a = [([a], 0)]

allFellows :: Actor -> [Details] -> [Actor] -- Then a helper, a function to generate a list of every Actor one specific Actor has ever worked with.
allFellows a = filter (/=a) . rmDups . flatten . filter (elem a) . map snd

fellowAdj' :: [Adj] -> [Details] -> [Actor] -> [Adj]  -- fellowAdj' takes a list of Adjs, and a list of explored Actors, and goes through each Adj generating
fellowAdj' [] _ _               = []                  -- a list of new Adjs based on the head of the Actor list of each one
fellowAdj' ((a, i):adjs) d done = [(new:a, i+1) | new <- newFellows] ++ fellowAdj' adjs d (newFellows ++ done)
                                  where newFellows = [n | n <- allFellows (head a) d, not (elem n done || elem n a)]

fellowAdj :: [Adj] -> [Details] -> [Actor] -> [Adj] -- fellowAdj is then a recursive function that takes the list generated by fellowAdj'
fellowAdj [] _ _    = []                            -- and reapplies fellowAdj' to that list, appending it to the first list
fellowAdj as d done = newList ++ fellowAdj newList d (map (head . fst) newList ++ done)
                      where newList = fellowAdj' as d done

allAdj :: Actor -> [Details] -> [Adj]   -- And allAdj takes fellowAdj and wraps it all up neatly
allAdj a d = baa ++ fellowAdj baa d []  -- so all you've to do is supply an Actor and a Detail list
             where baa = baseAdj a

adjLim :: Actor -> [Details] -> Int -> [Adj]  --allAdj is in theory an infinite list, so we use adjLim to limit it
adjLim a d l = takeWhile ((<=l) . snd) (allAdj a d)

adjSearch :: Actor -> Actor -> [Details] -> Adj                                       -- adjSearch now takes the list generated by adjLim and if no list starts
adjSearch a1 a2 d = if alList == [] then ([a1,a2], 1000) else head alList             -- with the searched Actor, returns the two Actors and 1000 as an error code
                    where alList = filter ((== a1) . head . fst) (adjLim a2 d limit)  -- If it does hit, it returns that Adj.

adjCheck :: Actor -> Actor -> [Details] -> Adj            -- adjCheck is basically input validation; it makes sure both Actors actually have records.
adjCheck a1 a2 d                                          -- If they don't it returns an error code in the result, otherwise it runs adjSearch.
  | not (elem a1 aa || elem a2 aa)  = ([a1,a2], -3)
  | not (elem a2 aa)                = ([a1,a2], -2)
  | not (elem a1 aa)                = ([a1,a2], -1)
  | otherwise                       = adjSearch a1 a2 d
  where aa = allActors d

link :: Actor -> Actor -> [Details] -> String -- link takes two actors and a list of Details and finds the link between the actors
link a1 a2 d = "- " ++ a1 ++ " was in " ++ (fst . head . filter ((\as -> elem a1 as && elem a2 as) . snd)) d ++ " with " ++ a2

links :: [Actor] -> [Details] -> String -- links then applies this across a list of Actors, doing them two at a time
links (a1:a2:[]) d = link a1 a2 d
links (a1:a2:as) d = link a1 a2 d ++ "\n" ++ links (a2:as) d

ppAdjCheck :: Actor -> Actor -> [Details] -> String -- Finally for the non-IO portion of this bit, ppAdjCheck takes the Actor names and the Detail list,
ppAdjCheck a1 a2 d                                  -- performs adjCheck on them, and returns the appropriate String
  | i == -3   = head as ++ " and " ++ last as ++ " are not Actors with records."
  | i == -2   = last as ++ " is not an Actor with a record."
  | i == -1   = head as ++ " is not an Actor with a record."
  | i == 0    = head as ++ " has 0 degrees of separation with themself by definition."
  | i == 1000 = head as ++ " and " ++ last as ++ " are either not linked, or have more than " ++ show limit ++ " degrees of separation."
  | otherwise = head as ++ " and " ++ last as ++ " have " ++ show i ++ " degrees of separation, and are linked as follows:\n" ++ links as d
  where (as, i) = adjCheck a1 a2 d

main' :: Actor -> Actor -> IO ()                          -- main' is where the IO starts; it feeds showDetails into ppAdjCheck
main' a1 a2 = allDetails >>= putStrLn . ppAdjCheck a1 a2  -- and putStrLn's the resultant String so we get nice '\n' newlines

main :: IO ()           --main takes two getLines and returns main' with them as input
main = do a1 <- getLine
          a2 <- getLine
          main' a1 a2
