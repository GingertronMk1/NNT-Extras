-------------------------------------------------------------------------------------------------------------------------------------------------------------
-- IMPORTS
---------------------------------------------------------------------------------------------------------------------------------------------------------------
import Utils
import Data.List                      (isPrefixOf, isSuffixOf, sort, group, intersperse, sortBy)
import Data.List.Split                (splitOn)
import Data.Ord                       (comparing)
import qualified Data.Text as T       (unpack)
import qualified Data.Text.IO as TIO  (readFile)

---------------------------------------------------------------------------------------------------------------------------------------------------------------
-- Now defining some types:
-- - Actor and ShowName for type clarity, otherwise it'd be lots of `String -> String` going on
-- - [(ShowName, [Actor])] for the list we're going to generate that contains all the important bits of a show
-- - And Adj, a sort of adjacency list used in the actual finding of the degrees of separation
---------------------------------------------------------------------------------------------------------------------------------------------------------------
type Actor = String
type ShowName = String
type Adj = ([Actor], Int)

---------------------------------------------------------------------------------------------------------------------------------------------------------------
-- A few test variables now:
-- - limit is how far it should go before giving up on finding a link
-- - searchJSON is the location of "search.json", the file I use to make all this work
-- - excludedShows is shows that are not taken into account
---------------------------------------------------------------------------------------------------------------------------------------------------------------
limit :: Int
limit = 1000

searchJSON :: FilePath
searchJSON = "search.json"

excludedShows :: [ShowName]
excludedShows = ["Charity Gala (2015-16)"] ++ ["Freshers' Fringe (" ++ show (n) ++ "-" ++ drop 2 (show (n+1)) ++ ")" | n <- [2010..2017]]

---------------------------------------------------------------------------------------------------------------------------------------------------------------
-- Helpers!
---------------------------------------------------------------------------------------------------------------------------------------------------------------
allActors :: [(ShowName, [Actor])] -> [Actor]
allActors = rmDups . flatten . map snd

---------------------------------------------------------------------------------------------------------------------------------------------------------------
-- What we can do instead of all that is generate the list of (ShowName, [Actor]) from a JSON file, like so:
---------------------------------------------------------------------------------------------------------------------------------------------------------------
sJSONShows :: IO [[String]]   -- Reads the JSON file containing all the show information, returning it in a [[String]] format for processing
sJSONShows = myReadFile searchJSON >>= return . filter (elem "        \"type\": \"show\",") . map lines . splitOn "\n    \n    \n\n    \n    ,"

sJSONTitle :: [String] -> ShowName  -- Extracts the show title from sJSONShows
sJSONTitle = stripShit . dropWhile (/=':') . head . filter (isPrefixOf "        \"title\": \"")

sJSONYear :: [String] -> String -- Extracts the year of production; useful for clarity
sJSONYear = flatten . intersperse "-" . splitOn "&ndash;" . stripShit . dropWhile (/=':') . head . filter (isPrefixOf "        \"year_title\": \"")

sJSONCast :: [String] -> [Actor]  -- Extracts the cast of the show
sJSONCast = filter (/="") . map stripShit . splitOn ", " . stripShit . dropWhile (/=':') . head . filter (isPrefixOf "        \"cast\": \"")

notExcluded :: (ShowName, [Actor]) -> Bool  -- Rather than use a lambda function in filter, I'd rather just use an actual function, again for clarity
notExcluded (s, _) = not (elem s excludedShows)

allDetails' :: [String] -> (ShowName, [Actor])    -- Adding the year to the title
allDetails' s = (sJSONTitle s ++ " (" ++ sJSONYear s ++ ")", sJSONCast s)

allDetails :: IO [(ShowName, [Actor])]    -- Returning a list of all shows, in a tuple of the form (ShowName, [Actor])
allDetails = sJSONShows >>= return . filter (\d -> notExcluded d && length (snd d) > 1) . map allDetails'  -- Get rid of any show with no or 1 recorded Actor/s

---------------------------------------------------------------------------------------------------------------------------------------------------------------
-- Finally, using everything above here, we can get two Actors, and return a printed String with the shortest link between them.
---------------------------------------------------------------------------------------------------------------------------------------------------------------
allFellows :: Actor -> [(ShowName, [Actor])] -> [Actor] -- A helper, a function to generate a list of every Actor one specific Actor has ever worked with.
allFellows a = filter (/=a) . rmDups . flatten . filter (elem a) . map snd

fellowAdj' :: [Adj] -> [(ShowName, [Actor])] -> [Actor] -> [Adj]  -- fellowAdj' takes a list of Adjs, and a list of explored Actors, and goes through each Adj generating
fellowAdj' [] _ _             = []                  -- a list of new Adjs based on the head of the Actor list of each one
fellowAdj' ((a, i):as) d done = let newFellows = [n | n <- allFellows (head a) d, not (elem n done || elem n a)]
                                in [(new:a, i+1) | new <- newFellows] ++ fellowAdj' as d (newFellows ++ done)

fellowAdj :: [Adj] -> [(ShowName, [Actor])] -> [Actor] -> [Adj] -- fellowAdj is then a recursive function that takes the list generated by fellowAdj'
fellowAdj [] _ _    = []                            -- and reapplies fellowAdj' to that list, appending it to the first list
fellowAdj as d done = let newList = fellowAdj' as d done
                      in newList ++ fellowAdj newList d (map (head . fst) newList ++ done)

allAdj :: Actor -> [(ShowName, [Actor])] -> [Adj]       -- And allAdj takes fellowAdj and wraps it all up neatly
allAdj a d = let baa = [([a], 0)]           -- turn the Actor into the most basic Adj first
             in baa ++ fellowAdj baa d []   -- then recursively generate the list of all Adjs

adjLim :: Actor -> Int -> [(ShowName, [Actor])] -> [Adj]  --allAdj is in theory an infinite list, so we use adjLim to limit it
adjLim a l d = takeWhile ((<=l) . snd) (allAdj a d)

adjSearch :: Actor -> Actor -> [(ShowName, [Actor])] -> Adj
adjSearch a1 a2 d = let alList = filter ((== a1) . head . fst) (adjLim a2 limit d)  -- Filters the list of all Adjs from a2 to just those with a1 at the head
                    in if alList == [] then ([a1,a2], 1000) else head alList        -- If that list is empty: error code, otherwise return the head

adjCheck :: Actor -> Actor -> [(ShowName, [Actor])] -> Adj  -- adjCheck is basically input validation; it makes sure both Actors actually have records.
adjCheck a1 a2 d                                            -- If they don't it returns an error code in the result, otherwise it runs adjSearch.
  | not (elem a1 aa || elem a2 aa)  = ([a1,a2], -3)
  | not (elem a2 aa)                = ([a1,a2], -2)
  | not (elem a1 aa)                = ([a1,a2], -1)
  | otherwise                       = adjSearch a1 a2 d
  where aa = allActors d

link :: Actor -> Actor -> [(ShowName, [Actor])] -> String -- link takes two actors and a list of (ShowName, [Actor]) and finds the link between the actors
link a1 a2 d = "- " ++ a1 ++ " was in " ++ (fst . head . filter (\(ss, as) -> elem a1 as && elem a2 as)) d ++ " with " ++ a2

links :: [Actor] -> [(ShowName, [Actor])] -> String -- links then applies this across a list of Actors, doing them two at a time
links (a1:a2:[]) d = link a1 a2 d
links (a1:a2:as) d = link a1 a2 d ++ "\n" ++ links (a2:as) d

ppAdjCheck :: Actor -> Actor -> [(ShowName, [Actor])] -> String -- Finally for the non-IO portion of this bit, ppAdjCheck takes the Actor names and the Detail list,
ppAdjCheck a1 a2 d                                  -- performs adjCheck on them, and returns the appropriate String
  | i == -3   = hl ++ " are not Actors with records."
  | i == -2   = last as ++ " is not an Actor with a record."
  | i == -1   = head as ++ " is not an Actor with a record."
  | i == 0    = head as ++ " has 0 degrees of separation with themself by definition."
  | i == 1000 = hl ++ " are either not linked, or have more than " ++ show limit ++ " degrees of separation."
  | otherwise = hl ++ " have " ++ show i ++ " degrees of separation, and are linked as follows:" ++ "\n" ++ links as d
  where (as, i) = adjCheck a1 a2 d
        hl = head as ++ " and " ++ last as

main' :: Actor -> Actor -> IO ()                          -- main' is where the IO starts; it feeds showDetails into ppAdjCheck
main' a1 a2 = allDetails >>= putStrLn . ppAdjCheck a1 a2  -- and putStrLn's the resultant String so we get nice '\n' newlines

main :: IO ()           --main takes two getLines and returns main' with them as input
main = do a1 <- getLine
          a2 <- getLine
          main' a1 a2

ppString :: Show a => [a] -> String
ppString = flatten . intersperse "\n" . map show

everyAdj = allDetails >>= (\d -> (writeFile "Adjs.txt" . ppString . sortBy (comparing snd) . flatten . map (\a -> allAdj a d)) (allActors d))
