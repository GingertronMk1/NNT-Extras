---------------------------------------------------------------------------------------------------------------------------------------------------------------
-- IMPORTS
---------------------------------------------------------------------------------------------------------------------------------------------------------------
import Utils
import Data.List                      (isPrefixOf, sort, group, intersperse, sortBy)
import Data.List.Split                (splitOn)
import Data.Ord                       (comparing)
import Data.Char

---------------------------------------------------------------------------------------------------------------------------------------------------------------
-- Now defining some types:
-- - Actor and ShowName for type clarity, otherwise it'd be lots of `String -> String` going on
-- - [Details] for the list we're going to generate that contains all the important bits of a show
-- - And Adj, a sort of adjacency list used in the actual finding of the degrees of separation
---------------------------------------------------------------------------------------------------------------------------------------------------------------
type Actor    = String
type ShowName = String
type Adj      = ([Actor], Int)
type Details  = (ShowName, [Actor])

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

adjFile :: FilePath
adjFile = "Adjs.txt"

---------------------------------------------------------------------------------------------------------------------------------------------------------------
-- What we can do instead of all that is generate the list of Details from a JSON file, like so:
---------------------------------------------------------------------------------------------------------------------------------------------------------------
sJSONShows :: IO [[String]]   -- Reads the JSON file containing all the show information, returning it in a [[String]] format for processing
sJSONShows = myReadFile searchJSON >>= return
                                       . filter (elem "\"type\": \"show\",")  -- 5: Filter to only those that are shows
                                       . map (filter (/="")                   -- 4: Get rid of empty pits
                                       . map (dropWhile (==' '))              -- 3: Get rid of leading whitespace
                                       . filter (/="    }") . lines)          -- 2: Get rid of the corresponding
                                       . splitOn "    ,{"                     -- 1: Split on the delimiter for new entries

sJSONTitle :: [String] -> ShowName  -- Extracts the show title from sJSONShows
sJSONTitle = stripShit . dropWhile (/=':') . head . filter (isPrefixOf "\"title\": \"")

sJSONYear :: [String] -> String -- Extracts the year of production; useful for clarity
sJSONYear = ("("++) . (++")") . flatten . intersperse "-" . splitOn "&ndash;" . stripShit . dropWhile (/=':') . head . filter (isPrefixOf "\"year_title\": \"")

sJSONCast :: [String] -> [Actor]  -- Extracts the cast of the show
sJSONCast = filter (not . null) . map stripShit . splitOn ", " . stripShit . dropWhile (/=':') . head . filter (isPrefixOf "\"cast\": \"")

allDetails' :: [String] -> Details    -- Adding the year to the title
allDetails' s = (sJSONTitle s ++ " " ++ sJSONYear s, sJSONCast s)

allDetails :: IO [Details]  -- Returning a list of all shows, in a tuple of the form Details
allDetails = sJSONShows >>= return . rmDups . filter (\(s, as) -> not (elem s excludedShows) && length as > 1) . map allDetails'  -- Get rid of any show with < 2 recorded Actors

---------------------------------------------------------------------------------------------------------------------------------------------------------------
-- Finally, using everything above here, we can get two Actors, and return a printed String with the shortest link between them.
---------------------------------------------------------------------------------------------------------------------------------------------------------------
allActors :: [Details] -> [Actor]
allActors = rmDups . flatten . map snd

allFellows :: Actor -> [Details] -> [Actor] -- A helper, a function to generate a list of every Actor one specific Actor has ever worked with.
allFellows a = filter (/=a) . allActors . filter ((elem a) . snd)

fellowAdj' :: [Adj] -> [Details] -> [Actor] -> [Adj]  -- fellowAdj' takes a list of Adjs, and a list of explored Actors, and goes through each Adj generating
fellowAdj' [] _ _             = []                    -- a list of new Adjs based on the head of the Actor list of each one
fellowAdj' ((a, i):as) d done = let newFellows = [n | n <- allFellows (head a) d, not (elem n (done ++ a))]
                                in [(new:a, i+1) | new <- newFellows] ++ fellowAdj' as d (newFellows ++ done)

fellowAdj :: [Adj] -> [Details] -> [Actor] -> [Adj] -- fellowAdj is then a recursive function that takes the list generated by fellowAdj'
fellowAdj [] _ _    = []                            -- and reapplies fellowAdj' to that list, appending it to the first list
fellowAdj as d done = let newList = fellowAdj' as d done
                      in newList ++ fellowAdj newList d (map (head . fst) newList ++ done)

allAdj :: Actor -> [Details] -> [Adj]       -- And allAdj takes fellowAdj and wraps it all up neatly
allAdj a d = let baa = [([a], 0)]           -- turn the Actor into the most basic Adj first
             in baa ++ fellowAdj baa d []   -- then recursively generate the list of all Adjs

adjLim :: Actor -> Int -> [Details] -> [Adj]  --allAdj is in theory an infinite list, so we use adjLim to limit it
adjLim a l d = takeWhile ((<=l) . snd) (allAdj a d)

adjSearch :: Actor -> Actor -> [Details] -> Adj
adjSearch a1 a2 d = let alList = (filter ((==a1) . head . fst) . adjLim a2 limit) d  -- Filters the list of all Adjs from a2 to just those with a1 at the head
                    in if null alList then ([a1,a2], 1000) else head alList         -- If that list is empty: error code, otherwise return the head

adjCheck :: Actor -> Actor -> [Details] -> Adj          -- adjCheck is basically input validation; it makes sure both Actors actually have records.
adjCheck a1 a2 d                                        -- If they don't it returns an error code in the result, otherwise it runs adjSearch.
  | not (elem a1 as || elem a2 as)  = ([a1,a2], -3)
  | not (elem a2 as)                = ([a1,a2], -2)
  | not (elem a1 as)                = ([a1,a2], -1)
  | otherwise                       = adjSearch a1 a2 d
  where as = allActors d

link :: Actor -> Actor -> [Details] -> String -- link takes two actors and a list of Details and finds the link between the actors
link a1 a2 d = "- " ++ a1 ++ " was in " ++ (fst . head . filter (\(_, as) -> elem a1 as && elem a2 as)) d ++ " with " ++ a2

links :: [Actor] -> [Details] -> String -- links then applies this across a list of Actors, doing them two at a time
links (a1:a2:[]) d = link a1 a2 d
links (a1:a2:as) d = link a1 a2 d ++ "\n" ++ links (a2:as) d

ppAdjCheck :: Actor -> Actor -> [Details] -> String -- Finally for the non-IO portion of this bit, ppAdjCheck takes the Actor names and the Detail list,
ppAdjCheck a1 a2 d
  | i == -3   = hl ++ " are not Actors with records."
  | i == -2   = last as ++ " is not an Actor with a record."
  | i == -1   = head as ++ " is not an Actor with a record."
  | i == 0    = head as ++ " has 0 degrees of separation with themself by definition."
  | i == 1000 = hl ++ " are either not linked, or have more than " ++ show limit ++ " degrees of separation."
  | otherwise = let deg = if i == 1 then " degree " else " degrees "
                          in hl ++ " have " ++ show i ++ deg ++ "of separation, and are linked as follows:\n" ++ links as d
  where (as, i) = adjCheck a1 a2 d
        hl = head as ++ " and " ++ last as

sixDegs :: Actor -> Actor -> IO ()                          -- sixDegs is where the IO () starts; it feeds showDetails into ppAdjCheck
sixDegs a1 a2 = allDetails >>= putStrLn . ppAdjCheck a1 a2

--main :: IO ()           --main takes two getLines and returns sixDegs with them as input
--main = do a1 <- getLine
--          a2 <- getLine
--          sixDegs a1 a2

ppAdj :: Adj -> String
ppAdj (as, i) = (flatten . intersperse " -> ") as ++ "\n"

main = do ad <- allDetails
          let str = (map ppAdj . sortBy (comparing snd) . flatten . map (\a -> adjLim a limit ad) . allActors) ad
          putStrLn $ (flatten str) ++ "\n" ++ (show . length) str
          writeFile adjFile $ flatten str

ppAllDetails' = putStr . flatten . map (\(s, as) -> s ++ "\n" ++ replicate (length s) '-' ++ "\n" ++ flatten ["  | " ++ a ++ "\n" | a <- as] ++ replicate 64 '=' ++ "\n")

ppAllDetails :: IO ()
ppAllDetails = allDetails >>= ppAllDetails'

readAdjs :: IO [Adj]
readAdjs = myReadFile adjFile >>= return . map ((\l -> (l, (length l) - 1)) . splitOn " -> ") . lines
