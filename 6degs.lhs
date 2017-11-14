----------------------------------------------------------------------------------------------------------------------------------------------------------------
We're first going to import some things:
- Data.List for isInfixOf, sort, and group
- System.Directory so we can muck about with files and directories
- Data.Ord for comparing, and more interesting sorting
- And Data.Text and Data.Text.IO for stricter file reading
----------------------------------------------------------------------------------------------------------------------------------------------------------------

> import Data.List
> import System.Directory
> import Data.Ord
> import qualified Data.Text as T
> import qualified Data.Text.IO as TIO

--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
Now defining some data types:
- Actor and ShowName for type clarity, otherwise it'd be lots of `String -> String` going on
- [Detail] for the list we're going to generate that contains all the important bits of a show
- And Adj, a sort of adjacency list used in the actual finding of the degrees of separation
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

> type Actor = String
> type ShowName = String
> type Detail = (ShowName, [Actor])
> type Adj = ([Actor], Int)

--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
A few test variables now:
- limit is how far it should go before giving up on finding a link
- showsPath is where the shows are in my copy of the history-project repo
- And myself and some people as test cases for the actual degree-finder
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

> limit :: Int
> limit = 1000
> showsPath :: String
> showsPath = "../history-project/_shows/"
> me :: Actor
> me = "Jack Ellis"
> fr :: Actor
> fr = "Fran Roper"
> br :: Actor
> br = "????na Brown"
> testAdj1 = ([me, fr, "Ian Sheard"], 2)
> testAdj2 = (["Ian Sheard", "Sam Peake"], 1)

--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
Helpers!
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

Flattening lists of lists

> flatten :: [[a]] -> [a]
> flatten ass = [a | as <- ass, a <- as]

Removing duplicate entries in a sortable list

> rmDups :: (Eq a, Ord a) => [a] -> [a]
> rmDups = map head . group . sort

--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
First we need to build a list of all of the shows that have records on the history site
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

First we get all of the contents of the directory where the shows are kept.
Then we drop anything that starts with a '.', and to that list we map the prepending of the showsPath and the appending of a `/` because filepaths.
We also map a little functions that extracts the contents of a directory (in this case the files themselves), and prepends the containing folder.
And that is the filepath for all of the shows that have records at the NNT.

> allFiles :: IO [FilePath]
> allFiles = do baseDir <- getDirectoryContents showsPath
>               showsInDirs <- (sequence . map (getDirContentsWPrep . (\s -> showsPath ++ s ++ "/"))) (dropDots baseDir)
>               (return . flatten) showsInDirs
>               where getDirContentsWPrep s = getDirectoryContents s >>= (\c -> (return . map (s++)) (dropDots c))
>                     dropDots = filter (not . isPrefixOf ".")

--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
Now that we've got a list of all of the shows, we need to extract from it a list of all actors.
First we're going to extract just the actors from a single show, as such:
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

Filtering people; taking just the chunk of text that relates to the actors in the plays.

> filterPeople :: [String] -> [String]
> filterPeople = filter (isInfixOf " name:") . dropWhile (not . isInfixOf "cast:") . takeWhile (not . isInfixOf "crew:")

Next, a helper to remove anything that isn't someone's name in the line.
That is, leading/trailing non-letter characters.
Basically my problem is that people's names are formatted incredibly inconsistently on the History Site.

> stripShit :: String -> String
> stripShit s
>  | hs == ' ' || hs == '\"' || hs == '\'' || hs == ':' = stripShit (tail s)
>  | ls == ' ' || ls == '\"' || ls == '\''              = stripShit (init s)
>  | otherwise                                          = s
>  where hs = head s
>        ls = last s

We can use this in conjunction with dropping until it hits a ':' to extract just the string we're after

> getString :: String -> String
> getString = stripShit . dropWhile (/= ':')

Using these, we can extract a show's title from the lines of the file that contains its details

> getTitle :: [String] -> ShowName
> getTitle = getString . head . filter (isInfixOf "title:")

We can also extract the names of all of the actors in the show

> getNames :: [String] -> [Actor]
> getNames = map getString . filterPeople

We can then combine these to take the lines of the containing file and return the Detail of it

> titleAndNames :: [String] -> Detail
> titleAndNames l = (getTitle l, getNames l)

Applying these, we can extract the details from a specific file

> showDetails :: FilePath -> IO Detail
> showDetails s = ((fmap T.unpack . TIO.readFile) s) >>= (return . titleAndNames . lines)

Applying /that/, we can map it to the list of all files we generated earlier

> allDetails :: IO [Detail]
> allDetails = allFiles >>= (\files -> (sequence . map showDetails) [f | f <- files, isInfixOf ".md" f, not (isInfixOf "freshers_fringe" f), not (isInfixOf "charity_gala" f)]) -- uncomment this line to exclude Freshers' Fringes & Charity Gala
>-- allDetails = allFiles >>= (\files -> (sequence . map showDetails) [f| f <- files, isInfixOf ".md" f])  -- uncomment this line to include Freshers' Fringes & Charity Gala

--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
Finally, using everything above here, we can get two Actors, and return a printed String with the shortest link between them.
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

First, a brief function to turn an Actor into the most basic possible Adj WRT that Actor, i.e. themself and a degree of 0

> baseAdj :: Actor -> [Adj]
> baseAdj a = [([a], 0)]

Then a helper, a function to generate a list of every Actor one specific Actor has ever worked with

> allActors :: [Detail] -> [Actor]
> allActors = rmDups . flatten . map snd

> allFellows :: Actor -> [Detail] -> [Actor]
> allFellows a = filter (/=a) . rmDups . flatten . filter (elem a) . map snd

fellowAdj' takes a list of Adjs, and a list of explored Actors, and goes through each Adj generating a list of new Adjs based on the head of the Actor list of each one.

> fellowAdj' :: [Adj] -> [Detail] -> [Actor] -> [Adj]
> fellowAdj' [] _ _               = []
> fellowAdj' ((a, i):adjs) d done = [(new:a, i+1) | new <- newFellows] ++ fellowAdj' adjs d (newFellows ++ done)
>                                   where newFellows = [n | n <- allFellows (head a) d, not (elem n done || elem n a)]

fellowAdj is then a recursive function that takes the list generated by fellowAdj' and reapplies fellowAdj' to that list, appending it to the first list.

> fellowAdj :: [Adj] -> [Detail] -> [Actor] -> [Adj]
> fellowAdj [] _ _    = []
> fellowAdj as d done = newList ++ fellowAdj newList d (map (head . fst) newList ++ done)
>                       where newList = fellowAdj' as d done

And allAdj takes fellowAdj and wraps it all up neatly so all you've to do is supply an Actor and a Detail list

> allAdj :: Actor -> [Detail] -> [Adj]
> allAdj a d = baa ++ fellowAdj baa d []
>              where baa = baseAdj a

allAdj is in theory an infinite list, so we use adjLim to limit it. Shock horror.
At the moment this is purely academic, as the number of Actors with records is less than the current value for 'limit'.

> adjLim :: Actor -> [Detail] -> Int -> [Adj]
> adjLim a d l = takeWhile ((<=l) . snd) (allAdj a d)

adjSearch now takes the list generated by adjLim and if no list starts with the searched Actor, returns the two Actors and 1000 as ostensibly an error message.
If it does hit, it returns that Adj.

> adjSearch :: Actor -> Actor -> [Detail] -> Adj
> adjSearch a1 a2 d = if null alList then ([a1,a2], 1000) else head alList
>                     where alList = (filter ((== a1) . head . fst)) (adjLim a2 d limit)

adjCheck is basically input validation; it makes sure both Actors actually have records. If they don't it returns an error code in the result, otherwise it runs adjSearch.

> adjCheck :: Actor -> Actor -> [Detail] -> Adj
> adjCheck a1 a2 d
>   | not (elem a1 aa || elem a2 aa)  = ([a1,a2], -3)
>   | not (elem a2 aa)                = ([a1,a2], -2)
>   | not (elem a1 aa)                = ([a1,a2], -1)
>   | otherwise                       = adjSearch a1 a2 d
>   where aa = allActors d

`links` is a helper function that takes a list of Actors and a list of Details and uses those to find the Shows that link each pair of Actors.

> links :: [Actor] -> [Detail] -> String
> links (a1:a2:as) d = case as of []        -> str
>                                 otherwise -> str ++ "\n" ++ links (a2:as) d
>                      where str = "- " ++ a1 ++ " was in " ++ link a1 a2 d ++ " with " ++ a2
>                            link a1 a2 = fst . head . filter ((\s -> elem a1 s && elem a2 s) . snd)

Finally for the non-IO portion of this bit, ppAdjCheck takes the Actor names and the Detail list, performs adjCheck on them, and returns the appropriate String

> ppAdjCheck :: Actor -> Actor -> [Detail] -> String
> ppAdjCheck a1 a2 d
>   | i == -3   = headAndLast ++ " are not Actors with records."
>   | i == -2   = last as ++ " is not an Actor with a record."
>   | i == -1   = head as ++ " is not an Actor with a record."
>   | i == 0    = head as ++ " has 0 degrees of separation with themself by definition."
>   | i == 1000 = headAndLast ++ " are either not linked, or have more than " ++ show limit ++ " degrees of separation."
>   | otherwise = headAndLast ++ " have " ++ show i ++ " degrees of separation, and are linked as follows:\n" ++ links as d
>   where (as, i) = adjCheck a1 a2 d
>         headAndLast = head as ++ " and " ++ last as

main' is where the IO starts; it feeds showDetails into ppAdjCheck and putStrLn's the resultant String so we get nice '\n' newlines

> main' :: Actor -> Actor -> IO ()
> main' a1 a2 = allDetails >>= (putStrLn . ppAdjCheck a1 a2)

main takes two getLines and returns main' with them as input 

> main :: IO ()
> main = do a1 <- getLine
>           a2 <- getLine
>           main' a1 a2

--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
EVERYTHING BELOW HERE IS JUST ME PLAYING WITH NNT STATISTICS
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

> allCombos'' []     = []
> allCombos'' (a:as) = [(a, b) | b <- as] ++ allCombos'' as
> allCombos' = allCombos'' . allActors

> allCombos = allDetails >>= (return . length . allCombos')

> allCombosDone = allDetails >>= (\d -> (writeFile "Adjs.txt" . ppAdj . map (\(a1,a2) -> adjCheck a1 a2 d)) (allCombos' d))

> adjPrint' :: Adj -> String
> adjPrint' (as, i) = "\"" ++ head as ++ "\",\"" ++ last as ++ "\"\n"
> adjPrint :: [Adj] -> String
> adjPrint as = (flatten . map adjPrint') as
> ppAdj' :: Adj -> String
> ppAdj' (a, i) = "([" ++ flatten (intersperse ", " a) ++ "], " ++ show i ++ ")\n"
> ppAdj :: [Adj] -> String
> ppAdj = flatten . map ppAdj'

> allAdjs :: IO ()
> allAdjs = allDetails >>= (\d -> (writeFile "Adjs.txt" . adjPrint . filter ((>0) . snd) . flatten . map (\a -> adjLim a d 1)) (allActors d))

> showCount :: IO Int
> showCount = allFiles >>= (return . length)

> actorCount = allDetails >>= (return . length . allActors)

> biggestLink a = allDetails >>= (\d -> (return . maxTuple) $ adjLim a d limit)
>                 where maxTuple ((a,i):[]) = (a,i)
>                       maxTuple ((a1,i1):(a2,i2):as) = if i1 > i2 then maxTuple ((a1,i1):as) else maxTuple ((a2,i2):as)
