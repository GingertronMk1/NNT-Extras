----------------------------------------------------------------------------------------------------------------------------------------------------------------
We're first going to import some things:
- Data.List for isInfixOf, sort, and group
- Data.Ord for sorting fun times
- System.Directory so we can muck about with files and directories
- Data.Char for intToDigit
- And finally Data.Text and Data.Text.IO for stricter file reading
----------------------------------------------------------------------------------------------------------------------------------------------------------------

> import Data.List
> import Data.Ord
> import System.Directory
> import Data.Char
> import qualified Data.Text as T
> import qualified Data.Text.IO as TIO

--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
Now defining some data types:
- And Actor and ShowName for type clarity
- [Detail] for the list we're going to generate that contains all the important bits of a show
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
- Finally, a test tree for demonstrating printing things
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

--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
Helpers!
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

> flatten :: [[a]] -> [a]
> flatten ass = [a | as <- ass, a <- as]

> rmdups :: (Eq a, Ord a) => [a] -> [a]
> rmdups = map head . group . sort

--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
First we need to build a list of all of the shows that have records on the history site
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

Surprisingly enough, this isn't that many lines. First we get all of the contents of the directory where the shows are kept
Then we drop the first 2 (`.` and `..`), and to that list we map the prepending of the showsPath and the appending of a `/` because filepaths
We also map a little functions that extracts the contents of a directory (in this case the files themselves), and prepends the containing folder
And that is the filepath for all of the shows that have records at the NNT

> allShows :: IO [IO [FilePath]]
> allShows = do baseDir <- getDirectoryContents showsPath
>               return $ map (getDirContentsPrep . (\s -> showsPath ++ s ++ "/")) (drop 2 baseDir)
>               where getDirContentsPrep s = do contents <- getDirectoryContents s
>                                               return $ map (s++) (drop 2 contents)

--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
Now that we've got a list of all of the shows, we need to extract from it a list of all actors.
First we're going to extract just the actors from a single show, as such:
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

> filterPeople :: [String] -> [String]
> filterPeople = filter (isInfixOf " name:") . dropWhile (\s -> not (isInfixOf "cast:" s)) . takeWhile (\s -> not (isInfixOf "crew:" s)) 

Next, a helper to remove anything that isn't someone's name in the line
That is, trailing/leading non-letter characters
Basically my problem is that people's names are formatted incredibly inconsistently on the History Site

> stripShit :: String -> String
> stripShit s
>  | hs == ' ' || hs == '\"' || hs == '\'' || hs == ':' = stripShit (tail s)
>  | ls == ' ' || ls == '\"' || ls == '\''              = stripShit (init s)
>  | otherwise                                          = s
>  where hs = head s
>        ls = last s

> getString :: String -> String
> getString = stripShit . dropWhile (/= ':')

With that, we can extract just the name from the string

> getNames :: [String] -> [Actor]
> getNames = map getString . filterPeople

Also we can use them to get the title as well, which is nice

> getTitle :: [String] -> String
> getTitle = getString . head . filter (isInfixOf "title:")

Applying these, we can extract the details from a specific file

> showDetails :: FilePath -> IO Detail
> showDetails s = do fileContents <- (fmap T.unpack . TIO.readFile) s
>                    let fileLines = lines fileContents
>                    return (getTitle fileLines, getNames fileLines)

And finally, we can map this across all of the shows (i.e. that list we generated with `allShows`)
We discount anything that's not a MarkDown file, is a Freshers' Fringe (otherwise this gets very dull), and any show with fewer than 2 actors

> allShowDetails :: IO [Detail]
> allShowDetails = do allDirs' <- allShows
>                     allDirs <- sequence allDirs'
>                     allDT <- (sequence . map showDetails . filter (\s -> isInfixOf ".md" s && not (isInfixOf "freshers_fringe" s)) . flatten) allDirs
>                     return $ filter (\s -> length (snd s) > 1) allDT

--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
Finally, using everything above here, we can get two Actors, and return a printed String with the shortest link between them.
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

> allActors :: [Detail] -> [Actor]
> allActors d = (rmdups . flatten . map snd) d

> allFellows :: Actor -> [Detail] -> [Actor]
> allFellows a dt = (filter (/=a) . rmdups . flatten . filter (elem a) . map snd) dt

> baseAdj :: Actor -> [Adj]
> baseAdj a = [([a], 0)]

> fellowAdj' [] _ _               = []
> fellowAdj' ((a, i):adjs) d done = [(new:a, i+1) | new <- newFellows] ++ fellowAdj' adjs d (newFellows ++ done)
>                                   where newFellows = [nf | nf <- allFellows (head a) d, not (elem nf done || elem nf a)]

> fellowAdj :: [Adj] -> [Detail] -> [Actor] -> [Adj]
> fellowAdj [] _ _    = []
> fellowAdj as d done = newList ++ fellowAdj newList d (done ++ newDone)
>                         where newList = fellowAdj' as d done
>                               newDone = map (head . fst) newList

> allAdj :: Actor -> [Detail] -> [Adj]
> allAdj a d = baa ++ fellowAdj baa d []
>              where baa = baseAdj a

> adjLim :: Actor -> [Detail] -> [Adj]
> adjLim a d = takeWhile ((<= limit) . snd) (allAdj a d)

> adjSearch :: Actor -> Actor -> [Detail] -> Adj
> adjSearch a1 a2 d = if null alList then ([a1,a2], 1000) else head alList
>                     where alList = (filter ((== a2) . head . fst)) (adjLim a1 d)

> adjCheck :: Actor -> Actor -> [Detail] -> Adj
> adjCheck a1 a2 d
>   | not (elem a1 aa || elem a2 aa)  = ([a1,a2], -3)
>   | not (elem a2 aa)                = ([a1,a2], -2)
>   | not (elem a1 aa)                = ([a1,a2], -1)
>   | a1 == a2                        = ([a1],     0)
>   | otherwise                       = adjSearch a2 a1 d
>   where aa = allActors d

> links :: [Actor] -> [Detail] -> String
> links (a1:a2:as) dt = if as == [] then str else str ++ links (a2:as) dt
>                          where str = "- " ++ a1 ++ " was in " ++ findLink a1 a2 ++ " with " ++ a2 ++ "\n"
>                                findLink a1 a2 = (fst . head . filter ((\s -> elem a1 s && elem a2 s) . snd)) dt

> ppAdjCheck :: Actor -> Actor -> [Detail] -> String
> ppAdjCheck a1 a2 d
>   | i == -3   = headAndLast ++ " are not Actors with records."
>   | i == -2   = last as ++ " is not an Actor with a record."
>   | i == -1   = head as ++ " is not an Actor with a record."
>   | i == 0    = head as ++ " has 0 degrees of separation with themself by definition."
>   | i == 1000 = headAndLast ++ " are not linked."
>   | otherwise = headAndLast ++ " are linked as follows:\n" ++ links as d ++ "\nThey have " ++ [intToDigit i] ++ " degrees of separation."
>   where (as, i) = adjCheck a1 a2 d
>         headAndLast = head as ++ " and " ++ last as

> main' :: Actor -> Actor -> IO ()
> main' a1 a2 = allShowDetails >>= (\d -> putStrLn $ ppAdjCheck a1 a2 d)

> main :: IO ()
> main = do a1 <- getLine
>           a2 <- getLine
>           main' a1 a2

> ppAdj :: Adj -> String
> ppAdj (as, i)
>   | i == 1000 = ""
>   | otherwise = "([" ++ ((flatten . intersperse ", ") as) ++ "], " ++ [intToDigit i] ++ ")\n"

> allCombos d = [(a1,a2) | a1 <- allActors d, a2 <- allActors d, a1 /= a2]

> alTest a = allShowDetails >>= (\d -> (return . length) $ adjLim a d)
> actLength = allShowDetails >>= (\d -> (return . length) (allActors d))

> test a = allShowDetails >>= (\d -> (return . length) $ adjLim a d)

-> test = allShowDetails >>= (\d -> (putStrLn . flatten . map (ppAdj . (\(a1,a2) -> adjCheck a1 a2 d))) (allCombos d))
