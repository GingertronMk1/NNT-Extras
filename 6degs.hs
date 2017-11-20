---------------------------------------------------------------------------------------------------------------------------------------------------------------
-- We're first going to import some things:
-- - Data.List for isInfixOf, sort, and group (isInfixOf is used so much in this)
-- - Data.Ord for comparing, and more interesting sorting
-- - System.Directory so we can muck about with files and directories
-- - And Data.Text and Data.Text.IO for stricter file reading
---------------------------------------------------------------------------------------------------------------------------------------------------------------
import Data.List
import Data.Ord
import System.Directory
import Data.List.Split
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
type Role = String
type PersonDetails = (Actor, [(ShowName, [Role])])

---------------------------------------------------------------------------------------------------------------------------------------------------------------
-- A few test variables now:
-- - limit is how far it should go before giving up on finding a link
-- - showsPath is where the shows are in my copy of the history-project repo
-- - excludedShows is shows that are not taken into account
-- - And myself and some people as test cases for the actual degree-finder
---------------------------------------------------------------------------------------------------------------------------------------------------------------
limit :: Int
limit = 1000
showsPath :: String
showsPath = "../history-project/_shows"
searchJSON :: FilePath
searchJSON = "search.json"
peopleJSON :: FilePath
peopleJSON = "people-collect.json"
excludedShows :: [String]
excludedShows = ["Freshers' Fringe","Charity Gala"]
me :: Actor
me = "Jack Ellis"
fr :: Actor
fr = "Fran Roper"

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
 | hs == ' ' || hs == '\"' || hs == '\'' || hs == ':' || hs == '[' = stripShit (tail s)
 | ls == ' ' || ls == '\"' || ls == '\'' || ls == ']' || ls == ',' = stripShit (init s)
 | otherwise                                                       = s
 where hs = head s
       ls = last s

---------------------------------------------------------------------------------------------------------------------------------------------------------------
-- What we can do instead of all that is generate the list of Details from a JSON file, like so:
---------------------------------------------------------------------------------------------------------------------------------------------------------------
sJSONShows :: IO [[String]]
sJSONShows = myReadFile searchJSON >>= return . filter (elem "        \"type\": \"show\",") . map (lines) . splitOn "\n    \n    \n\n    \n    ,"

allDetails' :: [String] -> Details
allDetails' s = (sJSONTitle s, sJSONCast s)
allDetails :: IO [Details]
allDetails = sJSONShows >>= return . filter (\(s,as) -> not (elem s excludedShows)) . map allDetails'

sJSONTitle = stripShit . dropWhile (/=':') . head . filter (isInfixOf "\"title\":")

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

---------------------------------------------------------------------------------------------------------------------------------------------------------------
-- ME FIDDLING WITH NNT STATISTICS
---------------------------------------------------------------------------------------------------------------------------------------------------------------
test = main' me fr
everyCombo = allDetails >>= putStrLn . ppAdj . sortBy (comparing snd) . everyCombo'
everyComboLength = allDetails >>= return . length . everyCombo'
everyCombo' d = (filter ((>0) . snd) . flatten . map (\a -> adjLim a d limit) . allActors) d
combosLengths = allDetails >>= (\d -> return [(a, length (adjLim a d limit)) | a <- allActors d])
everyActor = allDetails >>= return . allActors
allAdjs a = allDetails >>= return . allAdj a
ppAdj' :: Adj -> String
ppAdj' (a, i) = "([" ++ flatten (intersperse ", " a) ++ "], " ++ show i ++ ")\n"
ppAdj :: [Adj] -> String
ppAdj = flatten . map ppAdj'
showCount :: IO Int
showCount = allDetails >>= return . length
actorCount :: IO Int
actorCount = allDetails >>= return . length . allActors


---------------------------------------------------------------------------------------------------------------------------------------------------------------
-- NNTXP
---------------------------------------------------------------------------------------------------------------------------------------------------------------

jsonData :: IO [[String]]
jsonData = myReadFile peopleJSON >>= return . init . map lines . splitOn "    \n\n"
jsonData2 = myReadFile peopleJSON >>= return . head . map lines . splitOn "    \n\n"

jsonRolesTest = jsonData >>= return . sortBy (comparing fst) . map nameRoles

getName = stripShit . dropWhile (/=':') . head . filter (isPrefixOf "        \"name\": ")

stripTitle = stripShit . dropWhile (/=':')

stripRoles = map stripShit . splitOn "," . dropWhile (/=':')

jsonRoles' :: [String] -> [[Role]]
jsonRoles' [] = []
jsonRoles' (l:ls) = if isPrefixOf "                \"roles\": " l then (stripRoles l):(jsonRoles' ls)
                                                                  else jsonRoles' ls

jsonRoles = flatten . jsonRoles' . takeWhile (not . isPrefixOf "        \"committees\": ")

findActorRoles a ass = (head . filter (elem ("        \"name\": \"" ++ a ++ "\","))) ass

nameRoles :: [String] -> (Actor, [Role])
nameRoles ls = (getName ls, jsonRoles ls)

actorShows d s = (s, (map fst . filter (elem s . snd)) d)
allActorsShows d = map (actorShows d) (allActors d)
actorShows' :: IO [(Actor, [ShowName])]
actorShows' = allDetails >>= return . allActorsShows

addActing :: [(Actor, [ShowName])] -> (Actor, [Role]) -> (Actor, [Role])
addActing d (a, rs) = if filta == [] then (a, rs) else addActing' (head filta) (a, rs)
                        where filta = filter ((==a) . fst) d

addActing' :: (Actor, [ShowName]) -> (Actor, [Role]) -> (Actor, [Role])
addActing' (a1, []) (a2, rs) = (a2, rs)
addActing' (a1, s:ss) (a2, rs) = if s == "Freshers' Fringe" then addActing' (a1, ss) (a2, "FFActing":rs)
                                                            else addActing' (a1, ss) (a2, "Acting":rs)

nntXP2 a = do showDetails <- allDetails
              manDetails <- jsonData
              (return . addActing showDetails . nameRoles . findActorRoles a) manDetails


nntXP' :: [(Actor, [ShowName])] -> [(Actor, [Role])] -> [(Actor, Int)]
nntXP' ass arss = (sortBy (comparing snd) . map (personXP . addActing ass)) arss

nntXP = do showDeets <- allDetails
           peepDeets <- jsonData
           let ass = allActorsShows showDeets
           let arss = map nameRoles peepDeets
           return $ nntXP' (allActorsShows showDeets) (map nameRoles peepDeets)

nntXP1 a = nntXP >>= return . head . filter ((==a) . fst)


roleXP :: Role -> Int
roleXP s
  | s == "Director"                   = 100
  | s == "Executive Producer"         = 100
  | s == "Producer"                   = 80
  | s == "Acting"                     = 60
  | s == "Technical Director"         = 60
  | s == "Tech Master"                = 60    -- fucking CrystalQuest
  | s == "Lighting Designer"          = 40
  | s == "Lighting Design"            = 40
  | s == "Set Design"                 = 30
  | s == "Set Design / Construction"  = 30
  | s == "Set Designer"               = 30
  | s == "Sound Designer"             = 30
  | s == "Venue Technician"           = 20
  | s == "Projection Design"          = 20
  | s == "Publicity Manager"          = 20
  | s == "FFDirecting"                = 20
  | s == "Publicity Designer"         = 15
  | s == "Poster Designer"            = 15
  | s == "Poster Design"              = 15
  | isInfixOf "Video" s               = 10
  | s == "Musician"                   = 10
  | s == "Accent Coach"               = 10
  | s == "Sound"                      = 10
  | s == "Production Assistant"       = 10
  | s == "Costume Designer"           = 10
  | s == "Hair and Make-Up"           = 10
  | s == "Make-Up"                    = 10
  | s == "Make-up"                    = 10
  | s == "Make Up Artist"             = 10
  | s == "FFActing"                   = 10
  | s == "Set Construction"           = 5
  | s == "Design Assistant"           = 5
  | s == "Stage Manager"              = 5
  | s == "Technical Operator"         = 5
  | otherwise                         = 0

--roleMult :: Int -> Int
--roleMult n = round (1.5 * n)
roleMult n = 2 * n

getXP' [] flag = 0
getXP' (r:rs) flags
  | isPrefixOf "Shadow" r || isPrefixOf "Assistant" r = getXP' rs (r:flags)
  | elem ("Shadow " ++ r) flags = (roleMult . roleXP) r + getXP' rs (filter (/= "Shadow " ++ r) flags)
  | elem ("Assistant " ++ r) flags = (roleMult . roleXP) r + getXP' rs (filter (/= "Assistant " ++ r) flags)
  | otherwise = roleXP r + getXP' rs flags

getXP rs = getXP' rs []

personXP (n, rs) = (n, getXP rs)


nullRoles = jsonData >>= putStrLn . flatten . intersperse "\n" . map show . filter (\t -> not (isPrefixOf "Assistant" (fst t) || isPrefixOf "Shadow" (fst t))) .sortBy (comparing snd) . map (\g -> (head g, length g)) .group . sort . filter ((==0) . roleXP) . flatten . map (snd . nameRoles)
--nullRoles = jsonData >>= return . sort . filter ((==0) . roleXP) . flatten . map (snd . nameRoles)
