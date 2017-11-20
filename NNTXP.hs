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
type Role = String
type PersonDetails = (Actor, [(ShowName, [Role])])

---------------------------------------------------------------------------------------------------------------------------------------------------------------
-- A few test variables now:
-- - limit is how far it should go before giving up on finding a link
-- - showsPath is where the shows are in my copy of the history-project repo
-- - excludedShows is shows that are not taken into account
-- - And myself and some people as test cases for the actual degree-finder
---------------------------------------------------------------------------------------------------------------------------------------------------------------
peopleJSON :: FilePath
peopleJSON = "people-collect.json"

roleVal :: Role -> Int
roleVal s
  | s == "Director"                               = 100
  | s == "Executive Producer"                     = 100
  | s == "Producer"                               = 80
  | s == "Acting"                                 = 60
  | s == "Technical Director"                     = 60
  | s == "Tech Master"                            = 60    -- fucking CrystalQuest
  | s == "Lighting Designer"                      = 40
  | s == "Lighting Design"                        = 40
  | s == "Set Design"                             = 30
  | s == "Set Design / Construction"              = 30
  | s == "Set Designer"                           = 30
  | s == "Sound Designer"                         = 30
  | s == "Venue Technician"                       = 20
  | s == "Projection Design"                      = 20
  | s == "Publicity Manager"                      = 20
  | s == "FFDirecting"                            = 20
  | s == "Publicity Designer"                     = 15
  | s == "Poster Designer"                        = 15
  | s == "Poster Design"                          = 15
  | isInfixOf "Video" s                           = 10
  | s == "Musician"                               = 10
  | s == "Accent Coach"                           = 10
  | s == "Sound"                                  = 10
  | s == "Production Assistant"                   = 10
  | s == "Costume Designer"                       = 10
  | s == "Costumes"                               = 10
  | s == "Hair and Make-Up"                       = 10
  | s == "Make Up / Costumes"                     = 10
  | s == "Make Up Artist / Hair"                  = 10
  | s == "Make Up Artist/Costumes/Blood Effects"  = 10
  | s == "Make-Up Assistant"                      = 10
  | s == "Make-Up Supervisor"                     = 10
  | s == "Make-up Designer"                       = 10
  | s == "Make-up and Masks"                      = 10
  | s == "Make-Up"                                = 10
  | s == "Make-up"                                = 10
  | s == "Make Up Artist"                         = 10
  | s == "FFActing"                               = 10
  | s == "Photography"                            = 10
  | s == "Set Construction"                       = 5
  | s == "Design Assistant"                       = 5
  | s == "Stage Manager"                          = 5
  | s == "Technical Operator"                     = 5
  | otherwise                                     = 0


---------------------------------------------------------------------------------------------------------------------------------------------------------------
-- Helpers!
---------------------------------------------------------------------------------------------------------------------------------------------------------------
flatten :: [[a]] -> [a]                     -- Flattening lists of lists
flatten ass = [a | as <- ass, a <- as]
rmDups :: (Eq a, Ord a) => [a] -> [a]       -- Removing duplicate entries in a sortable list
rmDups = map head . group . sort
myReadFile :: FilePath -> IO String
myReadFile = fmap T.unpack . TIO.readFile
stripShit :: String -> String   -- Stripping out any characters that might surround an actor or show's name
stripShit s                     -- Whitespace, quotation marks, colons, etc.
 | hs == ' ' || hs == '\"' || hs == '\'' || hs == '[' || hs == ':' || hs == '\\' = stripShit (tail s)
 | ls == ' ' || ls == '\"' || ls == '\'' || ls == ']' || ls == ',' || ls == '\\' = stripShit (init s)
 | otherwise                                                                     = s
 where hs = head s
       ls = last s

---------------------------------------------------------------------------------------------------------------------------------------------------------------
-- First we've to get the 
---------------------------------------------------------------------------------------------------------------------------------------------------------------
jsonData :: IO [[String]]
jsonData = myReadFile peopleJSON >>= return . filter (elem "        \"type\": \"person\",") . map lines . splitOn "    \n\n    ,\n"

extractRoles = flatten . map (map stripShit . splitOn "," . dropWhile (/='['))
getRoles' = extractRoles . filter (isPrefixOf "                \"roles\": [") . takeWhile (not . isPrefixOf "        \"committees\": [")
getRoles = jsonData >>= return . map getRoles'

getName' = extractName . head . filter (isPrefixOf "        \"name\": ")
extractName = stripShit . dropWhile (/=':')
getNames = jsonData >>= return . map getName'

getRoleVals = getXP . getRoles'

nameRoles l = (getName' l, getRoles' l)
nameRolesVals (a, rs) = (a, getXP rs)

getXP' [] flag = 0
getXP' (r:rs) flags
  | isPrefixOf "Shadow" r || isPrefixOf "Assistant" r = getXP' rs (r:flags)
  | elem ("Shadow " ++ r) flags = 2*(roleVal r) + getXP' rs (filter (/= "Shadow " ++ r) flags)
  | elem ("Assistant " ++ r) flags = 2*(roleVal r) + getXP' rs (filter (/= "Assistant " ++ r) flags)
  | otherwise = roleVal r + getXP' rs flags

getXP rs = getXP' rs []

namesRoles = jsonData >>= return . map nameRoles

findOne' :: String -> [[String]] -> [String]
findOne' a = head . filter (\ls -> a == getName' ls)

oneXP :: Actor -> [[String]] -> (Actor, Int)
oneXP a = nameRolesVals . nameRoles . findOne' a

findOne a = jsonData >>= return . oneXP a

main = getLine >>= findOne

everyone = jsonData >>= return . sortBy (comparing snd) . map (nameRolesVals . nameRoles)


zeroRoles = namesRoles >>= putStrLn . flatten . intersperse "\n" . map (show. headLength) . sortBy (comparing length) . group . sort . filter (\r -> (r/="null") && (roleVal r == 0)) . flatten . map snd
            where headLength as = (head as, length as)
