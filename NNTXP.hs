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
-- - Actor and Role for type clarity, otherwise it'd be lots of `String -> String` going on
-- - PersonDetails: a tuple containing an Actor and a list of all the Roles they've had
---------------------------------------------------------------------------------------------------------------------------------------------------------------
type Actor = String
type Role = String
type PersonDetails = (Actor, [Role])

---------------------------------------------------------------------------------------------------------------------------------------------------------------
-- A few test variables now:
-- - peopleJSON is the location of the JSON file from which the information is taken
-- - roleVal is a list of roles and the XP score they have
---------------------------------------------------------------------------------------------------------------------------------------------------------------
peopleJSON :: FilePath
peopleJSON = "people-collect.json"

roleVal :: Role -> Int
roleVal s
  | s == "Director"                               = 100
  | s == "Executive Producer"                     = 100
  | s == "Producer"                               = 80
  | s == "Technical Director"                     = 60
  | s == "Tech Master"                            = 60    -- fucking CrystalQuest
  | s == "Lighting Designer"                      = 40
  | s == "Lighting Design"                        = 40
  | s == "Set Design"                             = 30
  | s == "Set Design / Construction"              = 30
  | s == "Set Designer"                           = 30
  | s == "Sound Designer"                         = 30
  | s == "Musical Director"                       = 30
  | s == "Venue Technician"                       = 20
  | s == "Projection Design"                      = 20
  | s == "Publicity Manager"                      = 20
  | s == "Assistant Director"                     = 15
  | s == "Publicity Designer"                     = 15
  | s == "Poster Designer"                        = 15
  | s == "Poster Design"                          = 15
  | isInfixOf "Video" s                           = 10
  | s == "Musician"                               = 10
  | s == "Accent Coach"                           = 10
  | s == "Sound"                                  = 10
  | s == "Production Assistant"                   = 10
  | s == "Assistant Producer"                     = 10
  | s == "Costume Designer"                       = 10
  | s == "Costumes"                               = 10
  | s == "Hair and Make-Up"                       = 10
  | s == "Hair & Make-Up"                         = 10
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
  | s == "Designer"                               = 5
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

extractRoles :: [String] -> [Role]
extractRoles = flatten . map (map stripShit . splitOn "," . dropWhile (/='['))

getRoles' :: [String] -> [Role]
getRoles' = extractRoles . filter (isPrefixOf "                \"roles\": [") . takeWhile (not . isPrefixOf "        \"committees\": [")

getXP' :: [Role] -> [Role] -> Int
getXP' [] flags                                       = 0
getXP' (r:rs) flags
  | isPrefixOf "Shadow" r           = getXP' rs (r:flags)
  | elem ("Shadow " ++ r) flags     = 2*(roleVal r) + getXP' rs (filter (/= "Shadow " ++ r) flags)
  | otherwise                       = roleVal r + getXP' rs flags

getXP :: [String] -> Int
getXP rs = getXP' rs []

getRoleVals :: [String] -> Int
getRoleVals = getXP . getRoles'

getName' :: [String] -> Actor
getName' = extractName . head . filter (isPrefixOf "        \"name\": ")

allNames = jsonData >>= return . map getName'

extractName :: String -> Actor
extractName = stripShit . dropWhile (/=':')

nameRoles :: [String] -> PersonDetails
nameRoles l = (getName' l, getRoles' l)

nameRolesVals :: (Actor, [Role]) -> (Actor, Int)
nameRolesVals (a, rs) = (a, getXP rs)


namesRoles :: IO [PersonDetails]
namesRoles = jsonData >>= return . map nameRoles

main'' :: String -> [[String]] -> [String]
main'' a = head . filter (\ls -> a == getName' ls)

oneXP :: Actor -> [[String]] -> (Actor, Int)
oneXP a d = if elem a (map getName' d) then (nameRolesVals . nameRoles . main'' a) d else (a, 0)

main' :: Actor -> IO (Actor, Int)
main' a = jsonData >>= return . oneXP a

mains' :: [Actor] -> IO [(Actor, Int)]
mains' as = jsonData >>= (\d -> return (map (\a -> oneXP a d) as))

main :: IO (Actor, Int)
main = getLine >>= main'

---------------------------------------------------------------------------------------------------------------------------------------------------------------
-- Other things here
---------------------------------------------------------------------------------------------------------------------------------------------------------------


everyone = jsonData >>= return . sortBy (comparing snd) . map (nameRolesVals . nameRoles)


zeroRoles = namesRoles >>= putStrLn . flatten 
            . intersperse "\n" 
            . map (show . headLength) 
            . sortBy (comparing length) 
            . group 
            . sort 
            . filter (\r -> (not (isPrefixOf "Shadow" r)) && (r/="null") && (roleVal r == 0)) 
            . flatten 
            . map snd
            where headLength as = (head as, length as)
