import Data.List
import Data.Char

----------------------------------------------------------------
-- CS 352 - Project 1
--
-- David Jablonski
--
----------------------------------------------------------------

----------------------------------------------------------------
-- function exec - reads the contents of the file "input.txt",
--   and creates an index of the words; it writes the result to
--   standard output
--
-- **** YOU SHOULD NOT HAVE TO CHANGE THIS FUNCTION ****
--
-- calling sequence:
--   exec
--
-- example:
--   (See example in the function 'createIndex'.)
--
-- status:
--   Incomplete.  Presently, it just echos the contents of the
--   input file to standard output, because the 'createIndex' function is
--   dummied up.
--
----------------------------------------------------------------
exec:: IO()
exec = 
  do input <- readFile "input.txt"
     putStr (computeGpas input)

----------------------------------------------------------------
-- function exec2 - reads the contents of the file "input.txt",
--   and creates an index of the words; it writes the result to
--   the file "output.txt"
--
-- **** YOU SHOULD NOT HAVE TO CHANGE THIS FUNCTION ****
--
-- calling sequence:
--   exec2
--
-- example:
--   (See example in the function 'createIndex'.)
--
-- status:
--   Incomplete.  Presently, it just echos the contents of the
--   input file to "output.txt", because the 'createIndex' function is
--   dummied up.
----------------------------------------------------------------
exec2:: IO()
exec2 = 
  do input <- readFile "input.txt"
     writeFile "output.txt" (computeGpas input)

----------------------------------------------------------------
----------------------------------------------------------------

--Main function for parsing, computing GPAs, and formatting results
computeGpas :: String -> String
computeGpas str = 
    let (checkedStr, errors) = errorCheck str
    in (format (consolidate (map tuplify (toWords checkedStr)))) ++ "\n" ++ (foldl combineLines "" errors) ++ "\n"


--UTILITY FUNCTIONS
--These are used to make some pieces of the code more readable and less verbose

--Splits a string by lines/whitespace into a list of lists of words
toWords :: String -> [[String]]
toWords str = map words (lines str)


--Finds the first element in a 3-tuple
fst3 :: (a, b, c) -> a
fst3 (a, b, c) = a


--Pads n characters to the right of the string
padRight :: Int -> String -> String
padRight n str = take n $ str ++ (repeat ' ')


--Combines two strings as two lines, removing empty lines
combineLines :: String -> String -> String
combineLines "" b = b
combineLines a "" = a
combineLines a b = a ++ "\n" ++ b


--ERROR CHECKING FUNCTIONS
--Since the domain is simple enough, we can get away with checking for errors before running the main function.

--Checks for errors in the input string
errorCheck :: String -> (String, [String])
errorCheck str = 
    let (checkedLines, lineErrors) = unzip (map checkLine (lines str))
        repeatClassErrors = findRepeatClasses checkedLines
        differentCreditErrors = findDifferentCredits checkedLines
        errors = lineErrors ++ repeatClassErrors ++ differentCreditErrors
    in  (foldl combineLines "" checkedLines, errors)


--Given lines with valid formatting, returns a list describing repeated classes taken by each student
findRepeatClasses :: [String] -> [String]
findRepeatClasses list = 
    let repeats = filter (\x -> matchClass "THE 102" x) list
    in  repeats

matchClass :: String -> String -> Bool
matchClass s1 s2 = 
    if s1 == "" || s2 == "" then False else
    let w1 = words s1
        w2 = words s2
        dept1 = w1 !! 0
        dept2 = w2 !! 0
        course1 = w1 !! 1
        course2 = w2 !! 1
        fname1 = w1 !! 4
        fname2 = w2 !! 4
        lname1 = w1 !! 5
        lname2 = w2 !! 5
    in  dept1 == dept2 && course1 == course2


--Given lines with valid formatting, returns a list describing varying credit amounts among the same class
findDifferentCredits :: [String] -> [String]
findDifferentCredits list = []


--Checks the validity of a single line
checkLine :: String -> (String, String)
checkLine str =
    let numArgsErrs = checkNumArgs str
        digitErrs = if numArgsErrs == "" then (checkNumArgs str) else ""
        gradeErrs = if numArgsErrs == "" then (checkGrade str) else ""
        subjectErrs = if numArgsErrs == "" then (checkSubject str) else ""
        foundErrors = foldl combineLines "" [numArgsErrs, digitErrs, gradeErrs, subjectErrs]
    in  if foundErrors == ""
            then (str, "")
            else ("", "Errors on the following line: \"" ++ str ++ "\"\n" ++ foundErrors ++ "\n")


--Checks that a line has the correct number of fields
checkNumArgs :: String -> String
checkNumArgs str = 
    if length (words str) == 7
    then ""
    else "Wrong number of args: " ++ (show (length (words str)))


--Verifies that a line's fields that should be digits in fact are
checkDigits :: String -> String
checkDigits str = 
    let [_, a, _, b] = words str
    in 
        if foldl (\x y -> x && (isAllDigits y)) True [a, b]
            then ""
        else "Expected the second and fourth words to be non-negative integers"


--Checks if all elements of a string are digits
isAllDigits :: String -> Bool
isAllDigits str = all (isDigit) str


--Verifies that the grade field is valid
checkGrade :: String -> String
checkGrade str = 
    let [_, _, _, _, _, _, grade] = words str
    in  if elem grade ["A+", "A", "A-", "B+", "B", "B-", "C+", "C", "C-", "D+", "D", "D-", "F+", "F", "F-"]
        then "" else "Grade value expected (A..D, F with +/- possibly appended), \"" ++ grade ++ "\" found."


--Ensures that the subject is 
checkSubject :: String -> String
checkSubject str =
    let [sub, _, _, _, _, _, _] = words str
    in  
        if all (\x -> elem x ['A'..'Z']) sub
        then ""
        else "Expected subject to have all capital letters: \"" ++ sub ++ "\"."

--GPA CALCULATION FUNCTIONS
--This is the 'meat and potatoes' where the main work happens


--Converts a list of strings into a tuple of relevant values
tuplify :: [String] -> (String, Int, String, Int, String, Float)
tuplify [dept, course, section, credits, firstName, lastName, grade] = (dept, 
                read course :: Int, 
                section, 
                read credits :: Int, 
                lastName ++ ", " ++ firstName, --combine first & last names
                gradeToFloat grade)


--Converts a Grade letter to its corresponding number value
gradeToFloat :: String -> Float
gradeToFloat "A+" = 4.0 + (1.0/3.0)
gradeToFloat "A" = 4.0
gradeToFloat "A-" = 4.0 - (1.0/3.0)
gradeToFloat "B+" = 3.0 + (1.0/3.0)
gradeToFloat "B" = 3.0
gradeToFloat "B-" = 3.0 - (1.0/3.0)
gradeToFloat "C+" = 2.0 + (1.0/3.0)
gradeToFloat "C" = 2.0
gradeToFloat "C-" = 2.0 - (1.0/3.0)
gradeToFloat "D+" = 1.0 + (1.0/3.0)
gradeToFloat "D" = 1.0
gradeToFloat "D-" = 1.0 - (1.0/3.0)
gradeToFloat "F+" = 0.0 + (1.0/3.0)
gradeToFloat "F" = 0.0
gradeToFloat "F-" = 0.0 - (1.0/3.0)
--This should never get triggered due to the GPA check
gradeToFloat unknown = error ("Invalid Gpa: " ++ (show unknown))


--Adds the student to a list of lists, grouping together like students
addName :: [[(String, Int, String, Int, String, Float)]]
        -> (String, Int, String, Int, String, Float)
        -> [[(String, Int, String, Int, String, Float)]]
addName list newElem = 
    if list == []
        then [[newElem]]
    else if any inList list
    then
        (filter (not . inList) list) ++ 
        [(((filter inList list) !! 0) ++ [newElem])]
    else
        list ++ [[newElem]]
    where inList = (sameName newElem) . (!! 0)

--Compares two names given their tuples
sameName :: (String, Int, String, Int, String, Float)
         -> (String, Int, String, Int, String, Float)
         -> Bool
sameName (_, _, _, _, firstName, _) (_, _, _, _, secondName, _) = firstName == secondName




--Retrieves a name
getName :: (String, Int, String, Int, String, Float) -> String
getName (_, _, _, _, name, _) = name

--Retrieves the GPA
getGpa :: (String, Int, String, Int, String, Float) -> Float
getGpa (_, _, _, _, _, gpa) = gpa

--Retrieves the Credit Hours
getCreditHours :: (String, Int, String, Int, String, Float) -> Int
getCreditHours (_, _, _, ch, _, _) = ch




--Adds a weighted GPA based on number of credits
addGpa :: Float -> (String, Int, String, Int, String, Float) -> Float
addGpa old new = old + ((getGpa new) * (fromIntegral (getCreditHours new)))

--Adds the credit hours to the running total
addCreditHours :: Int -> (String, Int, String, Int, String, Float) -> Int
addCreditHours old new = old + (getCreditHours new)

--Retireves the total credit hours a person is taking
totalCreditHours :: [(String, Int, String, Int, String, Float)] -> Int
totalCreditHours list = foldl addCreditHours 0 list

--Collapses a single person's information
collapseGpa :: [(String, Int, String, Int, String, Float)] -> (String, Float, Int)
collapseGpa list = 
    if totalCreditHours list == 0
    then (getName $ list !! 0, -1.0, 0)  -- -1.0 will be used as a flag for an invalid GPA
    else
        (getName $ list !! 0, gpa, totalCreditHours list)
        where gpa = (foldl addGpa 0.0 list) / (fromIntegral $ totalCreditHours list)



--Helper function for sorting GPAs
compareGpa :: (String, Float, Int) -> (String, Float, Int) -> Ordering
compareGpa (_, a, _) (_, b, _)
    | a > b = GT
    | a < b = LT
    | a == b = EQ


--Groups together tuples, computing GPAs
consolidate :: [(String, Int, String, Int, String, Float)] -> [(String, Float, Int)]
consolidate list =
    reverse $ sortBy (compareGpa) (map collapseGpa (foldl addName [] list))




--Turns tuple elements to strings for easier formatting
stringify :: (String, Float, Int) -> (String, String, String)
stringify (a, b, c) = 
    let roundB = ((fromInteger (round (b * 100))) / 100)
    in 
        if b == -1.0 -- -1.0 is used as a flag for an invalid GPA.  See collapseGpa for more.
        then (a, "-.--", show c)
        else (a, show roundB, show c)


--Performs all necessary padding for a single element, resulting in one line.
doPadding :: Int -> (String, String, String) -> String
doPadding maxSize (name, gpa, credits) = 
    (padRight maxSize name) ++ (padRight 4 gpa) ++ " " ++ credits ++ "\n"



--Formats processed data into a pretty output string
format :: [(String, Float, Int)] -> String
format list = 
    let strList = map (stringify) list
        names = map (fst3) strList
        maxSize = (foldl max 0 (map (length) names)) + 1
    in foldl (++) "" (map (doPadding maxSize) strList)



