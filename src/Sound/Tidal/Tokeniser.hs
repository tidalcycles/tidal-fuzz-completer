module Sound.Tidal.Tokeniser where

import Data.List
import Data.Char
import Data.List.Split (splitOn)


-- remove parantheses, any other punctuation
removePunc :: [Char] -> [Char]
removePunc xs = [ x | x <- xs, not (x `elem` "\'()$#") ]

-- split at double line break character, remove single line characters
breakupCode :: String -> String -> [String]
breakupCode st xs = map (filter (/= '\n')) $ splitOn st xs

-- separate mini notations
takeMininotation :: String -> (String, String)
takeMininotation ('"':s) = (('"': takeWhile (/= '"') s) ++ "\"", tail $ dropWhile (/= '\"') s )

-- separate words
takeWord :: String -> (String, String)
takeWord s = (takeWhile (/= ' ') s, dropWhile (/= ' ') s)

-- tokenise mini notation and words as separate
tokeniser :: String -> [String]
tokeniser [] = []
tokeniser (' ':s) = tokeniser s
tokeniser ('"':s) = m:(tokeniser s') -- get around adding /removing "??
                      where (m, s') = takeMininotation ('"':s)
tokeniser (s) = w:(tokeniser s')
                  where (w, s') = takeWord s
