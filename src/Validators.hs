module Validators where

import Data.Char (isAsciiLower, isAlpha, isAlphaNum, isAsciiUpper)
import Data.List (isInfixOf)
import Utils

isIdentifier :: String -> Bool
isIdentifier []     = False
isIdentifier (x:xs) = isAsciiLower x && all isAlphaNum xs

isVariable :: String -> Bool
isVariable []     = False
isVariable (x:xs) = isAsciiUpper x && all isAlphaNum xs

isConstant :: String -> Bool
isConstant = isIdentifier

isTerm :: String -> Bool
isTerm []  = False
isTerm str = isConstant str || isVariable str || isAtom str

isTermSequance :: String -> Bool
isTermSequance []  = False
isTermSequance seq = all isTerm (splitBy ',' seq)

isAtom :: String -> Bool
isAtom []  = False
isAtom str = isIdentifier beforeBracket 
                    && (isTermSequance . init . tail) betweenBrackets
                    && isValidPart betweenBrackets
    where
        beforeBracket   = takeWhile (/= '(') str
        betweenBrackets = dropWhile (/= '(') str
        isValidPart s   = head s == '(' && last s == ')' 

isFact :: String -> Bool
isFact []  = False
isFact str = (isAtom . init ) str && last str == '.'

isAtomSequance :: String -> Bool
isAtomSequance []  = False
isAtomSequance seq = all isAtom (splitBy ',' seq)

isRule :: String -> Bool
isRule []  = False
isRule str = isAtom firstPart && hasDelimeter str && isAtomSequance (init secondPart) && last str == '.' 
    where
        firstPart      = takeWhile (/= ':') str
        secondPart     = tail (dropWhile (/= '-') str)
        hasDelimeter s = ":-" `isInfixOf` s

isComment :: String -> Bool
isComment []  = False
isComment str =  head str == '%'

isValidProgram :: String -> Bool
isValidProgram [] = True
isValidProgram pr = all (\x -> isFact x || isRule x || isComment x) filteredEmpty
    where 
        filteredEmpty = filter (/= "") (splitBy '\n' pr)