module Queries where

import Validators
import Utils (removeSpaces, splitBy)
import Datatypes(Variable, Identifier, PrologProgram, Term)
import PrologConvertions (toTerm)

data QueryResult
  = EndQR Bool
  | MakeQR (Variable, Replacement) QueryResult
  deriving Eq

instance Show QueryResult where
  show (EndQR True)  = "true."
  show (EndQR False) = "false."
  show (MakeQR (var, r) (EndQR _)) = show var ++ "=" ++ show r ++ "."
  show (MakeQR (var, r) qr)        = show var ++ "=" ++ show r ++ "." ++ show qr

data Replacement
  = ReplaceId Identifier
  | ReplaceVar Variable
  deriving Eq

instance Show Replacement where
  show (ReplaceId id)   = show id
  show (ReplaceVar var) = show var

falseQuery :: QueryResult -> Bool
falseQuery (EndQR False) = True
falseQuery _             = False

trueQuery :: QueryResult -> Bool
trueQuery (EndQR True) = True
trueQuery _            = False

isQuery :: String -> Bool
isQuery query = (isFact query) || (isRule query) || (isEquality query) || (isSequence query)

isEquality :: String -> Bool
isEquality str = elem '=' str && last str == '.' && isTerm firstPart && isTerm secondPart
    where 
        breaked    = break (== '=') (init (removeSpaces str))
        firstPart  = fst breaked
        secondPart = tail (snd breaked)

isSequence :: String -> Bool
isSequence str = (not . null) str && last str == '.' && all isAtom (splitBy ',' (init str))

toEquality :: String -> (Term, Term)
toEquality str
    | (not . isEquality) str = error (str ++ " is not a valid equality query. ") 
    | otherwise              = (toTerm first, toTerm second)
    where
        breaked = break (== '=') (init (removeSpaces str))
        first   = fst breaked
        second  = tail (snd breaked)