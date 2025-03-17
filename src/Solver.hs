module Solver where

import Datatypes
import Unification
import Queries
import Validators
import PrologConvertions
import ResultTreeBuilder
import Utils

solveQuery :: String -> PrologProgram -> [QueryResult]
solveQuery query code
    | isFact query     = solveFact [toAtom (init query)] code
    | isEquality query = [unify (toEquality query)]
    | otherwise        = solveFact (map toAtom (splitBy ',' (init query))) code

solveFact :: [Fact] -> PrologProgram -> [QueryResult]
solveFact facts code = useful facts (gatherSolutions (resultTree code facts (EndQR True)))

useful :: [Atom] -> [QueryResult] -> [QueryResult]
useful atoms qrs = filter (not . falseQuery) (map (filterUseful vars) qrs)
  where
    vars = uniques (concatMap getVarsAtom atoms)
    filterUseful :: [Variable] -> QueryResult -> QueryResult
    filterUseful [] _ = EndQR True
    filterUseful arr (EndQR b) = EndQR b
    filterUseful arr (MakeQR qr@(var, ReplaceId _) qrs)
      | elem var arr = MakeQR qr (filterUseful (filter (/= var) arr) qrs)
      | otherwise    = filterUseful (filter (/= var) arr) qrs
    filterUseful arr (MakeQR qr@(var, ReplaceVar replacement) qrs)
      | elem replacement arr = MakeQR (replacement, ReplaceVar var) (filterUseful (filter (/= replacement) arr) qrs)
      | elem var arr         = MakeQR qr (filterUseful (filter (/= var) arr) qrs)
      | otherwise            = filterUseful (filter (/= var) arr) qrs
