module ResultTreeBuilder where

import Queries
import Datatypes
import Unification
import PrologConvertions

data ResultTree
  = EmptyTree
  | Node [ResultTree]
  | Leaf QueryResult

resultTree :: PrologProgram -> [Atom] -> QueryResult -> ResultTree
resultTree _ [] qr            = Leaf qr
resultTree (r, f) (a : as) qr = Node (factTree ++ ruleTree)
  where
    factTree = map (\fqr -> resultTree (r, f) (map (`applyAtom` fqr) as) (combineResults fqr qr)) (fst u)
    ruleTree = map (\(al, rqr) -> resultTree (r, f) (map (`applyAtom` rqr) (al ++ as)) (combineResults rqr qr)) (snd u)
    u = unifiers (r, f) a

gatherSolutions :: ResultTree -> [QueryResult]
gatherSolutions EmptyTree       = []
gatherSolutions (Leaf qr)       = [qr]
gatherSolutions (Node subtrees) = concatMap gatherSolutions subtrees
