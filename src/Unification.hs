module Unification where

import Validators
import Queries
import PrologConvertions
import Datatypes

unify :: (Term, Term) -> QueryResult
unify (left, right)
  | termContainsVariable left || termContainsVariable right = process [(left, right)] (EndQR False)
  | left == right = EndQR True
  | otherwise     = EndQR False
  where
    process :: [(Term, Term)] -> QueryResult -> QueryResult
    process [] result = result
    process ((TermConst l, TermConst r) : rest) result
      | l == r = process rest result
      | otherwise = EndQR False
    process ((TermVar l, TermVar r) : rest) result   = process (updateStackVar l r rest) (MakeQR (l, ReplaceVar r) result)
    process ((TermConst l, TermVar r) : rest) result = process (updateStackConst r l rest) (MakeQR (r, ReplaceId l) result)
    process ((TermVar l, TermConst r) : rest) result = process (updateStackConst l r rest) (MakeQR (l, ReplaceId r) result)
    process ((TermVar _, _) : _) _   = EndQR False
    process ((TermConst _, _) : _) _ = EndQR False
    process ((_, TermVar _) : _) _   = EndQR False
    process ((_, TermConst _) : _) _ = EndQR False
    process ((TermAtom lAtom, TermAtom rAtom) : rest) result = handleAtoms lAtom rAtom rest result

    handleAtoms :: Atom -> Atom -> [(Term, Term)] -> QueryResult -> QueryResult
    handleAtoms (CreateAtom id1 ts1) (CreateAtom id2 ts2) stack result
      | id1 /= id2 = EndQR False
      | sequenceLength ts1 /= sequenceLength ts2 = EndQR False
      | otherwise = process (mergeStack ts1 ts2 stack) result
      where
        sequenceLength :: Sequance a -> Int
        sequenceLength (SeqEnd _) = 1
        sequenceLength (CreateSeq _ seq) = 1 + sequenceLength seq 

updateStackVar :: Variable -> Variable -> [(Term, Term)] -> [(Term, Term)]
updateStackVar var newVar = map (updateTerms var newVar)
  where
    replaceVarInTerm :: Term -> Term
    replaceVarInTerm (TermVar v)
      | v == var = TermVar newVar
      | otherwise = TermVar v
    replaceVarInTerm (TermConst c) = TermConst c
    replaceVarInTerm (TermAtom atom) = TermAtom (updateAtom atom)

    updateAtom :: Atom -> Atom
    updateAtom (CreateAtom id terms) = CreateAtom id (updateSequence terms)

    updateSequence :: SequanceTerm -> SequanceTerm
    updateSequence (SeqEnd term) = SeqEnd (replaceVarInTerm term)
    updateSequence (CreateSeq term seq) = CreateSeq (replaceVarInTerm term) (updateSequence seq)

    updateTerms :: Variable -> Variable -> (Term, Term) -> (Term, Term)
    updateTerms var newVar (l, r) = (replaceVarInTerm l, replaceVarInTerm r)

mergeStack :: SequanceTerm -> SequanceTerm -> [(Term, Term)] -> [(Term, Term)]
mergeStack (SeqEnd t1) (SeqEnd t2) stack = (t1, t2) : stack
mergeStack (CreateSeq t1 ts1) (CreateSeq t2 ts2) stack = mergeStack ts1 ts2 ((t1, t2) : stack)
mergeStack _ _ _ = error "Sequences with different lengths presented."

updateStackConst :: Variable -> Constant -> [(Term, Term)] -> [(Term, Term)]
updateStackConst var const = map (updateTerms var const)
  where
    replaceConstInTerm :: Term -> Term
    replaceConstInTerm (TermVar v)
      | v == var = TermConst const
      | otherwise = TermVar v
    replaceConstInTerm (TermConst c)   = TermConst c
    replaceConstInTerm (TermAtom atom) = TermAtom (updateAtom atom)

    updateAtom :: Atom -> Atom
    updateAtom (CreateAtom id terms) = CreateAtom id (updateSequence terms)

    updateSequence :: SequanceTerm -> SequanceTerm
    updateSequence (SeqEnd term)        = SeqEnd (replaceConstInTerm term)
    updateSequence (CreateSeq term seq) = CreateSeq (replaceConstInTerm term) (updateSequence seq)
    
    updateTerms :: Variable -> Constant -> (Term, Term) -> (Term, Term)
    updateTerms var const (l, r) = (replaceConstInTerm l, replaceConstInTerm r)
      
applySequence :: SequanceAtom -> QueryResult -> SequanceAtom
applySequence (SeqEnd atom) result          = SeqEnd (applyAtom atom result)
applySequence (CreateSeq atom atoms) result = CreateSeq (applyAtom atom result) (applySequence atoms result)

applyAtom :: Atom -> QueryResult -> Atom
applyAtom atom (MakeQR (var, rep) nextResult@(MakeQR _ _)) = applyAtom (substituteAtom var rep atom) nextResult
applyAtom atom (MakeQR (var, rep) (EndQR _)) = substituteAtom var rep atom
applyAtom atom _ = atom

substituteAtom :: Variable -> Replacement -> Atom -> Atom
substituteAtom var rep (CreateAtom idPart terms) = CreateAtom idPart (substituteSequence var rep terms)

substituteSequence :: Variable -> Replacement -> SequanceTerm -> SequanceTerm
substituteSequence var rep (SeqEnd term)        = SeqEnd (substituteTerm var rep term)
substituteSequence var rep (CreateSeq term seq) = CreateSeq (substituteTerm var rep term) (substituteSequence var rep seq)

substituteTerm :: Variable -> Replacement -> Term -> Term
substituteTerm var (ReplaceId id) (TermVar v)
  | var == v = TermConst id
  | otherwise = TermVar v
substituteTerm var (ReplaceVar newVar) (TermVar v)
  | var == v  = TermVar newVar
  | otherwise = TermVar v
substituteTerm var rep (TermAtom atom) = TermAtom (substituteAtom var rep atom)
substituteTerm _ _ term = term

combineResults :: QueryResult -> QueryResult -> QueryResult
combineResults (EndQR _) result = result
combineResults (MakeQR elem next) result = MakeQR elem (combineResults next result)

unifiers :: PrologProgram -> Atom -> ([QueryResult], [([Atom], QueryResult)])
unifiers (r, f) a =
  ( 
    filter (not . falseQuery) (map (\fact -> unify (factToTerm fact, TermAtom a)) f),
    filter (not . falseQuery . snd) (map (\(CreateRule ah as) -> (asToAtomArray as, unify (TermAtom ah, TermAtom a))) r)
  )