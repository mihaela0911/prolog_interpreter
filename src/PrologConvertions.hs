module PrologConvertions where

import Datatypes
import Validators
import Utils

toLANS :: String -> LetterAndNumberSequence
toLANS = foldr Pair EmptyLANS 

toIdentifier :: String -> Identifier
toIdentifier [] = error "Identifier should contain at least one symbol. "
toIdentifier (x:xs)
    | not (isIdentifier (x:xs)) = error ((x:xs) ++ " is not a valid identifier. ")
    | otherwise                 = Id x (toLANS xs)

toVariable :: String -> Variable
toVariable [] = error "Variable should contain at least one symbol. "
toVariable (x:xs)
    | not (isVariable (x:xs)) = error ((x:xs) ++ " is not a valid variable. ")
    | otherwise               = Var x (toLANS xs)

toConst :: String -> Identifier
toConst [] = error "Constant should contain at least one symbol. "
toConst (x:xs)
    | not (isConstant (x:xs)) = error ((x:xs) ++ " is not a valid constant. ")
    | otherwise               = Id x (toLANS xs)

toTermSequence :: [Term] -> SequanceTerm
toTermSequence []       = error "Term sequence must have at least one term. "
toTermSequence [t]      = SeqEnd t
toTermSequence (x : xs) = CreateSeq x (toTermSequence xs)

toAtomSequence :: [Atom] -> SequanceAtom
toAtomSequence []       = error "Atom sequence must have at least one atom. "
toAtomSequence [a]      = SeqEnd a
toAtomSequence (x : xs) = CreateSeq x (toAtomSequence xs)

toTerm :: String  -> Term
toTerm l
  | not (isTerm l) = error (l ++ " is not a valid term. ")
  | isConstant l   = TermConst (toConst l)
  | isVariable l   = TermVar (toVariable l)
  | otherwise      = TermAtom (toAtom l)

toAtom :: String -> Atom
toAtom l
  | not (isAtom l) = error (l ++ " is not a valid atom. ")
  | otherwise      = CreateAtom id (toTermSequence terms)
  where
    id      = toIdentifier (takeWhile (/= '(') l)
    parPart = dropWhile (/= '(') l
    terms   = map (toTerm . removeEndSpace) (splitBy ',' (init (tail parPart)))

toFact :: String -> Fact
toFact s
  | not (isFact s) = error (s ++ " is not a valid fact. ")
  | otherwise      = toAtom (init s)

toRule :: String -> Rule
toRule l
  | not (isRule l) = error (l ++ "is not a valid rule. ")
  | otherwise      = CreateRule (toAtom beforeSpecial) (toAtomSequence atoms)
  where
    noDot         = init l
    breaking      = break (== '-') noDot
    beforeSpecial = init (fst breaking)
    afterSpecial  = tail (dropWhile (/= '-') (snd breaking))
    atoms         = map (toAtom . removeEndSpace) (splitBy ',' afterSpecial)

toPrologProgram :: [String] -> PrologProgram
toPrologProgram [] = ([],[])
toPrologProgram str = (rules, facts)
  where
    rules = map toRule (filter isRule str)
    facts = map toFact (filter isFact str)

factToTerm :: Fact -> Term
factToTerm f = TermAtom (toAtom (show f))

asToAtomArray :: SequanceAtom -> [Atom]
asToAtomArray (SeqEnd a)       = [a]
asToAtomArray (CreateSeq a as) = a : asToAtomArray as

getVarsAtom :: Atom -> [Variable]
getVarsAtom (CreateAtom _ ts) = getVarsTS ts

getVarsTS :: SequanceTerm -> [Variable]
getVarsTS (SeqEnd t)       = getVarsTerm t
getVarsTS (CreateSeq t ts) = getVarsTerm t ++ getVarsTS ts

getVarsTerm :: Term -> [Variable]
getVarsTerm (TermConst _) = []
getVarsTerm (TermVar v)   = [v]
getVarsTerm (TermAtom a)  = getVarsAtom a

termContainsVariable :: Term -> Bool
termContainsVariable (TermConst _) = False
termContainsVariable (TermVar _)   = True
termContainsVariable (TermAtom a)  = atomContainsVariable a

atomContainsVariable :: Atom -> Bool
atomContainsVariable (CreateAtom _ ts) = tsContainsVariable ts

tsContainsVariable :: SequanceTerm -> Bool
tsContainsVariable (SeqEnd t)       = termContainsVariable t
tsContainsVariable (CreateSeq t ts) = termContainsVariable t || tsContainsVariable ts