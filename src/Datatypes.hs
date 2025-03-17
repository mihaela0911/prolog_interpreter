module Datatypes where

data LetterAndNumberSequence = EmptyLANS | Pair Char LetterAndNumberSequence
    deriving Eq

instance Show LetterAndNumberSequence where
    show EmptyLANS = []
    show (Pair a lns) = a : show lns

data Identifier = Id Char LetterAndNumberSequence
    deriving Eq

instance Show Identifier where
    show (Id c lаns) = c : show lаns

-- check if the first is upper case
data Variable = Var Char LetterAndNumberSequence
    deriving Eq

instance Show Variable where
    show (Var c lаns) = c : show lаns

type Constant = Identifier

-- cannot be empty because SequanceTerm and SequanceAtom request at least one term
data Sequance el = SeqEnd el | CreateSeq el (Sequance el)
    deriving Eq

instance Show el => Show (Sequance el) where
    show (SeqEnd el) = show el
    show (CreateSeq el rest) = show el ++ "," ++ show rest


-- <term>{, <term>}
type SequanceTerm = Sequance Term

-- identifier(<term>{, <term>})
data Atom =  CreateAtom Identifier SequanceTerm
    deriving Eq

instance Show Atom where
  show (CreateAtom id ts) = show id ++ "(" ++ show ts ++ ")"

data Term = TermConst Constant 
            | TermVar Variable 
            | TermAtom Atom
    deriving Eq

instance Show Term where
    show (TermConst c) = show c
    show (TermVar v)   = show v
    show (TermAtom a)  = show a

-- <atom>.
type Fact = Atom

-- <atom>{, <atom>}
type SequanceAtom = Sequance Atom

-- atom :- <atom>{, <atom>}.
data Rule = CreateRule Atom SequanceAtom
    deriving Eq

instance Show Rule where
  show (CreateRule a as) = show a ++ ":-" ++ show as ++ "."

type PrologProgram = ([Rule], [Fact])