module PrologAst where

data Token = TIdent String
           | TVar String
           | Comma
           | Semi
           | Lbr
           | Rbr
           | Dot
           | Cork
           deriving (Eq, Show)

data PrologProgram = Program {
        pModule :: Maybe String
      , types   :: [TypeDef]
      , rels    :: [Relation]
      }
      deriving (Eq, Show)

data TypeDef = TypeDef String Type
             deriving (Eq, Show)

data Type = Var String
          | TAtom Atom
          | Arrow Type Type
          deriving (Eq, Show)

data Atom = Atom { atomHead :: String, atomArgs :: [Either Atom String] }
          deriving (Eq, Show)

data Relation = Relation { relHead :: Atom, relBody :: Maybe RelationBody }
              deriving (Eq, Show)

data RelationBody = RAtom Atom
                  | Conj RelationBody RelationBody
                  | Disj RelationBody RelationBody
                  deriving (Eq, Show)