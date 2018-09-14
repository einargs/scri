module Scri.Ast where
import Data.Colour (Colour)

-- | A specialization of 'Colour' that specifies the way
-- the colour data is stored. 
type FmtColour = Colour Double

-- | The top level AST datatype.
--
-- 'Script' represents a single @.scri@ file. It is a
-- pipeline of operations that are applied to the context.
data Script = Script [Stmt] deriving Show

-- | A 'Stmt' is a representation of a single operation
-- on the monadic context of the scri file.
data Stmt
  -- | Enque the text representation in the output.
  = PutText FmtText
  | RunCmd String
  deriving (Show)
  {- Commented out for a future, more complex syntax
  | PutExpr Expr
  | VarInit VarSym Expr
  | Import ModuleSym
  | Export VarSym
  -}

-- | Represents formatted text.
--
-- Consider adding an EnDash.
data FmtText
  = Plain String
  | Group [FmtText]
  | Bold FmtText
  | Italic FmtText
  | EmDash
  | Coloured FmtText FmtColour
  | ReplaceWith String
  deriving (Show)

{- Commented out for a future, more complex syntax
-- | Represents an expression that produces a haskell value.
data Expr
  = FuncCall VarSym [FuncArg]
  | VarRef VarSym
  | NumLit String
  | StrLit String

-- |
data FuncArg
  = KeywordArg ArgName Expr
  | PlaceArg Expr

-- |
type ArgName = String

-- |
type VarSym = String

-- |
type PathSym = String -- probably going to be changed to ModuleSym?
-}
