{
module Scri.Parser where
import qualified Scri.Token as T
import Scri.Ast
import Scri.Lexer
import Control.Monad.State.Lazy
import Control.Monad.Identity
}

%name parseScri Script
%tokentype { T.Token }
%error { parseError }

%monad { ParserMonad }
%lexer { lexwrap } { T.EOFToken }

%token
  psection { T.ParagraphSection $$ }
  linebreak { T.LineBreak }
  italic { T.Italic }
  bold { T.Bold }
  boldAndItalic { T.BoldAndItalic }
  varSub { T.VarSub $$ }
  beginCommand { T.BeginCommand }
  commandText { T.CommandText $$ }
  endCommand { T.EndCommand }
  {- Commented out for a future, more complex syntax
  import { T.Import }
  '=' { T.Eq }
  '.' { T.Period }
  ':' { T.Colon }
  lowIdent { T.LowIdent $$ }
  highIdent { T.HighIdent $$ }
  numLit { T.NumLit $$ }
  strLit { T.StrLit $$ }
  -}

-- Only defined for precedence
-- Highest precedence comes last(?)
%left prec1
%left prec2
%left prec3

%%

Script :: { Script }
Script
  : OptionalPBreak StmtSeq OptionalPBreak { Script (reverse $2) }

PBreak :: { () }
PBreak
  : PBreak linebreak { () }
  | linebreak { () }

OptionalPBreak :: { () }
OptionalPBreak
  : PBreak { () }
  | {- empty -} { () }

StmtSeq :: { [Stmt] }
StmtSeq
  : StmtSeq PBreak Stmt { $3:$1 }
  | Stmt { [$1] }

Stmt :: { Stmt }
Stmt
  : FmtGroup(FmtText) { PutText $1 }
  | beginCommand commandText endCommand { RunCmd $2 }

FmtText :: { FmtText }
FmtText
  : FmtTextB { $1 }
  | FmtTextI { $1 }
  | FmtTextBI { $1 }
  | FmtText0 { $1 }

FmtTextBI :: { FmtText }
FmtTextBI
  : boldAndItalic
    FmtGroupW2(FmtTextB, FmtTextI)
    boldAndItalic
    { Bold (Italic $2) }

FmtTextB :: { FmtText }
FmtTextB
  : bold FmtGroupW2(FmtTextB, FmtTextBI) bold { Bold $2 }

FmtTextI :: { FmtText }
FmtTextI
  : italic FmtGroupW2(FmtTextB, FmtTextBI) italic { Italic $2 }

FmtGroupW2(e1,e2)
  : FmtGroup(FmtTextW2(e1,e2)) { $1 }

FmtGroup(fmt)
  : List1(fmt) { Group $1 }

FmtTextW2(e1,e2)
  : FmtText0 { $1 }
  | e1 { $1 }
  | e2 { $1 }

FmtText0 :: { FmtText }
FmtText0
  : varSub { VarSub (drop 1 $1) }
  | psection { Plain $1 }

List1(p)
  : RevList1(p) { reverse $1 }

RevList1(p)
  : RevList1(p) p { $2:$1 }
  | p { [$1] }

{- Commented out for a future, more complex syntax
Command :: { Stmt }
Command
  : Expr { PutExpr $1 }
  | import ModuleSym { Import ModuleSym }

Expr :: { Expr }
Expr
  : VarSym ( KeywordArgs ) { FuncCall $1 $2 } 
  | VarSym PlaceArgs { FuncCall $1 $2 }


KeywordArgs :: { [KeywordArg] }
KeywordArgs
  : KeywordArgs KeywordArg { $2:$1 }
  | KeywordArg { [$1] }

KeywordArg :: { KeywordArg }
KeywordArg
  : ArgName ':' 


PlaceArgs :: { [PlaceArgs] }
PlaceArgs
  : PlaceArgs PlaceArg { $2:$1 }
  | PlaceArg { [$1] }
-}


{
type ParserMonad a = Alex a

parseError :: T.Token -> ParserMonad a
parseError tokens = fail $ unlines
  [ "An error occured in the parser."
  , "The tokens were: " ++ show tokens
  ]

-- | Wrapper for the Alex-generated lexer.
--
-- Please note that this could also be written as:
--
-- @lexwrap = (alexMonadScan >>=)@
--
-- For more information, look at the stack overflow answer below:
-- https://stackoverflow.com/questions/20315739/how-to-use-an-alex-monadic-lexer-with-happy
lexwrap :: (T.Token -> ParserMonad a) -> ParserMonad a
lexwrap cont = do
  token <- alexMonadScan
  cont token

parseWith parser s = runAlex s parser
parse = parseWith parseScri
}
