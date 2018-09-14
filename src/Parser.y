{
module Parser where
import qualified Scri.Token as T
import Scri.Ast
import Lexer
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
  pbreak { T.ParagraphBreak }
  italic { T.Italic }
  bold { T.Bold }
  boldAndItalic { T.BoldAndItalic }
  emDash { T.EmDash }
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
  : OptionalPbreak StmtSeq OptionalPbreak { Script (reverse $2) }

OptionalPbreak :: { () }
OptionalPbreak
  : pbreak { () }
  | {- empty -} { () }

StmtSeq :: { [Stmt] }
  : StmtSeq pbreak Stmt { $3:$1 }
  | Stmt { [$1] }

Stmt :: { Stmt }
Stmt
  : FmtTextGroup { PutText $1 }
  | beginCommand commandText endCommand { RunCmd $2 }

FmtText1 :: { FmtText }
FmtText1
  : italic FmtTextGroup italic { Italic $2 }
  | bold FmtTextGroup bold { Bold $2 }
  | boldAndItalic FmtTextGroup boldAndItalic { Bold (Italic $2) }
  | varSub { ReplaceWith (drop 1 $1) }
  | FmtText0 { $1 }

FmtText0
  : emDash { EmDash }
  | psection { Plain (map  (\c -> if c=='\n' then ' ' else c) $1) }

FmtTextGroup :: { FmtText }
FmtTextGroup
  : FmtTextSeq { Group $1 }

FmtTextSeq :: { [FmtText] }
  : FmtTextSeq FmtText1 { $2:$1 }
  | FmtText1 { [$1] }


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
