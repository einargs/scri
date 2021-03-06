{
module Scri.Lexer where
import qualified Scri.Token as T
}

%wrapper "monad"

$digit = 0-9
$lowc = [a-z]
$highc = [A-Z]
$alpha = [a-zA-Z]
$alphaNum = [$alpha $digit]
$forward_slash = \
$back_slash = \/
$astrisk = \42
$semic = \59

$linec = [^ \$ $back_slash $forward_slash $astrisk \n]
$cmdc = [ \n [^ $semic ] ]

@lowId = $lowc [$alphaNum _]*
@highId = $highc $alphaNum*
@lines = $linec+

scri :-
  <0> "//" .+ / \n { skip }
  <0> ^$forward_slash { (tok T.BeginCommand) `andBegin` command }
  <0> $astrisk{3} { tok T.BoldAndItalic }
  <0> $astrisk{2} { tok T.Bold }
  <0> $astrisk { tok T.Italic }
  <0> "$" @lowId { strTok T.VarSub }
  <0> @lines { strTok T.ParagraphSection }
  <0> \n { tok T.LineBreak }
 
  <command> ";"$ { (tok T.EndCommand) `andBegin` 0 }
  <command> $cmdc+ { strTok T.CommandText }
  

{
  {- Commented out for a future, more complex syntax
  <command> "import" { tok T.Import }
  <command> "%%" { begin markup }
  <command> '=' { tok T.EqSym }
  <command> '.' { tok T.PeriodSym }
  <command> ':' { tok T.ColonSym }
  <command> @lowId { strTok T.LowIdent }
  <command> @highId { strTok T.HighIdent }
  <command> $digit+ ('.' $digit+)? { strTok \s -> T.NumLit (read s :: Double) } 
  <command> $quote $stringchar* $quote { strTok T.StrLit }
  -}

-- Indicate the EOF token
alexEOF = return T.EOFToken

tok :: T.Token -> AlexAction T.Token
tok token inp len = return token

strTok :: (String -> T.Token) -> AlexAction T.Token
strTok ctr inp@(_,_,_,str) len = do
  return (ctr $ take len str)

}

