{
module Lexer where
import qualified Scri.Token as T
}

%wrapper "monad"

$digit = 0-9
$lowc = [a-z]
$highc = [A-Z]
$alpha = [a-zA-Z]
$alphaNum = [$alpha $digit]
$fs = \
$astrisk = \42
$semic = \59

$linec = [^ \- $fs $astrisk \n]
$cmdc = [ \n [^ $semic ] ]

@lowId = $lowc $alphaNum*
@highId = $highc $alphaNum*
@lines = $linec+

scri :-
  <0> ^$fs { (tok T.BeginCommand) `andBegin` command }
  <0> $astrisk{3} { tok T.BoldAndItalic }
  <0> $astrisk{2} { tok T.Bold }
  <0> $astrisk { tok T.Italic }
  <0> "-"{2} { tok T.EmDash }
  <0> "$" @lowId { strTok T.VarSub }
  <0> @lines { strTok T.ParagraphSection }
  <0> \n{2,} { tok T.ParagraphBreak }
  <0> \n { skip }
 
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

