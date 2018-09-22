module Scri.TestHelp
  ( p
  , g
  , s
  , i
  , ig
  , b
  , bg
  , emDash
  , cmd
  , shouldParseTo
  ) where

import Control.Monad.Free
import Test.Hspec
import Data.Text (Text, unpack)
import NeatInterpolation

import Scri.Ast
import Scri.Parser (parse)


data AstChain a next
  = ChainLink a next
  | ChainEnd

instance Functor (AstChain a) where
  -- fmap :: (a -> b) -> f a -> f b
  fmap f (ChainLink s n) = ChainLink s (f n)
  fmap f ChainEnd = ChainEnd

aliftF :: (Functor f) => f r -> Free f r
aliftF x = Free (fmap Pure x)

link x = aliftF $ ChainLink x ()
end = aliftF ChainEnd

type AstBuilder a n = Free (AstChain a) n
type StmtBuilder n = AstBuilder Stmt n
type FmtBuilder n = AstBuilder FmtText n

runAstBuilder :: AstBuilder a n -> [a]
runAstBuilder c = toList (c >> end)
  where
    toList (Free (ChainLink s n)) = s:(toList n)
    toList (Free ChainEnd) = []

runBuilder :: ([a] -> b) -> AstBuilder a n -> b
runBuilder f b = f $ runAstBuilder b

runStmtBuilder :: StmtBuilder n -> Script
runStmtBuilder = runBuilder Script

runFmtBuilder :: FmtBuilder n -> FmtText
runFmtBuilder = runBuilder Group


fmtToStmtBuilder :: FmtText -> StmtBuilder ()
fmtToStmtBuilder fmt = link $ PutText fmt

group :: FmtBuilder n -> FmtText
group = runFmtBuilder 





p :: String -> StmtBuilder ()
p s = fmtToStmtBuilder $ Group [Plain s]

g :: FmtBuilder n -> StmtBuilder ()
g = link . PutText . group

s :: String -> FmtBuilder ()
s = link . Plain

i :: String -> FmtBuilder ()
i str = link $ Italic $ Group [Plain str]

b :: String -> FmtBuilder ()
b str = link $ Bold $ Group [Plain str]

ig :: FmtBuilder n -> FmtBuilder ()
ig builder = link $ Italic $ group builder

bg :: FmtBuilder n -> FmtBuilder ()
bg builder = link $ Bold $ group builder

emDash :: FmtBuilder ()
emDash = link EmDash

cmd :: String -> StmtBuilder ()
cmd s = link $ RunCmd s


shouldParseTo :: Text -> StmtBuilder n -> Expectation
shouldParseTo txt builder = parsedVal `shouldBe` rightAst
  where parsedVal = parse (unpack txt)
        rightAst = Right $ runStmtBuilder builder
