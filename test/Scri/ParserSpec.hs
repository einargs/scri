{-# LANGUAGE QuasiQuotes #-}
module Scri.ParserSpec (spec) where

import Test.Hspec
import Data.Text (Text, unpack)

import NeatInterpolation (text)

import Scri.TestHelp
import Scri.Parser (parse)
import Scri.Ast

spec :: Spec
spec = do
  describe "parse" $ do
    context "when dealing with paragraphs" $ do
      it "parses a single paragraph" $ do
        shouldParseTo [text|
          Hello!
          |] $ do
            p "Hello!"

      it "parses multiple paragraphs" $ do
        shouldParseTo [text|
          First

          Second
          
          Third
          Fourth
          Fifth
          |] $ do
            p "First"
            p "Second"
            p "Third"
            p "Fourth"
            p "Fifth"
    
    context "when dealing with comments" $ do
      it "ignores comments" $ do
        shouldParseTo [text|
          Hey! I'm the first paragraph.

          // This is a comment. It should be ignored.

          I'm the second paragraph.
          |] $ do
            p "Hey! I'm the first paragraph."
            p "I'm the second paragraph." 

      it "ignores multiple comments" $ do
        shouldParseTo [text|
          p1

          // c1

          p2
          // c2
          p3
          //c3
          //     c4
          |] $ do
            p "p1"
            p "p2"
            p "p3"

      it "handles comments after text" $ do
        shouldParseTo [text|
          p1 // c1

          p2 //     c2

          p3 has a comment // c3 is longer
          |] $ do
            p "p1 "
            p "p2 "
            p "p3 has a comment "
    
    context "when handling italics" $ do
      it "parses italic text with no surrounding plain text" $ do
        shouldParseTo [text|
          *italic*
          |] $ g $ do
            i "italic"

      it "parses italics within a single paragraph" $ do
        shouldParseTo [text|
          I really *hate* this.
          |] $ g $ do
            s "I really "
            i "hate"
            s " this."

      it "parses multiple italics within a single paragraph" $ do
        shouldParseTo [text|
          This *has* multiple *italics* in it.
          |] $ g $ do
            s "This "
            i "has"
            s " multiple "
            i "italics"
            s " in it."

      it "parses italics in multiple paragraphs" $ do
        shouldParseTo [text|
          p1

          p2 has *italics*.

          p3

          p4 *also* has italics.
          |] $ do
            p "p1"
            g $ do
              s "p2 has "
              i "italics"
              s "."
            p "p3"
            g $ do
              s "p4 "
              i "also"
              s " has italics."
      
      it "parses multiple italics in multiple paragraphs" $ do
        shouldParseTo [text|
          p1 has *two* seperate sets of *italics*.

          p2

          p3 has *three* sets of *italics*. Isn't that *cool*?
          |] $ do
            g $ do
              s "p1 has "
              i "two"
              s " seperate sets of "
              i "italics"
              s "."
            p "p2"
            g $ do
              s "p3 has "
              i "three"
              s " sets of "
              i "italics"
              s ". Isn't that "
              i "cool"
              s "?"
    context "when dealing with commands" $ do
      it "parses a single command" $ do
        shouldParseTo [text|
          \test1 arg1 arg2;
          |] $ do
            cmd "test1 arg1 arg2"

      it "parses multiple commands" $ do
        shouldParseTo [text|
          \test1 arg1 arg2;
          \test2 arg1 arg2;
          |] $ do
            cmd "test1 arg1 arg2"
            cmd "test2 arg1 arg2"

      it "parses multiple commands interspersed by paragraphs" $ do
        shouldParseTo [text|
          \test1 a1 a2 a3 a4;
          
          p1
          
          \test2 a1;
          
          p2
          
          \test3 a1 a2; 
          |] $ do
            cmd "test1 a1 a2 a3 a4"
            p "p1"
            cmd "test2 a1"
            p "p2"
            cmd "test3 a1 a2"
      
      it "parses a multiline command" $ do
        shouldParseTo [text|
          \test ComplexArgs
            { kwarg1 = "v1"
            , kwarg2 = 123
            };
          |] $ do
            cmd "test ComplexArgs\n  { kwarg1 = \"v1\"\n  , kwarg2 = 123\n  }"

      it "parses several multiline commands" $ do
        shouldParseTo [text|
          \test1
            a1
            a2;

          p1

          \test2
            a1
            a2;

          p3

          \test a1 a2;
          |] $ do
            cmd "test1\n  a1\n  a2"
            p "p1"
            cmd "test2\n  a1\n  a2"
            p "p3"
            cmd "test a1 a2"
