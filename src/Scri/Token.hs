module Scri.Token where

data Token
  -- Markup tokens
  = ParagraphSection String
  | LineBreak
  | Italic
  | Bold
  | BoldAndItalic
  | VarSub String
  | EOFToken
  | BeginCommand
  -- Command tokens
  | EndCommand
  | CommandText String
  {- Reserved for usage in a later, more complex syntax
  | Import
  | Eq
  | Period
  | Colon
  | LowIdent String
  | HighIdent String
  | NumLit Double
  | StrLit String-}
  deriving (Eq, Show)
