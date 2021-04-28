{
module Tokens where
}

%wrapper "basic"

$digit = 0-9                            -- digits
$alpha = [a-zA-Z]                       -- alphabetic characters
$special    = [\.\;\,\$\|\*\+\?\#\~\-\{\}\(\)\[\]\^\/]
@string = [$alpha$digit$special]*
@id = [A-Za-z$digit][A-Za-z'_$digit]*
@filename = [$alpha$digit]*

tokens :-

  $white+                                   ;
  "--".*                                    ;
  SELECT                                    { \s -> TokenSelect }
  FROM                                      { \s -> TokenFrom }
  WHERE                                     { \s -> TokenWhere }
  CROSS$white JOIN                          { \s -> TokenCrossJoin }
  CONCAT$white JOIN                         { \s -> TokenConcatJoin }
  AS                                        { \s -> TokenAs }
  COLLECT                                   { \s -> TokenCollect }
  SEQUENTIAL                                { \s -> TokenSequential }
  PREDPICK                                  { \s -> TokenPredPick }
  TRUE                                      { \s -> TokenTrue }
  FALSE                                     { \s -> TokenFalse }
  AND                                       { \s -> TokenAnd }
  OR                                        { \s -> TokenOr }
  XOR                                       { \s -> TokenXOr }
  NOT                                       { \s -> TokenNot }
  EQUALS                                    { \s -> TokenEquals }
  LT                                        { \s -> TokenLT }
  GT                                        { \s -> TokenGT }
  \(                                        { \s -> TokenLParen }
  \)                                        { \s -> TokenRParen}
  @filename.csv                             { \s -> TokenFile s}
  \"@string\"                               { \s -> TokenConst (tail (init s)) }
  @id\[$digit*\]                            { \s -> TokenLabelledIndex (upToOccurrence '[' s) ( read (upToOccurrence ']' (afterOccurrence '[' s))) }
  $digit+                                   { \s -> TokenInt (read s) }
  @id\*                                     { \s -> TokenLabelledAsterisk (init s)}
  \*                                        { \s -> TokenAsterisk}
  @id                                       { \s -> TokenLabel s }

{
-- Each action has type :: String -> Token
-- The token type:
data Token =
    TokenSelect |
    TokenFrom |
    TokenAnd |
    TokenCrossJoin |
    TokenConcatJoin |
    TokenConst String |
    TokenLabelledIndex String Int |
    TokenInt Int |
    TokenAsterisk |
    TokenFile String |
    TokenLabelledAsterisk String |
    TokenWhere |
    TokenOr |
    TokenXOr |
    TokenNot |
    TokenEquals |
    TokenLT |
    TokenGT |
    TokenLParen |
    TokenRParen |
    TokenAs |
    TokenCollect |
    TokenSequential |
    TokenPredPick |
    TokenTrue |
    TokenFalse |
    TokenLabel String
  deriving (Eq,Show)

upToOccurrence :: Char -> String  -> String
upToOccurrence _ [] = []
upToOccurrence char (a:as) | a == char = []
                           | otherwise = a : upToOccurrence char as

afterOccurrence :: Char -> String -> String
afterOccurrence _ [] = []
afterOccurrence char (a:as) | a == char = as
                            | otherwise = afterOccurrence char as
}
