{
module Tokens where
}

%wrapper "basic"

$digit = 0-9			  -- digits
$alpha = [a-zA-Z]		-- alphabetic characters
@id = [A-Za-z$digit][A-Za-z'_$digit]*
@filename = [$alpha$digit]*

tokens :-

  $white+			                     	;
  "--".*				                    ;
  SELECT					                 { \s -> TokenSelect }
  FROM					                   { \s -> TokenFrom }
  CROSS$white JOIN                       { \s -> TokenCrossJoin }
  @filename.csv                    { \s -> TokenFile s}
  \"$alpha*\"                      { \s -> TokenConst (tail (init s)) }
  @id\[$digit*\]                   { \s -> TokenLabelledIndex (upToOccurrence '[' s) ( read (upToOccurrence ']' (afterOccurrence '[' s))) }
  $digit+				                   { \s -> TokenInt (read s) }
  @id\*                            { \s -> TokenLabelledAsterisk (init s)}
  \*                               { \s -> TokenAsterisk}

{
-- Each action has type :: String -> Token
-- The token type:
data Token =
  TokenSelect |
	TokenFrom |
  TokenAnd |
  TokenCrossJoin |
  TokenConst String |
  TokenLabelledIndex String Int |
  TokenInt Int |
  TokenAsterisk |
  TokenFile String |
  TokenLabelledAsterisk String
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
