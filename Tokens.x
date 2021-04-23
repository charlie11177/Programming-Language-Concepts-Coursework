{
module Tokens where
}

%wrapper "basic"

$digit = 0-9			  -- digits
$alpha = [a-zA-Z]		-- alphabetic characters

tokens :-

  $white+			                     	;
  "--".*				                    ;
  SELECT					                 { \s -> TokenSelect }
  FROM					                   { \s -> TokenFrom }
  \"$alpha*\"                      { \s -> TokenConst (tail (init s)) }
  $digit+				                   { \s -> TokenInt (read s) }
  [\=\+\-\*\/\(\)]			           { \s -> Sym (head s) }
  $alpha [$alpha $digit \_ \’]*    { \s -> TokenVar s }

{
-- Each action has type :: String -> Token

-- The token type:
data Token =
	TokenSelect |
	TokenFrom |
  TokenConst String |
  TokenInt Int |
	Sym Char |
	TokenVar String
	deriving (Eq,Show)

}
