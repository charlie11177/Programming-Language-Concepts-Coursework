{
	module Parsing where
}

%name parse
%tokentype { Token }
%error { parseError }

%token
	select		{ TokenSelect }
	from		{ TokenFrom }
	where 		{ TokenWhere }
	crossJoin	{ TokenCrossJoin }
	as 			{ TokenAs }	
	collect		{ TokenCollect }
	sequential  { TokenSequential }
	predPick    { TokenPredPick }
	and         { TokenAnd }
	or          { TokenOr }
	xor			{ TokenXOr }
	not			{ TokenNot }
	equals		{ TokenEquals }
	lt          { TokenLT }
	gt          { TokenGT }
	lParen		{ TokenLParen }
	rParen		{ TokenRParen }
	file		{ TokenFile $$ }
	const       { TokenConst $$ }
	labelIdx    { TokenLabelledIndex _ _ }
	int         { TokenInt $$ }
	labelAst    { TokenLabelledAsterisk $$ }
	ast         { TokenAst }

%%

Dummy 	: select from where 	{ DummyExp }

{

	parseError :: [Token] -> a
	parseError _ = error "Parse error"
	
	data Dummy = DummyExp deriving Show
}
