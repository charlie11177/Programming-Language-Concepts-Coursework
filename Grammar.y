{
module Parsing where
import Tokens
}

%name parse
%tokentype { Token }
%error { parseError }

%token
    select      { TokenSelect }
    from        { TokenFrom }
    where       { TokenWhere }
    crossJoin   { TokenCrossJoin }
    concatJoin  { TokenConcatJoin }
    as          { TokenAs } 
    collect     { TokenCollect }
    sequential  { TokenSequential }
    predPick    { TokenPredPick }
    and         { TokenAnd }
    or          { TokenOr }
    xor         { TokenXOr }
    not         { TokenNot }
    equals      { TokenEquals }
    lt          { TokenLT }
    gt          { TokenGT }
    lParen      { TokenLParen }
    rParen      { TokenRParen }
    file        { TokenFile $$ }
    const       { TokenConst $$ }
    labelIdx    { TokenLabelledIndex _ _ }
    int         { TokenInt $$ }
    labelAst    { TokenLabelledAsterisk $$ }
    ast         { TokenAsterisk }
    label       { TokenLabel $$ } 
%%

Statement : select {CSVStatement (File "")}

{

parseError :: [Token] -> a
parseError _ = error "Parse error"

data Statement =
		QueryStatement {
			qsQuery :: QuerySpec
		}
	|   CSVStatement {
			sFile :: CSVFile
		}

data QuerySpec =
		QuerySpec {
			qsElements :: [SelectElement],
			qsTableExpr :: TableExpr
		}
	|   BasicQuerySpec {
			bqsElements :: [SelectElement]
		}

data SelectElement =
		Asterisk
	|   LabelledAsterisk { 
			seLabel :: String
		}
	|   ColIdent -- TODO
	|   InterrowQuery -- TODO

data ColIdent =
		LabelIndex {
			ciLabel :: String,
			ciIndex :: Int
		}
	|   Constant {
			ciValue :: String
		}
	|   Index {
			ciIndex :: Int
		}
	|   GeneratedColumn {
			ciGenerator :: ColGen
		}

data TableExpr =
		NoWhereExpr {
			nwTableRef :: TableReference
		}
	|   WithWhereExpr {
			wwTableRef :: TableReference,
			wwWhereClause :: WhereClause
		}
	|   JustWhereExpr {
			jwWhereClause :: WhereClause
		}

data TableReference =
		JoinTableRef {
			jtrTable :: JoinedTable
		}
	|   SubQueryRef {
			sqrQuery :: SubQuery
		}
	|   CSV {
			csvFile :: CSVFile
		}

data SubQuery =
		ElementTransform {
			sqElements :: [SelectElement]
		}
	|   SubQuery {
			subquerySpec :: QuerySpec
		}

data JoinedTable =
		CrossJoinedTable CrossJoinTable
	|   ConcatJoinedTable ConcatJoinTable

data CrossJoinTable =
		CrossJoinTable TableReference TableReference
	|   CrossJoinTableLL TableReference ColLabel TableReference
	|   CrossJoinTableRL TableReference TableReference ColLabel
	|   CrossJoinTableLRL TableReference ColLabel TableReference ColLabel

data ConcatJoinTable =
		ConcatJoinTable TableReference TableReference
	|   ConcatJoinTableLL TableReference ColLabel TableReference
	|   ConcatJoinTableRL TableReference TableReference ColLabel
	|   ConcatJoinTableLRL TableReference ColLabel TableReference ColLabel

data WhereClause = WhereClause Predicate

data Predicate = 
		BinaryBoolOperation {
			bboOperandA :: Predicate,
			bboOperator :: BooleanOperator,
			bboOperandB :: Predicate
		}
	|   NotOperation {
			noOperand :: Predicate
		}
	|   ComparisonOperation {
			coOperandA :: ColIdent,
			coOperator :: ComparisonOperator,
			coOperandB :: ColIdent
		}

data BooleanOperator = AndOperator | OrOperator | XOrOperator

data ComparisonOperator = EqualsOperator | LTOperator | GTOperator

data InterRowQuery = InterRowQuery { irqTable :: TableReference } -- collect statement

data ColGen = ColGen {
			cgPredicate :: Predicate,
			cgColA :: ColIdent,
			cgColB :: ColIdent
		}

type ColLabel = String


data CSVFile = 
		File {
			filename :: String
		}
}
