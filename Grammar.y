{
module Grammar where
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

Statement : file 									{ CSVStatement (File $1) }
		  | QuerySpec 								{ QueryStatement ($1)}

QuerySpec : select SelectList				 		{ BasicQuerySpec ($2) }
		  | select SelectList TableExpr 			{ QuerySpec $2 $3 }

SelectList : SelectList SelectList                  { $1 ++ $2 }
		   | ast 									{ [Asterisk] }
		   | labelAst 								{ [LabelledAsterisk $1] }
		   | ColIdent 								{ [IdentifiedElement $1] }
		   | InterRowQuery 							{ [IRQ $1] }

ColIdent : labelIdx 								{ (\(TokenLabelledIndex label idx) -> LabelIndex label idx) $1 }
		 | const 									{ Constant $1 }
		 | int 										{ Index $1 }
		 | ColGen 									{ GeneratedColumn $1 }

InterRowQuery : collect TableReference 				{ InterRowQuery $2 }

ColGen : predPick Predicate ColIdent ColIdent 		{ ColGen $2 $3 $4 }

Predicate : not lParen Predicate lParen				{ NotOperation $3 }
		  | Predicate BinaryBoolOperator Predicate  { BinaryBoolOperation $1 $2 $3 }
		  | ColIdent ComparisonOperator ColIdent 	{ ComparisonOperation $1 $2 $3 }

BinaryBoolOperator : and 							{ AndOperator }
				   | or 							{ OrOperator }
				   | xor 							{ XOrOperator }

ComparisonOperator : equals 						{ EqualsOperator }
				   | lt 							{ LTOperator }
				   | gt 							{ GTOperator }



TableExpr : where 									{ JustWhereExpr ( WhereClause (ComparisonOperation (Constant "") EqualsOperator (Constant ""))  ) }

TableReference : file 								{ CSV (File $1) }





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
			qsElements :: SelectList,
			qsTableExpr :: TableExpr
		}
	|   BasicQuerySpec {
			bqsElements :: SelectList
		}

type SelectList = [SelectElement]

data SelectElement =
		Asterisk
	|   LabelledAsterisk {
			seLabel :: String
		}
	|   IdentifiedElement ColIdent -- TODO
	|   IRQ InterRowQuery -- TODO

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
			sqElements :: SelectList
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
