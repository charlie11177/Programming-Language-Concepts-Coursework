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

%left and or xor

%%

Statement : file 									{ CSVStatement (File $1) }
		  | QuerySpec 								{ QueryStatement ($1)}

QuerySpec : select SelectList				 		{ BasicQuerySpec ($2) }
		  | select SelectList TableExpr 			{ QuerySpec $2 $3 }

SelectList : SelectElement 							{ [$1] }
		   | SelectList SelectElement 				{ $1 ++ [$2] }

SelectElement : ast 								{ Asterisk }
			  | labelAst 							{ LabelledAsterisk $1 }
			  | ColIdent 							{ IdentifiedElement $1 }
			  | InterRowQuery 						{ IRQ $1 }

ColIdent : labelIdx 								{ (\(TokenLabelledIndex label idx) -> LabelIndex label idx) $1 }
		 | const 									{ Constant $1 }
		 | int 										{ Index $1 }
		 | ColGen 									{ GeneratedColumn $1 }


InterRowQuery : lParen collect TableReference rParen 	{ InterRowQuery $3 }

ColGen : predPick Predicate ColIdent ColIdent 		{ ColGen $2 $3 $4 }

TableExpr : from TableReference 					{ NoWhereExpr $2 }
		  | from TableReference WhereClause         { WithWhereExpr $2 $3 }
		  | WhereClause 							{ JustWhereExpr $1 }

TableReference : JoinedTable 						{ JoinTableRef $1 }
			   | SubQuery 							{ SubQueryRef $1 }
			   | file 								{ CSV (File $1) }

JoinedTable : CrossJoinTable 						{ CrossJoinedTable $1 }
			| ConcatJoinTable 						{ ConcatJoinedTable $1 }

CrossJoinTable : crossJoin lParen TableReference rParen lParen TableReference rParen 						{ CrossJoinTable $3 $6 }
			   | crossJoin lParen TableReference rParen as label lParen TableReference rParen				{ CrossJoinTableLL $3 $6 $8 }
			   | crossJoin lParen TableReference rParen lParen TableReference rParen as label 				{ CrossJoinTableRL $3 $6 $9 }
			   | crossJoin lParen TableReference rParen as label lParen TableReference rParen as label 		{ CrossJoinTableLRL $3 $6 $8 $11 }

ConcatJoinTable : concatJoin lParen TableReference rParen lParen TableReference rParen						{ ConcatJoinTable $3 $6 }
				| concatJoin lParen TableReference rParen as label lParen TableReference rParen 			{ ConcatJoinTableLL $3 $6 $8 }
				| concatJoin lParen TableReference rParen lParen TableReference rParen as label 			{ ConcatJoinTableRL $3 $6 $9 }
				| concatJoin lParen TableReference rParen as label lParen TableReference rParen as label  	{ ConcatJoinTableLRL $3 $6 $8 $11 }

SubQuery : sequential SelectList 					{ ElementTransform $2 }
		 | lParen QuerySpec rParen 					{ SubQuery $2 }

WhereClause : where Predicate 						{ WhereClause $2 }

Predicate : not lParen Predicate lParen				{ NotOperation $3 }
		  | Predicate and Predicate     			{ BinaryBoolOperation $1 AndOperator $3 }
		  | Predicate or Predicate 					{ BinaryBoolOperation $1 OrOperator $3 }
		  | Predicate xor Predicate 				{ BinaryBoolOperation $1 XOrOperator $3}
		  | ColIdent ComparisonOperator ColIdent 	{ ComparisonOperation $1 $2 $3 }
		  | lParen Predicate rParen 				{ BracketedPredicate $2 }

ComparisonOperator : equals 						{ EqualsOperator }
				   | lt 							{ LTOperator }
				   | gt 							{ GTOperator }

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
	| 	BracketedPredicate {
			predicate :: Predicate	
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
