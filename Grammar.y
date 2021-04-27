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
    ast         { TokenAst }
    label       { TokenLabel $$ } 
%%

Statement : select {CSV(File "")}

{

    parseError :: [Token] -> a
    parseError _ = error "Parse error"
    
    data Statement =
            QueryStatement {
                query :: QuerySpec,
            }
        |   CSV {
                file :: CSVFile
            }

    data QuerySpec =
            QuerySpec {
                elements :: [SelectElement],
                tableExpr :: TableExpr
            }
        |   BasicQuerySpec {
                elements :: [SelectElement]
            }
    
    data SelectElement =
            Asterisk
        |   LabelledAsterisk { 
                label :: String
            }
        |   ColIdent -- TODO
        |   InterrowQuery -- TODO

    data ColIdent =
            LabelIndex {
                label :: String,
                index :: Int
            }
        |   Constant {
                value :: String
            }
        |   Index {
                index :: Int
            }
        |   GeneratedColumn {
                generator :: ColGen
            }

    data TableExpr =
            NoWhereExpr {
                tableRef :: TableReference
            }
        |   WithWhereExpr {
                tableRef :: TableReference,
                whereClause :: WhereClause
            }
        |   JustWhereExpr {
                whereClause :: WhereClause
            }
    
    data TableReference =
            JoinTableRef {
                table :: JoinedTable
            }
        |   SubQueryRef {
                query :: SubQuery
            }
        |   CSV {
                file :: CSVFile
            }
    
    data SubQuery =
            ElementTransform {
                [SelectElement] :: elements
            }
        |   SubQuery {
                query :: QuerySpec
            }

    data JoinedTable =
            JoinedTable CrossJoinTable,
        |   JoinedTable ConcatJoinTable

    data CrossJoinTable =
            CrossJoinTable TableReference TableReference,
        |   CrossJoinTable TableReference ColLabel TableReference,
        |   CrossJoinTable TableReference TableReference ColLabel,
        |   CrossJoinTable TableReference ColLabel TableReference ColLabel

    data ConcatJoinTable =
            ConcatJoinTable TableReference TableReference,
        |   ConcatJoinTable TableReference ColLabel TableReference,
        |   ConcatJoinTable TableReference TableReference ColLabel,
        |   ConcatJoinTable TableReference ColLabel TableReference ColLabel
    
    data WhereClause = WhereClause Predicate

    data Predicate = 
            BinaryBoolOperation {
                operandA :: Predicate,
                operator :: BooleanOperator,
                operandB :: Predicate
            }
        |   NotOperation {
                operand :: Predicate
            }
        |   ComparisonOperation {
                operandA :: ColIdent,
                operator :: ComparisonOperator,
                operandB :: ColIdent
            }

    data BooleanOperator = AndOperator | OrOperator | XOrOperator

    data ComparisonOperator = EqualsOperator | LTOperator | GTOperator

    data InterRowQuery = InterRowQuery { table :: TableReference } -- collect statement
    
    data ColGen = ColGen {
                predicate :: Predicate,
                colA :: ColIdent,
                colB :: ColIdent
            }

    type ColLabel = String

    data WhereClause = WhereClause Predicate
    
    data CSVFile = 
            File {
                filename :: String
            }
}
