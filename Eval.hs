module Eval where
import Grammar
import Data.List

type Table = [(Maybe String, Maybe Int, [String])]

eval :: Statement -> IO Table
eval (CSVStatement x) = processFile x
eval (QueryStatement qs) = evalQuerySpec qs Nothing

selectColByCI :: ColIdent -> Maybe Table -> Maybe (Maybe String, Maybe Int, [String])
selectColByCI (Constant ciValue) _ = Just (Nothing, Nothing, [ciValue..])
selectColByCI (GeneratedColumn (ColGen predicate colA colB)) scope | colAResult == Nothing || colBResult == Nothing = Nothing
                                                                   | otherwise                                      = Just (Nothing, Nothing, [switchOnResult predResult colAElem colBElem | ((predResult,colAElem),colBElem)<-zip (zip predicateResults (extractContent colAResult)) (extractContent colBResult)])
  where
    switchOnResult :: Bool -> String -> String -> String
    switchOnResult True aElem _ = aElem
    switchOnResult False _ bElem = bElem
    colAResult = selectColByCI colA scope
    colBResult = selectColByCI colA scope
    predicateResults = applyPredicate predicate scope
    extractContent :: Maybe (Maybe String, Maybe Int, [String]) -> [String]
    extractContent Nothing = error "unreachable"
    extractContent (Just (label, index, content)) = content
selectColByCI _ Nothing = Nothing
selectColByCI (LabelIndex ciLabel ciIndex) (Just scope) = Just (head (filter (\(a, b, c) -> (filterOp a b)) scope))
  where
    filterOp :: Maybe String -> Maybe Int -> Bool
    filterOp Nothing _ = False
    filterOp _ Nothing = False
    filterOp (Just colLabel) (Just colIndex) = colLabel == ciLabel && ciIndex == colIndex
selectColByCI (Index index) (Just scope) = Just (scope!!index)


selectCols :: SelectList -> Maybe Table -> IO Table --TODO: IO monad stuff
selectCols sl scope = do
                      subTables <- mapM (\element -> selectColsBySE element scope) sl --TODO: shorten infinite length columns to be the length of the rest of the table
                      return (concat subTables)
  where
    selectColsBySE :: SelectElement -> Maybe Table -> IO Table
    selectColsBySE Asterisk Nothing = return []
    selectColsBySE Asterisk (Just scope) = return scope
    selectColsBySE (LabelledAsterisk seLabel) (Just scope) = return (filter (\(colLabel, index, col) -> filterOp colLabel) scope)
      where
        filterOp :: Maybe String -> Bool
        filterOp Nothing = False
        filterOp (Just str) = str == seLabel
    selectColsBySE (IdentifiedElement colIdent) scope = case elementResult of
                                                          Nothing  -> return []
                                                          (Just a) -> return [a]
      where
        elementResult = selectColByCI colIdent scope
    selectColsBySE (IRQ _) Nothing = error "Inter-Row queries require a FROM block to be provided"
    selectColsBySE (IRQ irq) (Just scope) = evalInterRowQuery irq scope

evalInterRowQuery :: InterRowQuery -> Table -> IO Table
evalInterRowQuery (InterRowQuery tableRef) scope = do
                                                   evaluatedTables <- mapM (\row -> evalTableReference tableRef (Just row)) records --[Table]
                                                   let tableContents = map (\(_,_,cols)->cols) (concat evaluatedTables) --[[String]]
                                                   let transposedTableContents = transpose tableContents --[[String]]
                                                   let rebuiltTable = map (\contents -> (Nothing,Nothing,contents)) transposedTableContents --Table
                                                   return rebuiltTable
  where
    expandedCols = [ map (\cell -> (label, index, cell)) col | (label, index, col) <- scope]
    records = transpose expandedCols

repeatForever :: a -> [a]
repeatForever x = xs
    where xs = x : xs

applyPredicate :: Predicate -> Maybe Table -> [Bool]
applyPredicate PredValueTrue _ = repeatForever True
applyPredicate PredValueFalse _ = repeatForever False
applyPredicate (BracketedPredicate predicate) scope = applyPredicate predicate scope
applyPredicate (NotOperation predicate) scope = [not value | value <- (applyPredicate predicate scope)]
applyPredicate (BinaryBoolOperation predA operator predB) scope = [(opToFunction operator) aResult bResult | (aResult, bResult) <- zip (applyPredicate predA scope) (applyPredicate predB scope)]
  where
    opToFunction :: BooleanOperator -> (Bool -> Bool -> Bool)
    opToFunction AndOperator = (&&)
    opToFunction OrOperator = (||)
    opToFunction XOrOperator = (\a b -> (a||b) && not(a==b))
applyPredicate (ComparisonOperation _ _ _ ) Nothing = []
applyPredicate (ComparisonOperation colA operator colB) (Just scope) = case (colAContents, colBContents) of
                                                                        (Nothing,Nothing)                -> error "unreachable"
                                                                        (Just aContents, Just bContents) -> [(opToFunction operator) aElem bElem| (aElem, bElem) <- zip aContents bContents]
  where
    colAContents = (\(_,_,contents)->contents) <$> (selectColByCI colA (Just scope))
    colBContents = (\(_,_,contents)->contents) <$> (selectColByCI colA (Just scope))
    opToFunction :: ComparisonOperator -> (String -> String -> Bool)
    opToFunction EqualsOperator = (==)
    opToFunction LTOperator = (<)
    opToFunction GTOperator = (>)


evalQuerySpec :: QuerySpec -> Maybe [(Maybe String, Maybe Int, String)] -> IO Table
evalQuerySpec (QuerySpec sl te) row = do
                                      producedTable <- evalTableExpr te row
                                      result <- selectCols sl producedTable
                                      return result
evalQuerySpec (BasicQuerySpec sl) _ = selectCols sl (Just [])



evalTableExpr :: TableExpr -> Maybe [(Maybe String, Maybe Int, String)] -> IO (Maybe Table)
evalTableExpr (NoWhereExpr tr) row = do
                                  table <- evalTableReference tr row
                                  return (Just table)
evalTableExpr (WithWhereExpr tr wc) row = do
                                      table <- evalTableReference tr row
                                      let filteredTable = evalWhereClause wc table
                                      return (Just filteredTable)
evalTableExpr (JustWhereExpr wc) _ = return (Just (evalWhereClause wc [])) --TODO: deal with Nothing cases
                                   

evalTableReference :: TableReference -> Maybe [(Maybe String, Maybe Int, String)] -> IO Table
evalTableReference (JoinTableRef table) row = evalJoinedTable table row 
evalTableReference (SubQueryRef sqr) row = evalSubQuery sqr row
evalTableReference (CSV csv) row = processFile csv


evalJoinedTable :: JoinedTable -> Maybe[(Maybe String, Maybe Int, String)] -> IO Table
evalJoinedTable (ConcatJoinedTable table) row = evalConcatJoinTable table row
evalJoinedTable (CrossJoinedTable table) row = evalCrossJoinTable table row

evalSubQuery :: SubQuery -> Maybe [(Maybe String, Maybe Int, String)] -> IO Table
evalSubQuery (SubQuery qs) row = evalQuerySpec qs row
evalSubQuery (ElementTransform sl) row = [(Nothing, Nothing, selectFromRow sl row)]
  where
    selectFromRow :: SelectList -> Maybe [(Maybe String, Maybe Int, String)] -> [String] --TODO: IO monad stuff
    selectFromRow sl row = do
                          subTables <- mapM (\element -> selectColsBySE element row) sl --TODO: shorten infinite length columns to be the length of the rest of the table
                          return (concat subTables)
      where
        extractContent :: (Maybe String, Maybe Int, String) -> String
        extractContent (_,_,content) = content
        selectColsBySE :: SelectElement -> [(Maybe String, Maybe Int, String)] -> IO[String]
        selectColsBySE Asterisk row = return (map extractContent row )
        selectColsBySE (LabelledAsterisk seLabel) row = return (map extractContent (filter (\(colLabel, index, content) -> colLabel == seLabel) row))
        selectColsBySE (IdentifiedElement colIdent) scope = return selectColByCI colIdent scope
          where
            selectColByCI :: ColIdent -> [(Maybe String, Maybe Int, String)] -> String
            selectColByCI (LabelIndex ciLabel ciIndex) row = head (filter (\(colLabel, colIndex, col) -> colLabel == ciLabel && ciIndex == colIndex) row)
            selectColByCI (Constant ciValue) _ = ciValue
            selectColByCI (Index index) row = extractContent (row!!index)
            selectColByCI (GeneratedColumn (ColGen predicate colA colB)) row = switchOnResult (applyPredicateRow predicate row) colAContents colBContents
              where
                switchOnResult :: Bool -> String -> String -> String
                switchOnResult True aElem _ = aElem
                switchOnResult False _ bElem = bElem
                colAContents = selectColByCI colA row
                colBContents = selectColByCI colA scope

                applyPredicateRow :: Predicate -> [(Maybe String, Maybe Int, String)] -> Bool
                applyPredicateRow PredValueTrue _ = True
                applyPredicateRow PredValueFalse _ = False
                applyPredicateRow (NotOperation predicate) row = not (applyPredicateRow predicate row)
                applyPredicateRow (BinaryBoolOperation predicateA operator predicateB) row = (opToFunction operator) (applyPredicateRow predicateA row) (applyPredicateRow predicateB row)
                  where
                    opToFunction :: BooleanOperator -> (Bool -> Bool -> Bool)
                    opToFunction AndOperator = (&&)
                    opToFunction OrOperator = (||)
                    opToFunction XOrOperator = (\a b -> (a||b) && not(a==b))
                applyPredicateRow (ComparisonOperation colA operator colB) row = (opToFunction operator) (selectColByCI colA) (selectColByCI colB)
                  where
                    opToFunction :: ComparisonOperator -> (String -> String -> Bool)
                    opToFunction EqualsOperator = (==)
                    opToFunction LTOperator = (<)
                    opToFunction GTOperator = (>)
        selectColsBySE (IRQ irq) scope = error "You cannot perform an InterRowQuery whilst already within another row's scope"

evalCrossJoinTable :: CrossJoinTable -> Maybe[(Maybe String, Maybe Int, String)] ->  IO Table
evalCrossJoinTable (CrossJoinTable tr1 tr2) row = do
  table1 <- evalTableReference tr1 row
  table2 <- evalTableReference tr2 row
  let aLength = (\(_,_,cols) -> length cols) (head table1)
  let bLength = (\(_,_,cols) -> length cols) (head table2)
  return ((\(label, index, cols) -> (label, index, concat[replicate bLength element |element <- cols])) table1) ++ (\(label, index, cols) -> (label,index,concat(replicate aLength cols))) table2
evalCrossJoinTable (CrossJoinTableLL tr1 lbl1 tr2) row = do
  table1 <- evalTableReference tr1 row
  table1 <- reLabel table1 lbl1
  table2 <- evalTableReference tr2 row
  let aLength = (\(_,_,cols) -> length cols) (head table1)
  let bLength = (\(_,_,cols) -> length cols) (head table2)
  return ((\(label, index, cols) -> (label, index, concat[replicate bLength element |element <- cols])) table1) ++ (\(label, index, cols) -> (label,index,concat(replicate aLength cols))) table2
evalCrossJoinTable (CrossJoinTableRL tr1 tr2 lbl2) row = do
  table1 <- evalTableReference tr1 row
  table2 <- evalTableReference tr2 row
  table2 <- reLabel table2 lbl2
  let aLength = (\(_,_,cols) -> length cols) (head table1)
  let bLength = (\(_,_,cols) -> length cols) (head table2)
  return ((\(label, index, cols) -> (label, index, concat[replicate bLength element |element <- cols])) table1) ++ (\(label, index, cols) -> (label,index,concat(replicate aLength cols))) table2
evalCrossJoinTable (CrossJoinTableLRL tr1 lbl1 tr2 lbl2) row = do
  table1 <- evalTableReference tr1 row
  table1 <- reLabel table1 lbl1
  table2 <- evalTableReference tr2 row
  table2 <- reLabel table2 lbl2
  let aLength = (\(_,_,cols) -> length cols) (head table1)
  let bLength = (\(_,_,cols) -> length cols) (head table2)
  return ((\(label, index, cols) -> (label, index, concat[replicate bLength element |element <- cols])) table1) ++ (\(label, index, cols) -> (label,index,concat(replicate aLength cols))) table2

reLabel :: Table -> String -> IO Table
reLabel tbl lbl = do
  let zippedCols = zip [0..] tbl
  return map (\(i, (Just label, _, cols)) -> (Just lbl, Just i, cols)) zippedCols


evalConcatJoinTable :: ConcatJoinTable -> Maybe[(Maybe String, Maybe Int, String)] -> IO Table
evalConcatJoinTable (ConcatJoinTable tr1 tr2) row = do
  table1 <- evalTableReference tr1 row
  table2 <- evalTableReference tr2 row
  let aLength = (\(_,_,cols) -> length cols) (head table1)
  let bLength = (\(_,_,cols) -> length cols) (head table2)
  let shortestLength = min aLength bLength
  let cuttable1 = map (\(lb, ind, cols) -> (lb, ind, take shortestLength cols)) table1
  let cuttable2 = map (\(lb, ind, cols) -> (lb, ind, take shortestLength cols)) table2
  return cuttable1 ++ cuttable2
evalConcatJoinTable (ConcatJoinTableLL tr1 lbl1 tr2) row = do
  table1 <- evalTableReference tr1 row
  table1 <- reLabel table1 lbl1
  table2 <- evalTableReference tr2 row
  let aLength = (\(_,_,cols) -> length cols) (head table1)
  let bLength = (\(_,_,cols) -> length cols) (head table2)
  let shortestLength = min aLength bLength
  let cuttable1 = map (\(lb, ind, cols) -> (lb, ind, take shortestLength cols)) table1
  let cuttable2 = map (\(lb, ind, cols) -> (lb, ind, take shortestLength cols)) table2
  return cuttable1 ++ cuttable2
evalConcatJoinTable (ConcatJoinTableRL tr1 tr2 lbl2) row = do
  table1 <- evalTableReference tr1 row
  table2 <- evalTableReference tr2 row
  table2 <- reLabel table2 lbl2
  let aLength = (\(_,_,cols) -> length cols) (head table1)
  let bLength = (\(_,_,cols) -> length cols) (head table2)
  let shortestLength = min aLength bLength
  let cuttable1 = map (\(lb, ind, cols) -> (lb, ind, take shortestLength cols)) table1
  let cuttable2 = map (\(lb, ind, cols) -> (lb, ind, take shortestLength cols)) table2
  return cuttable1 ++ cuttable2
evalConcatJoinTable (ConcatJoinTableLRL tr1 lbl1 tr2 lbl2) row = do
  table1 <- evalTableReference tr1 row
  table1 <- reLabel table1 lbl1
  table2 <- evalTableReference tr2 row
  table2 <- reLabel table2 lbl2
  let aLength = (\(_,_,cols) -> length cols) (head table1)
  let bLength = (\(_,_,cols) -> length cols) (head table2)
  let shortestLength = min aLength bLength
  let cuttable1 = map (\(lb, ind, cols) -> (lb, ind, take shortestLength cols)) table1
  let cuttable2 = map (\(lb, ind, cols) -> (lb, ind, take shortestLength cols)) table2
  return cuttable1 ++ cuttable2


evalWhereClause :: WhereClause -> Table -> Table --TODO: Nothing cases
evalWhereClause (WhereClause p) scope = [(label, index, [cell | (predResult, cell) <- zip col (applyPredicate p scope), predResult]) | (label, index, col) <- scope]
unparse :: Table -> String

unparse tb = unlines (map (intercalate "," ) (transpose (map getRow tb)))
  where getRow (_, _, xs) = xs

processFile :: CSVFile -> IO Table
processFile (File path) = do
  sourceText <- readFile path
  let name = take (length path - 4) path
  let cols = splitCols sourceText
  return ([(name, fst x, snd x) | x <- zip [0..] cols])

splitCols :: String -> [[String]]
splitCols xs = transpose (map (splitOn ',') (splitOn '\n' xs))

splitOn :: Char -> String -> [String]
splitOn c [] = [""]
splitOn c (x:[]) | x == c = [[]]
splitOn c (x:xs) | x == c = "":(splitOn c xs)
                 | otherwise = (x : head (splitOn c xs)):(tail $ splitOn c xs)
