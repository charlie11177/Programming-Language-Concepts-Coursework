module Eval where
import Grammar
import Data.List
import Debug.Trace

type Table = [(Maybe String, Maybe Int, [String])]

debugTrace :: Show a => String -> a ->a
debugTrace str a = trace (str ++ (show a) ++ " ") a

eval :: Statement -> IO Table
eval (CSVStatement x) = processFile x
eval (QueryStatement qs) = evalQuerySpec qs Nothing

selectColByCI :: ColIdent -> Maybe Table -> Maybe (Int, (Maybe String, Maybe Int, [String]))
selectColByCI (Constant ciValue) _ = Just (maxBound, (Nothing, Nothing, (repeatForever ciValue)))
selectColByCI (GeneratedColumn (ColGen predicate colA colB)) scope = case (colAOutput, colBOutput) of
                        (Nothing,_) -> Nothing
                        (_,Nothing) -> Nothing
                        (Just(colALength, colAResult), Just(colBLength, colBResult)) -> (Just (min colALength colBLength, (Nothing, Nothing, [switchOnResult predResult colAElem colBElem | ((predResult,colAElem),colBElem)<-zip (zip predicateResults (extractContent colAResult)) (extractContent colBResult)])))
  where
    switchOnResult :: Bool -> String -> String -> String
    switchOnResult True aElem _ = aElem
    switchOnResult False _ bElem = bElem
    colAOutput = selectColByCI colA scope
    colBOutput = selectColByCI colB scope
    predicateResults = applyPredicate predicate scope
    extractContent (label, index, content) = content
selectColByCI _ Nothing = Nothing
selectColByCI (LabelIndex ciLabel ciIndex) (Just scope) = (Just (length content, outputVal))
  where
    filterOp :: Maybe String -> Maybe Int -> Bool
    filterOp Nothing _ = False
    filterOp _ Nothing = False
    filterOp (Just colLabel) (Just colIndex) = colLabel == ciLabel && ciIndex == colIndex
    outputVal@(_,_,content) = head (filter (\(a, b, c) -> (filterOp a b)) scope)
selectColByCI (Index index) (Just scope) = Just ((\(_,_,content) -> length content) (scope!!index),scope!!index)


selectCols :: SelectList -> Maybe Table -> IO Table --TODO: IO monad stuff
selectCols sl scope = do
                      result <- mapM (\element -> selectColsBySE element scope) sl --TODO: shorten infinite length columns to be the length of the rest of the table
                      let minLength = minimum (map fst result)
                      let subTables = map snd result
                      return ((trimToLength $ concat subTables) minLength)
  where
    getMinLength :: Table -> Int
    getMinLength table = minimum (map (\(_,_,content) -> length content) table)
    selectColsBySE :: SelectElement -> Maybe Table -> IO (Int,Table)
    selectColsBySE Asterisk Nothing = return (0,[])
    selectColsBySE Asterisk (Just scope) = return (getMinLength scope, scope)
    selectColsBySE (LabelledAsterisk seLabel) (Just scope) = return (getMinLength returnVal, returnVal)
      where
        filterOp :: Maybe String -> Bool
        filterOp Nothing = False
        filterOp (Just str) = str == seLabel
        returnVal = filter (\(colLabel, index, col) -> filterOp colLabel) scope
    selectColsBySE (IdentifiedElement colIdent) scope = case elementResult of
                                                          Nothing  -> return (0,[])
                                                          (Just (colLength,a)) -> return (colLength,[a])
      where
        elementResult = selectColByCI colIdent scope
    selectColsBySE (IRQ _) Nothing = error "Inter-Row queries require a FROM block to be provided"
    selectColsBySE (IRQ irq) (Just scope) = do
                      result <- evalInterRowQuery irq scope
                      return (getMinLength result, result)
    trimToLength :: Table -> Int -> Table
    trimToLength table maxLength = map (\(label, index, content) -> (label, index, take maxLength content)) table

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
    colAContents = (\(_,(_,_,contents))->contents) <$> (selectColByCI colA (Just scope))
    colBContents = (\(_,(_,_,contents))->contents) <$> (selectColByCI colB (Just scope))
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
evalSubQuery (ElementTransform sl) row = do
                      return [(Nothing, Nothing, selectFromRow sl row)]
  where
    selectFromRow :: SelectList -> Maybe [(Maybe String, Maybe Int, String)] -> [String] --TODO: IO monad stuff
    selectFromRow sl row = concat $ map (\element -> selectColsBySE element row) sl --TODO: shorten infinite length columns to be the length of the rest of the table
      where
        extractContent :: (Maybe String, Maybe Int, String) -> String
        extractContent (_,_,content) = content
        selectColsBySE :: SelectElement -> Maybe [(Maybe String, Maybe Int, String)] -> [String]
        selectColsBySE Asterisk Nothing = []
        selectColsBySE Asterisk (Just row) = (map extractContent row )
        selectColsBySE (LabelledAsterisk _) Nothing = []
        selectColsBySE (LabelledAsterisk seLabel) (Just row) = map extractContent (filter (\(colLabel, index, content) -> filterOp colLabel) row)
          where
            filterOp :: Maybe String -> Bool
            filterOp Nothing = False
            filterOp (Just colLabel) = colLabel == seLabel
        selectColsBySE (IdentifiedElement colIdent) row = case (selectColByCI colIdent row) of
                                                           Nothing       -> []
                                                           (Just output) -> [output]
          where
            selectColByCI :: ColIdent -> Maybe [(Maybe String, Maybe Int, String)] -> Maybe String
            selectColByCI (Constant ciValue) _ = Just ciValue
            selectColByCI (GeneratedColumn (ColGen predicate colA colB)) row = (switchOnResult (applyPredicateRow predicate row) colAContents colBContents)
              where
                switchOnResult :: Bool -> Maybe String -> Maybe String -> Maybe String
                switchOnResult True aElem _ = aElem
                switchOnResult False _ bElem = bElem
                colAContents = selectColByCI colA row
                colBContents = selectColByCI colB row
                boolResult = applyPredicateRow predicate row

                applyPredicateRow :: Predicate -> Maybe [(Maybe String, Maybe Int, String)] -> Bool
                applyPredicateRow PredValueTrue _ = True
                applyPredicateRow PredValueFalse _ = False
                applyPredicateRow (NotOperation predicate) row = not (applyPredicateRow predicate row)
                applyPredicateRow (BinaryBoolOperation predicateA operator predicateB) row = (opToFunction operator) (applyPredicateRow predicateA row) (applyPredicateRow predicateB row)
                  where
                    opToFunction :: BooleanOperator -> (Bool -> Bool -> Bool)
                    opToFunction AndOperator = (&&)
                    opToFunction OrOperator = (||)
                    opToFunction XOrOperator = (\a b -> (a||b) && not(a==b))
                applyPredicateRow (ComparisonOperation colA operator colB) row = applyOperator (opToFunction operator) (selectColByCI colA row) (selectColByCI colB row)
                  where
                    opToFunction :: ComparisonOperator -> (String -> String -> Bool)
                    opToFunction EqualsOperator = (==)
                    opToFunction LTOperator = (<)
                    opToFunction GTOperator = (>)
                    applyOperator :: (String -> String -> Bool) -> Maybe String -> Maybe String -> Bool
                    applyOperator _ Nothing _ = False
                    applyOperator _ _ Nothing = False
                    applyOperator op (Just a) (Just b) = op a b
            selectColByCI _ Nothing = Nothing
            selectColByCI (LabelIndex ciLabel ciIndex) (Just row) = (Just (head [content |(label, index, content) <- row, label == (Just ciLabel), index == (Just ciIndex)]))
            selectColByCI (Index index) (Just row) = Just (extractContent (row!!index))
        selectColsBySE (IRQ irq) scope = error "You cannot perform an InterRowQuery whilst already within another row's scope"

evalCrossJoinTable :: CrossJoinTable -> Maybe [(Maybe String, Maybe Int, String)] ->  IO Table
evalCrossJoinTable (CrossJoinTable tr1 tr2) row = do
  table1 <- evalTableReference tr1 row
  table2 <- evalTableReference tr2 row
  let aLength = case table1 of
                   []     -> 0
                   (a:as) -> (\(_,_,cols) -> length cols) a
  let bLength = case table2 of
                   []     -> 0
                   (b:bs) -> (\(_,_,cols) -> length cols) b
  return ((map (\(label, index, cols) -> (label, index, concat[replicate bLength element |element <- cols])) table1) ++ (map (\(label, index, cols) -> (label,index,concat(replicate aLength cols))) table2))
evalCrossJoinTable (CrossJoinTableLL tr1 lbl1 tr2) row = do
  table1 <- evalTableReference tr1 row
  table1 <- reLabel table1 lbl1
  table2 <- evalTableReference tr2 row
  let aLength = (\(_,_,cols) -> length cols) (head table1)
  let bLength = (\(_,_,cols) -> length cols) (head table2)
  return ((map (\(label, index, cols) -> (label, index, concat[replicate bLength element |element <- cols])) table1) ++ (map (\(label, index, cols) -> (label,index,concat(replicate aLength cols))) table2))
evalCrossJoinTable (CrossJoinTableRL tr1 tr2 lbl2) row = do
  table1 <- evalTableReference tr1 row
  table2 <- evalTableReference tr2 row
  table2 <- reLabel table2 lbl2
  let aLength = (\(_,_,cols) -> length cols) (head table1)
  let bLength = (\(_,_,cols) -> length cols) (head table2)
  return ((map (\(label, index, cols) -> (label, index, concat[replicate bLength element |element <- cols])) table1) ++ (map (\(label, index, cols) -> (label,index,concat(replicate aLength cols))) table2))
evalCrossJoinTable (CrossJoinTableLRL tr1 lbl1 tr2 lbl2) row = do
  table1 <- evalTableReference tr1 row
  table1 <- reLabel table1 lbl1
  table2 <- evalTableReference tr2 row
  table2 <- reLabel table2 lbl2
  let aLength = (\(_,_,cols) -> length cols) (head table1)
  let bLength = (\(_,_,cols) -> length cols) (head table2)
  return ((map (\(label, index, cols) -> (label, index, concat[replicate bLength element |element <- cols])) table1) ++ (map (\(label, index, cols) -> (label,index,concat(replicate aLength cols))) table2))

reLabel :: Table -> String -> IO Table
reLabel tbl lbl = do
  let zippedCols = zip [0..] tbl
  return (map (\(i, (_, _, cols)) -> (Just lbl, Just i, cols)) zippedCols)


evalConcatJoinTable :: ConcatJoinTable -> Maybe[(Maybe String, Maybe Int, String)] -> IO Table
evalConcatJoinTable (ConcatJoinTable tr1 tr2) row = do
  table1 <- evalTableReference tr1 row
  table2 <- evalTableReference tr2 row
  let aLength = (\(_,_,cols) -> length cols) (head table1)
  let bLength = (\(_,_,cols) -> length cols) (head table2)
  let shortestLength = min aLength bLength
  let cuttable1 = map (\(lb, ind, cols) -> (lb, ind, take shortestLength cols)) table1
  let cuttable2 = map (\(lb, ind, cols) -> (lb, ind, take shortestLength cols)) table2
  return (cuttable1 ++ cuttable2)
evalConcatJoinTable (ConcatJoinTableLL tr1 lbl1 tr2) row = do
  table1 <- evalTableReference tr1 row
  table1 <- reLabel table1 lbl1
  table2 <- evalTableReference tr2 row
  let aLength = (\(_,_,cols) -> length cols) (head table1)
  let bLength = (\(_,_,cols) -> length cols) (head table2)
  let shortestLength = min aLength bLength
  let cuttable1 = map (\(lb, ind, cols) -> (lb, ind, take shortestLength cols)) table1
  let cuttable2 = map (\(lb, ind, cols) -> (lb, ind, take shortestLength cols)) table2
  return (cuttable1 ++ cuttable2)
evalConcatJoinTable (ConcatJoinTableRL tr1 tr2 lbl2) row = do
  table1 <- evalTableReference tr1 row
  table2 <- evalTableReference tr2 row
  table2 <- reLabel table2 lbl2
  let aLength = (\(_,_,cols) -> length cols) (head table1)
  let bLength = (\(_,_,cols) -> length cols) (head table2)
  let shortestLength = min aLength bLength
  let cuttable1 = map (\(lb, ind, cols) -> (lb, ind, take shortestLength cols)) table1
  let cuttable2 = map (\(lb, ind, cols) -> (lb, ind, take shortestLength cols)) table2
  return (cuttable1 ++ cuttable2)
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
  return (cuttable1 ++ cuttable2)


evalWhereClause :: WhereClause -> Table -> Table --TODO: Nothing cases
evalWhereClause (WhereClause p) scope = [(label, index, [cell | (cell, predResult) <- zip col (applyPredicate p (Just scope)), predResult]) | (label, index, col) <- scope]
unparse :: Table -> String

unparse tb = unlines (sort (map (intercalate "," ) (transpose (map getRow tb))))
  where getRow (_, _, xs) = xs



processFile :: CSVFile -> IO Table
processFile (File path) = do
  sourceText <- readFile path
  let name = take (length path - 4) path
  let cols = splitCols sourceText
  let result = case sourceText of
                    [] -> []
                    as -> ([(Just name, Just (fst x), snd x) | x <- zip [0..] cols])
  return result

splitCols :: String -> [[String]]
splitCols xs = map (map (dropWhileEnd (==' ')))(map (map (dropWhile (==' '))) cols)
  where
    arity = length (filter (==',') (head (splitOn '\n' xs))) + 1
    rows = (map (splitOn ',') (splitOn '\n' xs))
    cols = transpose rowsSameArity
    rowsSameArity = map (\x -> if ((length x) < arity) then ( x ++ (replicate (arity - (length x)) "")) else x) rows



splitOn :: Char -> String -> [String]
splitOn c [] = [""]
splitOn c (x:[]) | x == c = [[]]
splitOn c (x:xs) | x == c = "":(splitOn c xs)
                 | otherwise = (x : head (splitOn c xs)):(tail $ splitOn c xs)
