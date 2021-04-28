import Tokens
import Grammar
import Eval
import System.Environment
import Data.List
import Control.Exception
import System.IO

main :: IO ()
main = catch main' noParse

main' = do (fileName : _ ) <- getArgs
           sourceText <- readFile fileName
           putStrLn ("Parsing : " ++ sourceText)
           let lexedProg = alexScanTokens sourceText
           putStrLn ("Lexed as: " ++ show lexedProg)
           let parsedProg = parse lexedProg
           putStrLn ("Parsed as " ++ (show parsedProg) ++ "\n")
           result <- eval parsedProg
           putStrLn ("Evaluates to: \n" ++ (unparse result) ++ "\n")

noParse :: ErrorCall -> IO ()
noParse e = do let err =  show e
               hPutStr stderr err
               return ()


lexProg ::  String ->  IO ([Token])
lexProg path = do
  sourceText <- readFile path
  return (alexScanTokens sourceText)
