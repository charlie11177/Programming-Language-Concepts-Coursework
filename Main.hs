import Tokens
import System.Environment
import Data.List
import System.IO

main :: IO ()
main = do
  args <- getArgs
  let path = args!!0
  sourceText <- readFile path
  putStrLn ("Lexing: " ++ sourceText)
  let lexedProg = alexScanTokens sourceText
  putStrLn ("Lexed as: " ++ show lexedProg)


lexProg ::  String ->  IO ([Token])
lexProg path = do
  sourceText <- readFile path
  return (alexScanTokens sourceText)
