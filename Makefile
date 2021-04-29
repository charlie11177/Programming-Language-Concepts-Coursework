csvql: Main.hs
		ghc Main.hs -o csvsql

Main.hs: Tokens.hs Grammar.hs Eval.hs Tokens.x Grammar.y
		alex Tokens.x
		happy Grammar.y
