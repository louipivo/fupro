import Data.Char

main = do
	putStrLn "Eingabedatei:"
	e <- getLine
	ein <- readFile e
	putStrLn "Ausgabedatei:"
	aus <- getLine
	putStrLn "Wähle Ausgabeart: 1 für Kleinschreibung 2 für Großschreibung"
	art <- getLine
	| art == "1" = writeFile aus (map toLower ein)
	|	otherwise = writeFile aus (map toUpper ein)
