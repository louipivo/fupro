import Data.Char

main = do
  putStrLn "Eingabedatei"
  e <- getLine
  ein <- readFile e
  putStrLn "Ausgabedatei"
  aus <- getLine
  putStrLn "Wähle Ausgabeart: 1 für Kleinschreibung oder 2 für Großschreibung"
  art <- getLine
  if(art == "1")
    then writeFile aus (map toLower ein)
    else do
      writeFile aus (map toUpper ein)
