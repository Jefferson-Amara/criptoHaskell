import Data.IORef
import Data.List
import Control.Monad
import Data.Char

------------------------------------------------- BEGIN AUX ------------------------------------------
splitToListofLists:: String ->[[String]]
splitToListofLists (x:rest) = [[x]]:(splitToListofLists rest)
splitToListofLists rest = [[rest]]
------------------------------------------------- END AUX --------------------------------------------
------------------------------------------------BEGIN CESAR-------------------------------------------
let2Int :: Char -> Char -> Int
let2Int x c = ord c - ord x

lower2Int :: Char -> Int
lower2Int = let2Int 'a'

upper2Int :: Char -> Int
upper2Int = let2Int 'A'

int2Let :: Char -> Int -> Char
int2Let x n = chr (ord x + n)

int2lower :: Int -> Char
int2lower = int2Let 'a'

int2upper :: Int -> Char
int2upper = int2Let 'A'

shift :: Int -> Char -> Char
shift n c | isLower c = int2lower ((lower2Int c + n) `mod` 26)
          | isUpper c = int2upper ((upper2Int c + n) `mod` 26)
          | otherwise = c

cesarEncode :: Int -> String -> String
cesarEncode n xs = [shift n x | x <- xs]

cesarDecode :: Int -> String -> String
cesarDecode n = cesarEncode (-n)
------------------------------------------------END CESAR-----------------------------------------------
----------------------------------------------BEGIN VIGENERE--------------------------------------------
vigenereEncode :: [Char] -> [Char] -> [Char]
vigenereEncode x [] = x        -- Se o caractere da palavra chave eh vazio, o resultado eh o caractere original
vigenereEncode [] y = []       -- Se o caractere original eh vazio, o resultado tambem eh um caractere vazio
vigenereEncode (x:xs) (y:ys) = (hash x y):(vigenereEncode xs (ys++[y])) where -- Relaciona o par de caracteres e concatena o resultado do deslocamento
    hash a b | isLower a && isLower b = chr $ 97 + mod (66 + ord a + ord b) 26 -- Hash para caracteres minúsculos
                  | isUpper a && isUpper b = chr $ 65 + mod (ord a + ord b) 26 -- Hash para caracteres maiúsculos
                  | otherwise = ' ' -- Caracteres nao alfabeticos nao sao computados


vigenereDecode :: [Char] -> [Char] -> [Char]
vigenereDecode x [] = x
vigenereDecode [] y = []    
vigenereDecode (x:xs) (y:ys) = (hash x y):(vigenereDecode xs (ys++[y])) where
    hash a b | isLower a && isLower b = chr $ 97 + mod (ord a - ord b) 26
                  | isUpper a && isUpper b = chr $ 65 + mod (ord a - ord b) 26
                  | otherwise = ' '
-------------------------------------------------END VIGENERE--------------------------------------------
------------------------------------------------- BEGIN MAIN --------------------------------------------
guide :: IO()
guide = do
    putStrLn "\n"
    putStrLn "\n"
    putStrLn "                                CRIPTOGRAFIA                                    "

    putStrLn "---------------- 010000100101 -------------------- 010010010111 ----------------"
    putStrLn " 010010010111 ----------------------------------------------------  010010010111"
    putStrLn "--------------------------------- 010010010111 ---------------------------------"
    putStrLn " 010010010111 ------------------------------------------------------ 11111110001"
    putStrLn "---------------- 100110011001 -------------------- 1111001110 ----------------\n"

    putStrLn "                          Para utilizar a Cifra de Cesar:                      "
    putStrLn "               Codificar   : cesarEncode <deslocamento> <\"texto\">"
    putStrLn "               Decodificar : cesarDecode <deslocamento> <\"texto\">\n"

    putStrLn "                          Para utilizar a Cifra de Vigenere:                    "
    putStrLn "               Codificar   : vigenereEncode <deslocamento> <\"texto\">"
    putStrLn "               Decodificar : vigenereDecode <\"texto\"> <\"chave\">\n"

    return ()
------------------------------------------------- END MAIN --------------------------------------------