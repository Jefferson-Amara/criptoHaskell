import Data.IORef
import Data.List
import Data.Char(toUpper)
import Foreign.Marshal.Unsafe
import Data.Array.IO
import Control.Monad
import Data.Char
)

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

encodeC :: Int -> String -> String
encodeC n xs = [shift n x | x <- xs]

decodeC :: Int -> String -> String
decodeC n = encodeC (-n)

callCesar :: Int -> String -> Int -> String
callCesar op text desloc = encodeC desloc text
------------------------------------------------END CESAR-----------------------------------------------
----------------------------------------------BEGIN VIGENERE--------------------------------------------
encodeV :: [Char] -> [Char] -> [Char]
encodeV x [] = []
encodeV [] y = []    
encodeV (x:xs) (y:ys) = (vigEncode x y):(encodeV xs (ys++[y])) where
    vigEncode a b -- | isLower a && isLower b = chr $ 97 + mod (ord a + ord b) 97
                  | isUpper a && isUpper b = chr $ 65 + mod (ord a + ord b) 26


decodeV :: [Char] -> [Char] -> [Char]
decodeV x [] = []
decodeV [] y = []    
decodeV (x:xs) (y:ys) = (vigDecode x y):(decodeV xs (ys++[y]))where
    vigDecode a b -- | isLower a && isLower b = chr $ 97 + mod (ord a - ord b) 97
                  | isUpper a && isUpper b = chr $ 65 + mod (ord a - ord b) 26
-------------------------------------------------END VIGENERE--------------------------------------------

main :: IO()
main = do

    putStrLn "CRIPTOGRAFIA"
    putStr "\ESC[2J"--  limpa a tela (dando espaço)

    putStrLn "---------------- 010000100101 -------------------- 010010010111 ----------------"
    putStrLn " 010010010111 ----------------------------------------------------  010010010111"
    putStrLn "--------------------------------- 010010010111 ---------------------------------"
    putStrLn " 010010010111 ------------------------------------------------------ 11111110001"
    putStrLn "---------------- 100110011001 -------------------- 1111001110 ----------------\n"

    putStrLn " METODO 1 - CIFRA DE CESAR"
    putStrLn " METODO 2 - CIFRA DE VIGENERE\n"

    m <- getString "Informe numero do metodo que deseja utilizar: "
    let metd = read m::Int

    putStrLn " OPERACAO 1 - CODIFICAR"
    putStrLn " OPERACAO 2 - DECODIFICAR\n"

    n <- getString "Informe o numero da operacao a realizar: "
    let op = read n::Int

    t <- getString "Digite o texto: "

    if(metd==1 && op==1) then
        callCesar op t 3    
    else 
        callCesar op t 3

    return ()