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

encodeC :: Int -> String -> String
encodeC n xs = [shift n x | x <- xs]

decodeC :: Int -> String -> String
decodeC n = encodeC (-n)
------------------------------------------------END CESAR-----------------------------------------------
----------------------------------------------BEGIN VIGENERE--------------------------------------------
encodeV :: [Char] -> [Char] -> [Char]
encodeV x [] = x
encodeV [] y = []    
encodeV (x:xs) (y:ys) = (vigEncode x y):(encodeV xs (ys++[y])) where
    vigEncode a b | isLower a && isLower b = chr $ 97 + mod (66 + ord a + ord b) 26
                  | isUpper a && isUpper b = chr $ 65 + mod (ord a + ord b) 26
                  | otherwise = ' '


decodeV :: [Char] -> [Char] -> [Char]
decodeV x [] = x
decodeV [] y = []    
decodeV (x:xs) (y:ys) = (vigDecode x y):(decodeV xs (ys++[y])) where
    vigDecode a b | isLower a && isLower b = chr $ 97 + mod (ord a - ord b) 26
                  | isUpper a && isUpper b = chr $ 65 + mod (ord a - ord b) 26
                  | otherwise = ' '
-------------------------------------------------END VIGENERE--------------------------------------------
-----------------------------------------------BEGIN POLYBIUM--------------------------------------------
{-import Data.Matrix( Matrix, fromLists, getCol, getRow )
import Data.Vector( Vector, elem )
import Data.List.Split( chunksOf)

cifra = fromLists[  ["n","1","h","d","z","3"],
                    ["4","c","t","5","i","y"],
                    ["7","e","6","2","q","9"],
                    ["8","k","u","a","o","f"],
                    ["r","m","0","s","v","w"],
                    ["p","l","j","x","b","g"]]



getFromPolibio::[Char]->[Char]
getFromPolibio [] = []
getFromPolibio (x:xs) = [row,col] ++ getFromPolibio (xs)
    where 
        row = (getRowPol [x])
        col = (getColPol [x])

getRowPol::[Char]->Char
getRowPol a 
    | getRowAux a 1 = 'a'
    | getRowAux a 2 = 'd'
    | getRowAux a 3 = 'f'
    | getRowAux a 4 = 'g'
    | getRowAux a 5 = 'v'
    | getRowAux a 6 = 'x'
    | otherwise = ' ' 
    where getRowAux x n = Data.Vector.elem x (getRow n cifra)

getColPol::[Char]->Char
getColPol a 
    | getColAux a 1 = 'a'
    | getColAux a 2 = 'd'
    | getColAux a 3 = 'f'
    | getColAux a 4 = 'g'
    | getColAux a 5 = 'v'
    | getColAux a 6 = 'x'
    | otherwise = ' '
    where getColAux x n = Data.Vector.elem x (getCol n cifra)


matrixEnc::[Char]->[Char]-> Matrix Char
matrixEnc []  = fromLists []
matrixEnc  [] = fromLists []
matrixEnc frase chave = fromLists mat
    where
        mat = chunksOf tamanhoPalavra crip
        tamanhoPalavra = length chave
        crip = getFromPolibio frase

toMap :: [Char] -> [Char] -> [(Char,Char)]
toMap x [] = []
toMap [] y = []
toMap (x:xs) (y:ys) = [(x,y)]++(toMap xs ys)


enc::[Char]->[Char]->[(Char, (Vector Char) )]
enc []  = []
enc  [] = []
enc palavra chave  = [ ( chave !! (x - 1), getCol x m ) | x <- [1..tamanhoPalavra] ]
    where
        m = matrixEnc palavra chave
        tamanhoPalavra = length chave-}
------------------------------------------------ END POLYBIUM -------------------------------------------
------------------------------------------------- BEGIN MAIN --------------------------------------------
{-main :: IO()
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

    m <- getLine "Informe numero do metodo que deseja utilizar: "
    let metd = read m::Int


    putStrLn " OPERACAO 1 - CODIFICAR"
    putStrLn " OPERACAO 2 - DECODIFICAR\n"

    o <- getLine "Informe o numero da operacao a realizar: "
    let op = read o::Int


    t <- getLine "Informe o texto: "
    let text = read t::String

    if(metd==1) then
        d <- getLine "Informe o valor do deslocamento"  
    else 
        key <- getLine "Informe o texto chave"

    if(metd==1 && op==1)then
        e = encodeC d::Int text
    else if(metd==1 && op==2)then
        e = decodeC d::Int text
    else if(metd==2 && op==1)then
        e = encodeV text key
    else if(metd==2 && op==2)then
        e = decodeV text key

    return ()-}
------------------------------------------------- END MAIN --------------------------------------------