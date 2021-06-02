-- Prática 04 de Haskell
-- Nome: Luiz Felipe Cavalheiro

faixaIdoso :: Int -> String
faixaIdoso idade
    | idade >= 60 && idade < 65 = "IDO64"
    | idade >= 65 && idade < 70 = "IDO69"
    | idade >= 70 && idade < 75 = "IDO74"
    | idade >= 75 && idade < 80 = "IDO79"
    | idade >= 80 = "IDO80"
    | otherwise = "ND"


classifIdosos :: [(String,Int)] -> [(String,Int,String)]
classifIdosos list = [(nome,idade,faixaIdoso idade) | (nome,idade) <- list]


classifIdosos' :: [(String,Int)] -> [(String,Int,String)]
classifIdosos' list = map (\(nome,idade) -> (nome,idade,faixaIdoso idade)) list


strColor :: (Int,Int,Int) -> String
strColor t = "rgb" ++ show t ++ ""


genCircs :: Int -> (Int,Int) -> Int -> [(Int,Int,Int)]
genCircs n (x,y) r = take n [(x,y,r) | x <- [fst (x,y), (fst (x,y) + 2*r)..]]
 
    
genReds :: Int -> [(Int,Int,Int)]
genReds n = take n [(red,0,0) | red <- [n*11, n*11+15..]]



