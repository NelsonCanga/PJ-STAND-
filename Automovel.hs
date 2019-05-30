--Modulo Automovel --
module Automovel where 
import System.IO 



type Nome = String 
type Codigo = Int 
type Preco = Float


data Categoria = Amador | Profissional | Pesado deriving (Show, Eq)
data Marca = Volvo | Toyota | Jaguar | BMW | Honda deriving (Show , Eq)
data Tipo = Carro | Mota deriving (Show , Eq)

type Automovel = (Nome, Codigo , Preco , Categoria , Marca , Tipo )



mota1:: Automovel
mota1 =("JOG",1,120000.0,Amador,Honda,Mota) 
mota2:: Automovel
mota2 =("JOG",2,120000.0,Amador,Honda,Mota) 
mota3:: Automovel
mota3 =("JOG",3,120000.0,Amador,Honda,Mota) 
carro1:: Automovel 
carro1 = ("I10",4,150.0,Amador,BMW,Carro)
carro2:: Automovel 
carro2 = ("I10",5,200.0,Amador,BMW,Carro)


automoveis :: [Automovel] 
automoveis =[mota1,mota2,mota3,carro1,carro2]

getPrice :: Automovel -> Float 
getPrice (_,_,xs,_,_,_) = xs  

getCodig :: Automovel -> Int 
getCodig (_,x,xs,_,_,_) =x 

findAutomovel :: Int -> Int 
findAutomovel n | n == getCodig carro1 = getCodig carro1
                | otherwise = error "Codigo do Automovel Não existe"

findPrice :: Int -> [(Automovel)]-> Float 
findPrice cod [] = error " Nenhum Automovel encontrado "
findPrice  cod (cabeca: corpo ) =  if  getCodig cabeca == cod
                                        then  getPrice cabeca
                                            else findPrice cod corpo

findprice :: [Int] -> [Automovel]->[Float]
findprice  [] lista = []
findprice (x:xs) (cabeca:corpo) | getCodig cabeca == x = precos 
                                | otherwise = findprice xs corpo 
                                 where precos = [getPrice cabeca] 


listaAutomoveis ::[Automovel]->IO() 
listaAutomoveis [] = putStrLn" " 
listaAutomoveis ((a,b,c,d,e,f):xs) =do 
                                putStrLn (show(a)++"     "++show(b)++"       "++show(c)++"   "++show(d)++"       "++show(e)++"   "++show(f))
                                listaAutomoveis xs 


listItems::[Int]->[Float]->[Automovel]->Float
listItems [] [] artigos = 0.0 
listItems  (x:xs) (a:b) artigos | artigos /= [] = total 
                          | otherwise = listItems xs b artigos 
                          where total = sum [(findPrice x artigos)*a]+ listItems  xs b artigos
                          
allprice :: [Int] ->[Automovel] -> [Float]
allprice [] artigos = []
allprice (x:xs) artigos | artigos /= [] = precos 
                        | otherwise = allprice xs artigos 
                         where precos = [findPrice x artigos ] ++ allprice xs artigos

showAtributos :: Int -> [Automovel]->IO()               
showAtributos cod (cabeca@(a,b,c,d,e,f):corpo) = do if (cod == b ) then do {  putStrLn  ("\nNome:"++show(a)++"\n\nCodigo:"++show(b)++"\n\nPreço:"++show(c)++"\n\nCategoria:"++show(d)++"\n\nMarca: "++show(e)++"\n\nTipo:"++show(f)); }
                                                        else showAtributos cod corpo 