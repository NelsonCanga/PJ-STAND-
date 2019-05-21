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


automoveis :: [Automovel] 
automoveis =[mota1,mota2,mota3,carro1]

getPrice :: Automovel -> Float 
getPrice (_,_,xs,_,_,_) = xs  

getCodig :: Automovel -> Int 
getCodig (_,x,xs,_,_,_) =x 

findAutomovel :: Int -> Int 
findAutomovel n | n == getCodig carro1 = getCodig carro1
                | otherwise = error "Codigo do Automovel Não existe"

findPrice :: Int -> [(Automovel)]-> Float 
findPrice  cod (cabeca: corpo ) =  if  getCodig cabeca == cod
                                        then  getPrice cabeca
                                            else findPrice cod corpo                                                                    