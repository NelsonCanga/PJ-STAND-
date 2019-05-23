module FicheirosFactura  where 
import System.IO 
import Factura
import Automovel 


--funcao para inserir uma factura na lista de facturas --

facturaCliente  ::Factura->Facturas 
facturaCliente factura = [factura]

--funcão que inseri a lista de facturas no ficheiro--
inserir :: Facturas -> IO()
inserir factura = do 
                    file <- openFile "Ficheiros /factura_cliente.txt" WriteMode 
                    hPutStr file (show(factura))
                    putStrLn ""
                    hClose file 


showAllfactura :: Facturas -> IO()
showAllfactura [] =  putStrLn ""
showAllfactura ((a,b,c,d,e,f,g,h,i):xs)= do   
                                         putStrLn (show(a)++"     "++show(b)++"               "++show(c)++"          "++show(d)++"       "++show(e)++" "++show(f)++" "++show(g)++" "++show(h)++" "++show(i))
                                         showAllfactura xs 
 -- Fazer a função para pegar o dia da factura --

 
--Pega uma Factura por data fazer com retorne todas as ocorrencias ---
facDay :: String->Facturas-> IO()
facDay  date [] = putStrLn "Nenhuma Factura deste Dia: "
facDay date (cabeca@(_,_,_,_,_,_,_,_,x):corpo) = do 
                                        if  ((drop 9(take 11 (show(x)))) == date) then  putStrLn(show(cabeca))
                                        else  facDay date corpo 

--Pega uma factura pelo codigo--
searchFact :: Int -> Facturas -> IO() 
seachFact cod [] = putStrLn "Codigo de Factura  Não Encontrado: "
searchFact cod (cabeca@(x,_,_,_,_,_,_,_,_):corpo) = do 
                                                if (cod == x ) then  putStrLn (show(cabeca))
                                                else  searchFact cod  corpo
