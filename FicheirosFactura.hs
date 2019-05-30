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
                    file1 <-openFile "Ficheiros /Factura_todas.txt" WriteMode
                    hPutStr file1 ((show(factura))) 
                    putStrLn ""
                    hClose file1 


showAllfactura :: Facturas -> IO()
showAllfactura [] =  putStrLn ""
showAllfactura ((a,k,b,c,d,e,f,g,h,z,i):xs)= do {putStrLn ("Codigo:"++show(a)++"\n\nVendedor"++show(k)++"\n\nNome:"++show(b)++"\n\nTelefone:"++show(c)++
"\n\nCod Produtos:"++show(d)++"\n\nQTD:"++show(e)++"\n\nPreço:"++show(f)++"\n\nTotal:"++show(h)++"\n\nValor-Pago:"++show(g)++"\n\nTroco: "++show(z)++"\n\nData:"++show(i));
putStrLn "-------------------------------------------------------------------------------";                                     
                                         showAllfactura xs } 
 -- Fazer a função para pegar o dia da factura --

 
--Pega uma Factura por data fazer com retorne todas as ocorrencias ---
facDay :: String->Facturas-> IO()
facDay  date [] = putStrLn "Nenhuma Factura deste Dia: "
facDay date ((a,k,b,c,d,e,f,g,h,z,i):corpo) = do 
                                        if  ((drop 9(take 11 (show(i)))) == date) then do {
                                            putStrLn ("Codigo:"++show(a)++"\n\nVendedor:"++show(k)++"\n\nNome:"++show(b)++"\n\nTelefone:"++show(c)++
                                                "\n\nCodigo  Produtos:"++show(d)++"\n\nQTD:"++show(e)++"\n\nPreço:"++show(f)++"\n\nTotal:"++show(h)++"\n\nValor-Pago:"++show(g)++"\n\nTroco: "++show(z)++"\n\nData:"++show(i));
                                            putStrLn "-------------------------------------------------------------------------------";   
                                        }  
                                        else  facDay date corpo 

--Pega uma factura pelo codigo--
searchFact :: Int -> Facturas -> IO() 
seachFact cod [] = putStrLn "Codigo de Factura  Não Encontrado: "
searchFact cod ((a,y,b,c,d,e,f,g,h,z,i):corpo) = do 
                                                if (cod == a ) then do {
                                                    putStrLn ("Codigo:"++show(a)++"\n\nVendedor "++show(y)++"\nNome:"++show(b)++"\n\nTelefone:"++show(c)++
                                                    "\n\nCodigo Produtos:"++show(d)++"\n\nQTD:"++show(e)++"\n\nPreço:"++show(f)++"\n\nTotal:"++show(h)++"\n\nValor-Pago:"++show(g)++"\n\nTroco: "++show(z)++"\n\nData:"++show(i));
                                                }  
                                                else  searchFact cod  corpo



getListCod :: Facturas ->[Int]
getListCod [] = []
getListCod (cabeca@(_,_,_,_,cod@(x:xs),_,_,_,_,_,_):corpo) =cod++getListCod corpo


getMax :: [Int]->[Int]
getMax (x:[]) = [x]
getMax list@(x:xs) =((filter  (x==) xs ) ++ getMax xs)
