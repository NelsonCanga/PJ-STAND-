module Ficheiros  where 
import System.IO 
import Factura 
import Data.Time 
import Automovel 


getName :: Factura ->Int
getName dados@(x,xs,_,_,_,_,_,_)=xs

facturaCliente ::[Factura]->[Factura] 
facturaCliente factura = [factura]

inserir :: [Factura] -> IO()
inserir factura = do 
                    file <- openFile "Ficheiros /factura_cliente.txt" AppendMode
                    hPutStr file (show(factura)++"\n")
                    putStrLn ""
                    hClose file 

menuCompra :: [Factura]->IO()
menuCompra factura = do 
        putStrLn (show(facturaCliente factura)) 
        

carregar = do
            file <- openFile  "Ficheiros /factura_cliente.txt" ReadMode 
            dados <- hGetContents file 
            let factura = read (dados) :: [Factura]      
            menuCompra (factura)  
            hClose file


