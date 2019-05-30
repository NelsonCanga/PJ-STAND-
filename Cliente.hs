module Cliente where 
import System.IO 

type Nome = String 
type Numero = Int 

type Cliente = (Nome,Numero)




salvarCliente :: Cliente -> IO() 
salvarCliente cliente = do 
            
                            file <- openFile "Ficheiros /factura_cliente.txt" AppendMode 
                            hPutStr file (show(cliente))
                            hClose file 
                            putStrLn ""
