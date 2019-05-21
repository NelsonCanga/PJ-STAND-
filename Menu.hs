module Menu where 
import System.IO
import Data.Char
import Data.Time 
import Factura
import Automovel
import FicheirosFactura


carregar :: IO()
carregar = do
            file <- openFile  "Ficheiros /factura_cliente.txt" ReadMode 
            dados <- hGetContents file
            let factura = read (dados) :: Facturas 
            menuCompra (factura) 
            hClose file

--Menu para a funcão compra --

menuCompra :: Facturas->IO()
menuCompra facturas = do 
        let codfactura = (length facturas )
        putStrLn "Informe o nome : "
        nome <-getLine 
        putStrLn "Informe o numero de Telefone: "
        phone <-readLn :: IO Int 
        putStrLn "Informe o codigo do Automovel: "
        cod <- readLn :: IO Int 
        let valor = findPrice cod automoveis
        putStrLn ("Preço: "++show (valor)++" AKZ")
        putStrLn "Informe a quantidade: "
        qtd <- readLn :: IO Float
        let  total = ((findPrice cod automoveis)*qtd)
        putStrLn ("Preço a pagar : "++show (total)++" AKZ")
        valorPago <- readLn :: IO Float
        let troco = (valorPago - total)
        if(valorPago >=  total) then do {
        variavel <-getCurrentTime; putStrLn ("Troco: "++show(troco)++"    Data Compra :"++(take 19 (show(variavel)))++"  Codigo Factura:"++show(codfactura));
        inserir (facturas++facturaCliente (nome,phone,cod,qtd,valor,valorPago,troco,(take 19(show(variavel)))));
                menuPrincipal 
        }
               else do { putStrLn " Valor Insuficiente"; menuPrincipal}          

--Menu para a função ver preço--
menuVerpreco :: IO()
menuVerpreco = do  
        putStrLn "=================Ver-Preçario==============="
        putStrLn "Informe o codigo do Automovel: "
        codigo <- readLn :: IO Int 
        let valor = findPrice codigo automoveis;
        putStrLn ("Preço: "++show (valor)++" AKZ")


menuPrincipal ::IO()
menuPrincipal  = do 
        
    putStrLn ("=====================================================================")
    putStrLn ("|                1-Ver Precario                                      |") 
    putStrLn ("|                2-Listar Automoveis                                 |")
    putStrLn ("|                3-Efectuar Compra                                   |")
    putStrLn ("|                4-Consultar Factura                                 |")
    putStrLn ("|                5-Ver Factura Diário                                |")
    putStrLn ("|                6-Ver Todas Facturas                                |")
    putStrLn ("|                7-Mostra as informacoes do Automovel mais vendido   |")
    putStrLn ("|                8-Sair                                              |")
    putStr "       Digite a opção desejada: "
    opcao <- readLn
    putStrLn ""
    
    case opcao of
         1 -> do
                menuVerpreco
                putStrLn "1-Para voltar ao Menu Principal \n0-Para ver o preço novamente: "
                op <- readLn :: IO Int 
                if op == 1 then menuPrincipal else menuVerpreco 
         2 -> do putStrLn "============Listar Automoveis============" 
                 listaAutomoveis 
                 putStrLn "Precione 1 para voltar ao menuPrincipal:  "
                 op <- readLn :: IO Int 
                 if op == 1 then menuPrincipal else menuPrincipal                      
         3 -> do putStrLn "===========Efectuar Compra=========== "
                 --menuCompra factuas
                 carregar 
                 putStrLn "1-Para voltar ao Menu Principal \n1-Para efectuar uma outra compra:  "
                 opcao <- readLn :: IO Int 
                 if(opcao == 1 ) then menuPrincipal else putStrLn "" --menuCompra
        
         4 -> do putStrLn "Consultar Factura"
         5 -> do putStrLn "Ver Factura Diário"
         6 -> do putStrLn "Ver Todas Facturas"
         7 -> do putStrLn "Mostra as informacoes do Automovel mais vendido"
         8 -> do putStrLn "Terminando o Programa ...Volte Sempre "
         _ -> do{ putStrLn "Opcão Invalida "; menuPrincipal }
             

         

