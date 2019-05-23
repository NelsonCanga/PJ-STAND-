
module Factura where

 type CodFactura = Int
 type Qtd = Float
 type Preco_Prod = Float 
 type Pago = Float 
 type Troco = Float 
 type Nome = String
 type CodProd = Int
 type Numero = Int
 type Data = String
 
 type Factura = (CodFactura,Nome,Numero,CodProd,Qtd,Preco_Prod,Pago,Troco,Data)

 type Facturas = [Factura]


