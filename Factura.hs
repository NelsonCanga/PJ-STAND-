
module Factura where

 type CodFactura = Int
 type Qtd = [Float]
 type Preco_Prod = [Float]
 type Pago = Float 
 type Troco = Float 
 type Nome = String
 type CodProd = [Int]
 type Numero = Int
 type Data = String
 type Total = Float 
 type Vendedor = String 
 
 type Factura = (CodFactura,Vendedor,Nome,Numero,CodProd,Qtd,Preco_Prod,Pago,Total,Troco,Data)

 type Facturas = [Factura]
