{-# LANGUAGE NoMonomorphismRestriction #-}
import Text.Show.Functions
import Data.List
import Data.Maybe
import Test.Hspec


-- Definimos el tipo Evento --
type Evento = Float -> Float

deposito ::Float -> Evento
deposito cantidadADepositar = (+ cantidadADepositar)


extraccion :: Float -> Evento
extraccion cantidadAExtraer carteraOnline | carteraOnline >= cantidadAExtraer = carteraOnline - cantidadAExtraer
                             | otherwise = 0

upgrade :: Evento
-- Creamos una funcion llamada "Bonus" --
bonus = (* 0.2)


upgrade carteraOnline | ((<= 10). bonus) carteraOnline = carteraOnline + bonus carteraOnline
                  | otherwise = carteraOnline + 10

cierreDeCuenta :: Evento
cierreDeCuenta = (*0)

quedaIgual :: Evento
quedaIgual = (* 1)


-- Definimos la data de un usuario --
data Usuario = Usuario {
nombre :: String,
billetera :: Float
} deriving (Show, Eq)

-- Creamos el pepe y el lucho que nos piden --
pepe = Usuario {
nombre = "Jose",
billetera = 10
}

lucho = Usuario {
nombre = "Luciano",
billetera = 2
}


-- TRANSACCIONES --

-- Definimos el tipo Transaccion --
type Transaccion = Usuario -> Evento

luchoCierraLaCuenta :: Transaccion
luchoCierraLaCuenta unUsuario | nombre unUsuario == "Luciano" = cierreDeCuenta
                              | otherwise = quedaIgual

pepeDeposita5Monedas :: Transaccion
pepeDeposita5Monedas unUsuario | nombre unUsuario == "Jose" = deposito 5
                               | otherwise = quedaIgual


-- Creamos el pepe 2 --
pepe2 = Usuario {
nombre = "Jose",
billetera = 20
}

-- NUEVOS EVENTOS --

tocoYMeVoy :: Evento
tocoYMeVoy = (cierreDeCuenta . upgrade . (deposito 15))


ahorranteErrante :: Evento
ahorranteErrante = ((deposito 10) . upgrade . (deposito 8) . (extraccion 1) . (deposito 2) . (deposito 1))


luchoTocaYSeVa :: Usuario -> Float
luchoTocaYSeVa unUsuario |nombre unUsuario == "Luciano" = tocoYMeVoy (billetera unUsuario)
                         |otherwise = quedaIgual (billetera unUsuario)


luchoEsAhorranteErrante :: Usuario -> Float
luchoEsAhorranteErrante unUsuario |nombre unUsuario == "Luciano" = ahorranteErrante (billetera unUsuario)
                                  |otherwise = quedaIgual (billetera unUsuario)


pepeLeDa7UnidadesALucho :: Transaccion
pepeLeDa7UnidadesALucho unUsuario | nombre unUsuario == "Jose" = extraccion 7
                                  | nombre unUsuario == "Luciano" = deposito 7
                                  | otherwise = quedaIgual


-- Vamos con los Tests ahora :D --
ejecutarTests = hspec $ do
 describe "Operaciones de Eventos " $ do
   it "Depositar 10 en una billetera de 10 monedas = 20" $ deposito 10 10 `shouldBe` 20
   it "Extraer 3 de una billetera de 10 monedas = 7" $ extraccion 3 10 `shouldBe` 7
   it "Extraer 15 de una billetera de 10 monedas = 0" $ extraccion 15 10 `shouldBe` 0
   it "Upgrade a una billetera de 10 monedas = 12" $ upgrade 10 `shouldBe` 12
   it "Cerrar la cuenta a una billetera de 10 monedas = 0" $ cierreDeCuenta 10 `shouldBe` 0
   it "Que quede igual la billetera de 10 monedas = 10" $ quedaIgual 10 `shouldBe` 10
   it "Depositar 1000 y realizar un upgrade a una billetera de 10 monedas = 1020" $ (upgrade . (deposito 1000)) 10 `shouldBe` 1020
 describe "Operaciones de Usuarios " $ do
   it "La billetera de Pepe debería ser de 10 monedas" $ billetera pepe `shouldBe` 10
   it "El cierre de cuenta de la billetera de Pepe quedaría en 0 monedas" $ cierreDeCuenta (billetera pepe) `shouldBe` 0
   it "La billetera de Pepe, luego del deposito de 15 monedas, extraer 2 y tener un upgrade, tiene 27,6 monedas" $ (upgrade . (extraccion 2) . (deposito 15)) (billetera pepe) `shouldBe` 27.6
 describe "Operaciones de Transacciones " $ do
   it "Aplicamos la transaccion lucho cierra su cuenta, a pepe y la funcion que nos da lo aplicamos a una billetera de 20 monedas, esto da 20 ya que queda igual " $ (luchoCierraLaCuenta pepe) 20 `shouldBe` 20
   it "La transacción 'pepe deposita 5 monedas' se aplica a Pepe. El resultad lo aplicamos a una billetera de 10 monedas, deberia quedar con 15" $ (pepeDeposita5Monedas pepe) 10 `shouldBe` 15
   it "La transaccion 'pepe deposita 5 monedas' se aplica al Pepe2. El evento resultante se aplica a una billetera de 50 monedas, y debería quedar con 55." $ (pepeDeposita5Monedas pepe2) 50 `shouldBe` 55
 describe "Operaciones de Nuevos Eventos " $ do
   it "Un usuario con 10 monedas toca y se va, terminaria con 0 monedas" $ (tocoYMeVoy 10) `shouldBe` 0
   it "Un usuario con 10 monedas es ahorrante errante, deberia terminar con 34 monedas " $ (ahorranteErrante 10) `shouldBe` 34
 describe "Transacciones mas complejas " $ do
   it "Aplicamos la transaccion 'pepe le da 7 unidades a lucho' con Pepe, que cuenta con una billetera de 10 monedas, y termina con 3 monedas " $ (pepeLeDa7UnidadesALucho pepe 10)`shouldBe` 3
   it "Aplicamos la transaccion 'pepe le da 7 unidades a lucho' con Lucho, que cuenta con una billetera de 10 monedas, termina con 17 monedas " $ (pepeLeDa7UnidadesALucho lucho 10) `shouldBe` 17
