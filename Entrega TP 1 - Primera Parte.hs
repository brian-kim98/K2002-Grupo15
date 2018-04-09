{-# LANGUAGE NoMonomorphismRestriction #-}
import Text.Show.Functions
import Data.List
import Data.Maybe
import Test.Hspec



-- Definimos el tipo Evento --
type Evento = Float -> Float

-- Ahora integramos a las funciones que debemos crear --
deposito ::Float -> Evento
deposito cantidadADepositar = (+ cantidadADepositar)

extraccion :: Float -> Evento
extraccion cantidadAExtraer carteraOnline | carteraOnline >= cantidadAExtraer = carteraOnline - cantidadAExtraer
                             | otherwise = 0

upgrade :: Evento
-- Creamos una funcion llamada "Bonus"
bonus = (* 0.2)


upgrade carteraOnline | ((<= 10). bonus) carteraOnline = carteraOnline + bonus carteraOnline
                  | otherwise = carteraOnline + 10

cierreDeCuenta :: Evento
cierreDeCuenta = (*0)

quedaIgual :: Evento
quedaIgual = (* 1)

-- FUNCIONA TODO HASTA ACA --

-- Definimos la data de un usuario --
data Billetera = Usuario {
nombre :: String,
cantidad :: Float
} deriving (Show, Eq)

-- Creamos el pepe y el lucho que nos piden --
pepe = Usuario {
nombre = "Jose",
cantidad = 10
}

lucho = Usuario {
nombre = "Luciano",
cantidad = 2
}

-- Se prueba "cantidad pepe" y anda --
-- Se prueba "cierreDeCuenta (cantidad pepe)" y anda --
-- Se prueba "(upgrade . (extraccion 2) . (deposito 15)) (cantidad pepe)" y anda --

-- TRANSACCIONES --
luchoCierraLaCuenta :: Transaccion
luchoCierraLaCuenta unUsuario | nombre unUsuario == "Luciano" = cierreDeCuenta
                              | otherwise = quedaIgual

pepeDeposita5Monedas :: Transaccion
pepeDeposita5Monedas unUsuario | nombre unUsuario == "Jose" = deposito 5
                               | otherwise = quedaIgual

-- Probamos en la consola --
-- (luchoCierraLaCuenta pepe) 20 anda --
-- (pepeDeposita5Monedas pepe) 10 anda --

-- Creamos el pepe 2 --
pepe2 = Usuario {
nombre = "Jose",
cantidad = 20
}

-- Probamos lo que nos pide --
-- (pepeDeposita5Monedas pepe2) 50 anda --
-- Definimos el tipo Transaccion --

type Transaccion = Billetera -> Evento

-- Se aplica arriba de las funciones --



-- NUEVOS EVENTOS --

tocoYMeVoy :: Evento
tocoYMeVoy = (cierreDeCuenta . upgrade . (deposito 15))

--Probamos con " >tocoYMeVoy 10" y funciona --
--Probamos con " >tocoYMeVoy (cantidad pepe)" y funciona --


ahorranteErrante = ((deposito 10) . upgrade . (deposito 8) . (extraccion 1) . (deposito 2) . (deposito 1))
ahorranteErrante :: Evento

--Probamos con ">ahorranteErrante 10" y funciona --
--Probamos con ">ahorranteErrante (cantidad lucho)" y funciona --


luchoTocaYSeVa :: Billetera -> Float
luchoTocaYSeVa unUsuario |nombre unUsuario == "Luciano" = tocoYMeVoy (cantidad unUsuario)
                         |otherwise = quedaIgual (cantidad unUsuario)

--Probamos con " >luchoTocaYSeVa lucho" y funciona--


luchoEsAhorranteErrante :: Billetera -> Float
luchoEsAhorranteErrante unUsuario |nombre unUsuario == "Luciano" = ahorranteErrante (cantidad unUsuario)
                                  |otherwise = quedaIgual (cantidad unUsuario)

--Probamos con " >luchoEsAhorranteErrante lucho" anda --

pepeLeDa7UnidadesALucho :: Transaccion

pepeLeDa7UnidadesALucho unUsuario | nombre unUsuario == "Jose" = extraccion 7
                                  | nombre unUsuario == "Luciano" = deposito 7
                                  | otherwise = quedaIgual

--Probamos " > pepeLeDa7UnidadesALucho pepe 10"  y funciona --
--Probamos " > pepeLeDa7UnidadesALucho lucho 10"  y funciona --

-- Vamos con los Tests ahora :D --
ejecutarTests = hspec $ do
 it "Depositar 10 en una billetera de 10 monedas = 20" (deposito 10 10 `shouldBe` 20)
 it "Extraer 3 de una billetera de 10 monedas = 7" (extraccion 3 10 `shouldBe` 7)
 it "Extraer 15 de una billetera de 10 monedas = 0" (extraccion 15 10 `shouldBe` 0)
 it "Upgrade a una billetera de 10 monedas = 12" (upgrade 10 `shouldBe` 12)
 it "Cerrar la cuenta a una billetera de 10 monedas = 0" (cierreDeCuenta 10 `shouldBe` 0)
 it "Que quede igual la billetera de 10 monedas = 10" (quedaIgual 10 `shouldBe` 10)
 it "Depositar 1000 y realizar un upgrade a una billetera de 10 monedas = 1020" ((upgrade . (deposito 1000)) 10 `shouldBe` 1020)
 it "La billetera de Pepe debería ser de 10 monedas" (cantidad pepe `shouldBe` 10)
 it "El cierre de cuenta de la billetera de Pepe quedaría en 0 monedas" (cierreDeCuenta (cantidad pepe) `shouldBe` 0)
 it "La billetera de Pepe, luego del deposito de 15 monedas, extraer 2 y tener un upgrade, tiene 27,6 monedas" ((upgrade . (extraccion 2) . (deposito 15)) (cantidad pepe) `shouldBe` 27.6)
