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
