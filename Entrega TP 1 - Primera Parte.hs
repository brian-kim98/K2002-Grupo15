{-# LANGUAGE NoMonomorphismRestriction #-}
import Text.Show.Functions
import Data.List
import Data.Maybe
import Test.Hspec

type Dinero = Float

type Billetera = Dinero

type Evento = Billetera -> Billetera

type Nombre = String

type Transaccion = Usuario -> Evento -> Usuario -> Evento

type Transferencias = Usuario -> Usuario -> Dinero -> Usuario -> Evento

data Usuario = Usuario {
nombre :: Nombre,
billetera :: Dinero
} deriving (Show, Eq)


deposito :: Dinero -> Evento
deposito cantidadADepositar carteraOnline = (+ carteraOnline) (max 0.0 cantidadADepositar)


extraccion :: Dinero -> Evento
extraccion cantidadAExtraer carteraOnline | (max 0 cantidadAExtraer) <= carteraOnline = carteraOnline - (max 0 cantidadAExtraer)
                                          | otherwise = 0

upgrade :: Evento
bonus = (* 0.2)
upgrade carteraOnline = min 10 (bonus carteraOnline) + carteraOnline

cierreDeCuenta :: Evento
cierreDeCuenta = (*0)

quedaIgual :: Evento
quedaIgual = id

tocoYMeVoy :: Evento
tocoYMeVoy = (cierreDeCuenta . upgrade . (deposito 15))

ahorranteErrante :: Evento
ahorranteErrante = ((deposito 10) . upgrade . (deposito 8) . (extraccion 1) . (deposito 2) . (deposito 1))

testEventos = hspec $ do
 describe "Operaciones de Eventos " $ do
   it "Depositar 10 en una billetera de 10 monedas = 20 monedas" $ deposito 10 10 `shouldBe` 20
   it "Extraer 3 de una billetera de 10 monedas = 7 monedas" $ extraccion 3 10 `shouldBe` 7
   it "Extraer 15 de una billetera de 10 monedas = 0 monedas" $ extraccion 15 10 `shouldBe` 0
   it "Upgrade a una billetera de 10 monedas = 12 monedas" $ upgrade 10 `shouldBe` 12
   it "Cerrar la cuenta a una billetera de 10 monedas = 0 monedas" $ cierreDeCuenta 10 `shouldBe` 0
   it "Que quede igual la billetera de 10 monedas = 10 monedas" $ quedaIgual 10 `shouldBe` 10
   it "Depositar 1000 y realizar un upgrade a una billetera de 10 monedas = 1020 monedas" $ (upgrade . (deposito 1000)) 10 `shouldBe` 1020


pepe = Usuario {
nombre = "Jose",
billetera = 10
}

lucho = Usuario {
nombre = "Luciano",
billetera = 2
}


testUsuarios = hspec $ do
 describe "Operaciones de Usuarios " $ do
   it "La billetera de Pepe debería ser de 10 monedas" $ billetera pepe `shouldBe` 10
   it "El cierre de cuenta de la billetera de Pepe quedaría en 0 monedas" $ cierreDeCuenta (billetera pepe) `shouldBe` 0
   it "La billetera de Pepe, luego del deposito de 15 monedas, extraer 2 y tener un upgrade, tiene 27,6 monedas" $ (upgrade . (extraccion 2) . (deposito 15)) (billetera pepe) `shouldBe` 27.6


generadorTransacciones :: Transaccion
generadorTransacciones unaPersona unEvento personaAplicada | mismoUsuario unaPersona personaAplicada = unEvento
                                                              | otherwise = quedaIgual

mismoUsuario :: Usuario -> Usuario -> Bool
mismoUsuario persona1 persona2 = nombre persona1 == nombre persona2

pepe2 = Usuario {
nombre = "Jose",
billetera = 20
}


testTransacciones = hspec $ do
  describe "Operaciones de Transacciones " $ do
  it "Aplicamos la transaccion 'lucho cierra su cuenta' con pepe, y esto lo aplicamos a una billetera de 20 monedas = 20 monedas" $ (generadorTransacciones lucho cierreDeCuenta pepe 20) `shouldBe` 20
  it "Aplicamos la transacción 'pepe deposita 5 monedas' con pepe, y esto lo aplicamos a con una billetera de 10 monedas = 15 monedas" $ (generadorTransacciones pepe (deposito 5) pepe 10) `shouldBe` 15
  it "Aplicamos la transaccion 'pepe deposita 5 monedas' con pepe2, y esto lo aplicamos a una billetera de 50 monedas = 55 monedas" $ (generadorTransacciones pepe (deposito 5) pepe2 50)`shouldBe` 55
  it "Aplicamos la transaccion 'lucho toca y se va' al mismo usuario, y esto lo aplicamos a una billetera de 10 monedas = 0" $ (generadorTransacciones lucho tocoYMeVoy lucho 10) `shouldBe` 0
  it "Aplicamos la transaccion 'lucho ahorrante errante' al mismo usuario, y esto lo aplicamos a una billetera de 10 monedas = 34" $ (generadorTransacciones lucho ahorranteErrante lucho 10) `shouldBe` 34


generadorTransferencias :: Transferencias
generadorTransferencias usuarioDeudor usuarioAcreedor unaCantidad usuarioAplicado  | mismoUsuario usuarioDeudor usuarioAplicado = (extraccion unaCantidad)
                                                                                   | mismoUsuario usuarioAcreedor usuarioAplicado = (deposito unaCantidad)
                                                                                   | otherwise = quedaIgual


testTransferencias = hspec $ do
  describe "Transacciones mas complejas " $ do
  it "Aplicamos la transaccion 'pepe le da 7 unidades a lucho' con Pepe, esto lo aplicamos a una billetera de 10 monedas, y termina con 3 monedas " $ (generadorTransferencias pepe lucho 7 pepe 10)`shouldBe` 3
  it "Aplicamos la transaccion 'pepe le da 7 unidades a lucho' con Lucho, esto lo aplicamos a una billetera de 10 monedas, termina con 17 monedas " $ (generadorTransferencias pepe lucho 7 lucho 10) `shouldBe` 17

---------------------------------------------------------------------------------------------------------------------------------
  -- SEGUNDA PARTE --


testUsuarioTransaccion = hspec $ do
   describe "Usuario luego de transacción" $ do
     it "Se realiza la transacción 'Lucho cierra la cuenta' en Pepe directamente (Pepe cuenta con una billetera de 10 monedas) = 10 monedas" $ (generadorTransacciones lucho cierreDeCuenta pepe (billetera pepe)) `shouldBe` 10
     it "Aplicamos la transaccion 'pepe le da 7 unidades a lucho' a Lucho directamente (Lucho cuenta con una billetera de 2 monedas) = 9 monedas" $ (generadorTransferencias pepe lucho 7 lucho (billetera lucho)) `shouldBe` 9
     it "Aplicamos la transaccion 'pepe le da 7 unidades a lucho' y luego 'pepe deposita 5 monedas' a Pepe (Pepe cuenta con una billetera de 10 monedas) = 8" $ ((generadorTransacciones pepe (deposito 5) pepe) . (generadorTransferencias pepe lucho 7 pepe)) (billetera pepe) `shouldBe` 8


billeteraLuegoDeTransaccion :: Usuario -> Evento -> Usuario -> Usuario
billeteraLuegoDeTransaccion unaPersona unEvento personaAplicada = personaAplicada {
  billetera = (generadorTransacciones unaPersona unEvento personaAplicada) (billetera personaAplicada)
 }
billeteraLuegoDeTransferencia :: Usuario -> Usuario -> Dinero -> Usuario -> Usuario
billeteraLuegoDeTransferencia usuarioDeudor usuarioAcreedor unaCantidad usuarioAplicado = usuarioAplicado {
  billetera = generadorTransferencias usuarioDeudor usuarioAcreedor unaCantidad usuarioAplicado  (billetera usuarioAplicado)
 }

 --BLOQUE--
 -- Creamos las transacciones para que sea menos tedioso --
luchoCierraLaCuenta :: Usuario -> Evento
luchoCierraLaCuenta = generadorTransacciones lucho cierreDeCuenta

pepeDeposita5monedas :: Usuario -> Evento
pepeDeposita5monedas = generadorTransacciones pepe (deposito 5)

luchoTocaYSeVa :: Usuario -> Evento
luchoTocaYSeVa = generadorTransacciones lucho tocoYMeVoy

luchoAhorranteErrante :: Usuario -> Evento
luchoAhorranteErrante = generadorTransacciones lucho ahorranteErrante

pepeDa7ALucho :: Usuario -> Evento
pepeDa7ALucho = generadorTransferencias pepe lucho 7

testBloques = hspec $ do
   describe "Testeo de Bloques" $ do
     it "A partir del primer bloque, pepe deberia quedar con una billetera de 18 monedas " $ (foldr ($) (billetera pepe) . map ($ pepe)) bloque1 `shouldBe` 18
     it "Para el bloque 1, y los usuarios Pepe y Lucho, el único que quedaría con un saldo mayor a 10, es Pepe." $ filter ((>= 10) . billetera . primerBloque) [lucho, pepe] `shouldBe` [pepe]
     it "Determinar el mas adinerado con el primer bloque en una lista con lucho y pepe, deberia ser pepe. " $ elMasAdinerado primerBloque [lucho,pepe] `shouldBe` pepe
     it "Determinar el menos adinerado con el primer bloque en una lista con lucho y pepe, deberia ser lucho. " $ elMenosAdinerado primerBloque [lucho,pepe] `shouldBe` lucho

bloque1 = [luchoTocaYSeVa, pepeDa7ALucho, luchoAhorranteErrante, luchoTocaYSeVa, pepeDeposita5monedas, pepeDeposita5monedas, pepeDeposita5monedas, luchoCierraLaCuenta]

type Bloque = Usuario -> Usuario
primerBloque :: Bloque
primerBloque unUsuario = unUsuario {
  billetera = (foldr ($) (billetera unUsuario) . map ($ unUsuario)) bloque1
   }

saldoDeAlMenosNCreditos :: Dinero -> Bloque -> [Usuario] -> [Usuario]
saldoDeAlMenosNCreditos n bloque unaListaDeUsuarios = filter ((> n) . billetera . bloque) unaListaDeUsuarios


-- auxiliar --
elMasRicoEsElPrimero :: Usuario -> Usuario -> Bloque -> Bool
elMasRicoEsElPrimero usuario1 usuario2 unBloque = billetera (unBloque usuario1) >= billetera (unBloque usuario2)
-------------

elMasAdinerado :: Bloque -> [Usuario] -> Usuario
elMasAdinerado unBloque (cabezaUsuario : []) = cabezaUsuario
elMasAdinerado unBloque (cabezaUsuario : (cabezaColaUsuarios:colaColaUsuarios)) | elMasRicoEsElPrimero cabezaUsuario cabezaColaUsuarios unBloque = elMasAdinerado unBloque (cabezaUsuario : colaColaUsuarios)
                                                                                | otherwise = elMasAdinerado unBloque (cabezaColaUsuarios : colaColaUsuarios)

elMenosAdinerado :: Bloque -> [Usuario] -> Usuario
elMenosAdinerado unBloque (cabezaUsuario : []) = cabezaUsuario
elMenosAdinerado unBloque (cabezaUsuario : (cabezaColaUsuarios:colaColaUsuarios)) | elMasRicoEsElPrimero cabezaColaUsuarios cabezaUsuario unBloque = elMenosAdinerado unBloque (cabezaUsuario : colaColaUsuarios)
                                                                                  | otherwise = elMenosAdinerado unBloque (cabezaColaUsuarios : colaColaUsuarios)

 --BLOCKCHAIN--

testBlockChain = hspec $ do
      describe "Testeo de BlockChains " $ do
      it "El peor bloque con el cual podria empezar pepe de una cadena de 1 segundo bloque y 10 primer bloque, deberia ser cualquiera de los primer bloque, ya que empezaria con 18 monedas" $ billetera (elPeorBloque pepe (segundoBloque :(take 10 (repeat primerBloque))) pepe) `shouldBe` 18
      it "Aplicar un BlockChain compuesta del segundoBloque, seguido del primerBloque 10 veces a pepe, esto deberia dar una billetera de 115" $ foldr ($) pepe listaBlockChain `shouldBe` Usuario {nombre = "Jose", billetera = 115.0}
      it "Probar tomando solo los primeros 3 bloques de una cadena de 1 segundo bloque y 10 primer bloque, aplicados a pepe, esto deberia dar un pepe con 51 monedas" $ foldr ($) pepe (take 3 listaBlockChain) `shouldBe`  Usuario {nombre = "Jose", billetera = 51.0}
      it "Aplicar la BlockChain de 1 segundo bloque seguido de 10 segundo bloques, a una lista que contenga a pepe y lucho, esto deberia devolvernos una lista de pepe con 115 monedas y un lucho con 0 monedas" $  sum (map (billetera . (blockChain listaBlockChain)) [pepe,lucho]) `shouldBe` 115

bloque2 = [pepeDeposita5monedas, pepeDeposita5monedas, pepeDeposita5monedas, pepeDeposita5monedas ,pepeDeposita5monedas]

segundoBloque :: Bloque
segundoBloque unUsuario = unUsuario {
    billetera = (foldr ($) (billetera unUsuario) . map ($ unUsuario)) bloque2
}

elPeorBloque :: Usuario -> [Bloque] -> Bloque
elPeorBloque unUsuario [unBloque] =  unBloque
elPeorBloque unUsuario (cabezaBloque : medioBloque :colaBloque) | elPeorBloqueEsElSegundo cabezaBloque medioBloque unUsuario = elPeorBloque unUsuario (medioBloque:colaBloque)
                                                                | otherwise = elPeorBloque unUsuario (cabezaBloque:colaBloque)
-- auxiliar --
elPeorBloqueEsElSegundo :: Bloque -> Bloque -> Usuario -> Bool
elPeorBloqueEsElSegundo unBloque otroBloque unUsuario = billetera (unBloque unUsuario) >= billetera (otroBloque unUsuario)
--------------

saldoLuegoDeNBloques :: Int -> Usuario -> [Bloque] -> Usuario
saldoLuegoDeNBloques cantidadDeBloques unUsuario listaDeBloques = blockChain (take cantidadDeBloques listaDeBloques) unUsuario

listaBlockChain = (segundoBloque :(take 10 (repeat primerBloque)))

type BlockChain = [Bloque]
blockChain :: BlockChain -> Usuario -> Usuario
blockChain unaListaDeBloques unUsuario = foldr ($) unUsuario unaListaDeBloques


--BLOCKCHAIN INFINITO--

testBlockChainInfinito = hspec $ do
  describe "Testeo de BlockChain Infinito" $ do
    it "Cuantos elementos de  una BlockChain infinita creada a partir del bloque 1 deben usarse para que Pepe llegue a tener 10000 créditos, deberian ser 11 para llegar a 16386" $ length (listaCuantosNecesarios 10000 pepe (blockChainInfinito primerBloque))  `shouldBe` 11

blockChainInfinito :: Bloque -> [Bloque]
blockChainInfinito bloque =  bloque : (blockChainInfinito (bloque.bloque))

listaCuantosNecesarios :: Dinero -> Usuario -> [Bloque] -> [Bloque]
listaCuantosNecesarios numero usuario (cabeza : cola) | billetera (cabeza usuario) >= numero = [cabeza]
                                                      | billetera (cabeza usuario) < numero = (cabeza : listaCuantosNecesarios numero (cabeza usuario) cola)

-- El concepto clave para este ejercicio es el de Evaluación Diferida, ya que se busca que una vez encontrado el valor, que corte y no siga analizando la lista de forma infinita sin que corte --
