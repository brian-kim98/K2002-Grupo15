import Hechizos.*
import Artefactos.*
import Refuerzos.*

object mundo {
	
	var property fechaDeHoy = new Date(11,11,2018)
	
	var property fuerzaOscura = 5

	method eclipse() {
		fuerzaOscura *= 2
	}

}

class Personaje {

	const property artefactos = []
	const property capacidadDeCarga
	var property hechizoPreferido
	var property basePelea = 1
	var property monedas = 100

	method basePoder() = 3

	method nivelDeHechiceria() = self.basePoder() * self.hechizoPreferido().poder() + mundo.fuerzaOscura()

	method agregaLosArtefactos(unosArtefactos) {
		if (self.podesAgregarEstosArtefactos(unosArtefactos).negate()) {
			throw new ExcepcionDeNoPuedeCargarMas ("Tu personaje no puede cargar este/os Artefacto/s")
		}
		return self.artefactos().addAll(unosArtefactos)
	}
	
	/* Agrego este metodo, que si bien es redundante, para que quede mas lindo (es el peso que CARGA) */
	
	method pesoTotal() = self.losPesosDeTusArtefactos()
	
	method losPesosDeTusArtefactos() = self.artefactos().sum({ artefacto => artefacto.cuantoPesas() })

	method podesAgregarEstosArtefactos(artefactosNuevos) = self.losPesosDeTusArtefactos() + artefactosNuevos.sum({ artefacto => artefacto.cuantoPesas() }) <= self.capacidadDeCarga()

	method agregaArtefacto(unArtefacto) = self.agregaLosArtefactos([ unArtefacto ])

	method removeArtefacto(unArtefacto) = self.artefactos().remove(unArtefacto)

	method removeTodosLosArtefactos() = self.artefactos().clear()

	method teCreesPoderoso() = self.hechizoPreferido().esPoderoso()

	method valorDeLucha() = self.basePelea() + self.poderDeArtefactos()

	method poderDeArtefactos() = self.artefactos().sum({ artefacto => artefacto.unidadesDeLucha(self) })

	method mejorLuchadorQueMago() = self.valorDeLucha() > self.nivelDeHechiceria()

	method estasCargado() = self.artefactos().size() >= 5

	method mejorArtefacto() = self.artefactosSin(espejoFantastico).max({ artefacto => artefacto.unidadesDeLucha(self) })

	method artefactosSin(unArtefacto) = self.artefactos().filter({ artefacto => artefacto != unArtefacto })

	method cantidadDeArtefactosSin(unArtefacto) = self.artefactosSin(unArtefacto).size()

	method monedas(nuevaCantidad) {
		monedas = nuevaCantidad
	}

	method cumplisObjetivo() {
		self.ganasMonedas(10)
	}

	method ganasMonedas(monedasGanadas) {
		self.monedas(self.monedas() + monedasGanadas)
	}

	method compraArtefacto(artefactoAComprar) {
		if (self.podesComprarArtefacto(artefactoAComprar).negate()) {
			throw new ExcepcionDeNoTenesMonedas("Te faltan un par de monedas en la billetera para comprar el artefacto")
		}
		self.agregaArtefacto(artefactoAComprar)
		self.monedas(self.monedas() - artefactoAComprar.precio())
	}
	
	method comprarArtefactoAunComerciante(unArtefacto, unComerciante){
		
		if (self.podesComprarleAlComerciante(unComerciante, unArtefacto).negate()) {
			throw new ExcepcionDeNoTenesMonedas("Te faltan un par de monedas en la billetera para comprar el artefacto")
		}
		self.agregaArtefacto(unArtefacto)
		self.monedas(self.monedas() - unComerciante.valor(unArtefacto))
		
	}
	
	method podesComprarleAlComerciante(unComerciante, unArtefacto) = unComerciante.valor(unArtefacto) <= self.monedas()
	
	method podesComprarArtefacto(artefactoAComprar) = artefactoAComprar.precio() <= self.monedas()

	method compraHechizo(nuevoHechizo) {
		if ( self.descuento() + self.monedas() < nuevoHechizo.precio()) {
			throw new ExcepcionDeNoTenesMonedas()
		}
			self.monedas(self.monedas() -(nuevoHechizo.precio() - self.descuento()).max(0))
			self.hechizoPreferido(nuevoHechizo)
	}
	
	method descuento() = self.hechizoPreferido().precio() / 2
	

}

class ExcepcionDeNoTenesMonedas inherits Exception {}

class ExcepcionDeNoPuedeCargarMas inherits Exception {}


class NPC inherits Personaje {

	var property nivel = nivelFacil

	override method valorDeLucha() = (super()) * self.nivel().multiplicador()
}

object nivelFacil {
	method multiplicador() = 1
}

object nivelModerado {
	method multiplicador() = 2
}

object nivelDificil {
	method multiplicador() = 4
}

class Comerciante{
	
	var property impuesto
	var property porcentajeIVA = 21
	
	method recategorizar() {
		
		self.impuesto().recategorizar(self)
	}
	
	method valor(unArtefacto) = unArtefacto.precio() + self.impuesto().impuesto(unArtefacto)
	
}

class ComercianteIndependiente{
	
	var property comision
	
	method impuesto(unArtefacto) = unArtefacto.precio() * self.comision() / 100
	
	method recategorizar(unComerciante) {
		
		if(self.comision()*2 > unComerciante.porcentajeIVA()){
			unComerciante.impuesto(comercianteRegistrado)
		}
		else{
			self.comision(self.comision()*2)
		}
	}
	
	
}

object comercianteRegistrado{
	
	method impuesto(unArtefacto) = unArtefacto.precio() * (unArtefacto.porcentajeIVA()/100)
	
	method recategorizar(unComerciante) {
		
		unComerciante.impuesto(comercianteConImpuestoAlaGanancia)
	}
}

object comercianteConImpuestoAlaGanancia{
	/* es parta todos igual, por eso esto es un objeto y no una clase */
	
	var property minimoNoImponible = 5
	
	method impuesto(unArtefacto) {
		
		if(unArtefacto.precio() >= self.minimoNoImponible()){
			
			return (unArtefacto.precio() - self.minimoNoImponible()) * 35 /100
		}
		else{
			return 0
		}
	}
	
	method recategorizar(unComerciante) {/* sigue igual */}
}