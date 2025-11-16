import datos._

package object Itinerarios {

  // ----------------------------------------------
  // 1. Cálculo de todos los itinerarios
  // ----------------------------------------------
  def itinerarios(vuelos: List[Vuelo], aeropuertos: List[Aeropuerto]): (String, String) => List[Itinerario] = {

    def construir(actual: String, destino: String, visitados: Set[String]): List[Itinerario] = {

      // Caso base
      if (actual == destino)
        List(Nil)
      else {
        // Vuelos que salen del aeropuerto actual
        val salientes =
          for {
            v <- vuelos
            if v.Org == actual
            if !visitados(v.Dst) // EVITA REVISITAR DESTINOS
          } yield v

        // Expansión recursiva
        for {
          vuelo <- salientes
          resto <- construir(vuelo.Dst, destino, visitados + vuelo.Dst)
        }
        yield vuelo :: resto
      }
    }

    // Función final
    (origen: String, destino: String) =>
      construir(origen, destino, Set(origen))

  }

/*
  // ----------------------------------------------
  // 2. Minimización de tiempo total de viaje
  // ----------------------------------------------
  def itinerariosTiempo(vuelos: List[Vuelo], aeropuertos: List[Aeropuerto]): (String, String) => List[Itinerario] = {

    // IMPLEMENTACIÓN AQUÍ
    ???
  }

  // ----------------------------------------------
  // 3. Minimización de número de escalas
  // ----------------------------------------------
  def itinerariosEscalas(vuelos: List[Vuelo], aeropuertos: List[Aeropuerto]): (String, String) => List[Itinerario] = {

    // IMPLEMENTACIÓN AQUÍ
    ???
  }

  // ----------------------------------------------
  // 4. Minimización de tiempo en el aire
  // ----------------------------------------------
  def itinerariosAire(vuelos: List[Vuelo], aeropuertos: List[Aeropuerto]): (String, String) => List[Itinerario] = {

    // IMPLEMENTACIÓN AQUÍ
    ???
  }

  // ----------------------------------------------
  // 5. Optimización de la hora de salida
  // ----------------------------------------------
  def itinerarioSalida(vuelos: List[Vuelo], aeropuertos: List[Aeropuerto]): (String, String, Int, Int) => Itinerario = {

    // IMPLEMENTACIÓN AQUÍ
    ???
  }

 */
}
