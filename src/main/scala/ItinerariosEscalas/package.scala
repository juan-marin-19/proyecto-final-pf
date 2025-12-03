import datos._
package object ItinerariosEscalas {
    
    def itinerariosEscalas(vuelos: List[Vuelo], aeropuertos: List[Aeropuerto]):
    (String, String, Int) => List[Itinerario] = {

    // Construcción recursiva con límite de escalas
    def construir(actual: String,
                destino: String,
                visitados: Set[String],
                maxEscalas: Int): List[Itinerario] = {

    // Si llegamos al destino → itinerario válido (sin más vuelos)
    if (actual == destino)
      List(Nil)

    // Si ya no quedan escalas permitidas → no hay itinerarios
    else if (maxEscalas < 0)
      Nil

    else {
      // Vuelos salientes desde el aeropuerto actual
      val salientes =
        for {
          v <- vuelos
          if v.Org == actual
          if !visitados(v.Dst) // evita ciclos
        } yield v

      // Expandir cada posible vuelo
      for {
        vuelo <- salientes
        resto <- construir(
                    vuelo.Dst,
                    destino,
                    visitados + vuelo.Dst,
                    maxEscalas - 1  // consumir 1 escala
                 )
      } yield vuelo :: resto
    }
  }

  (origen: String, destino: String, maxEscalas: Int) =>
    construir(origen, destino, Set(origen), maxEscalas)
}

}
