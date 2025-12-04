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

  def itinerariosTiempo(vuelos: List[Vuelo], aeropuertos: List[Aeropuerto]): (String, String) => List[Itinerario] = {
    // --- Conversion de horas locales a minutos UTC ---

    // mapa, codigo aeropuerto -> offset en minutos
    val offsetPorCodigo: Map[String, Int] =
      aeropuertos
        .map(a => a.Cod -> ((a.GMT / 100) * 60))
        .toMap

    // Devuelve el offset del aeropuerto en minutos
    def offsetMinutos(cod: String): Int =
      offsetPorCodigo.get(cod).fold(0)(offset => offset)

    // Convierte una hora local (h:m) en "minutos UTC"
    def minutosUTC(cod: String, h: Int, m: Int): Int =
      h * 60 + m - offsetMinutos(cod)

    // Diferencia minima no negativa entre dos eventos
    def diferenciaMinutos(cod1: String, h1: Int, m1: Int,
                          cod2: String, h2: Int, m2: Int): Int = {
      val t1 = minutosUTC(cod1, h1, m1)
      val t2 = minutosUTC(cod2, h2, m2)
      val d  = t2 - t1
      if (d >= 0) d else d + 24 * 60 // Si la diferencia es negativa le suma 24 horas en minutos
    }

    // --- Construcción de la secuencia de eventos (salidas y llegadas) de un itinerario ---

    type Evento = (String, Int, Int) // (código aeropuerto, hora, minuto) en hora local

    def eventos(it: Itinerario): List[Evento] = it match {
      case Nil => Nil
      case v0 :: vs =>
        def construirEventos(actual: Vuelo, resto: List[Vuelo]): List[Evento] = resto match {
          case Nil => List((actual.Dst, actual.HL, actual.ML))
          case siguiente :: cola =>
            (actual.Dst, actual.HL, actual.ML) ::
              (siguiente.Org, siguiente.HS, siguiente.MS) ::
              construirEventos(siguiente, cola)
        }
        (v0.Org, v0.HS, v0.MS) :: construirEventos(v0, vs)
    }

    // --- Tiempo total de viaje (vuelos + esperas) de un itinerario ---

    def tiempoTotal(it: Itinerario): Int = {
      def sumar(evts: List[Evento]): Int = evts match {
        case Nil           => 0
        case _ :: Nil      => 0
        case (c1,h1,m1) :: (c2,h2,m2) :: resto =>
          diferenciaMinutos(c1,h1,m1,c2,h2,m2) +
            sumar((c2,h2,m2) :: resto)
      }

      sumar(eventos(it))
    }

    val todosItinerarios = itinerarios(vuelos, aeropuertos)

    // Funcion final
    (cod1: String, cod2: String) =>
      todosItinerarios(cod1, cod2)        // todos los itinerarios c1 -> c2
        .sortBy(tiempoTotal)              // ordenados por tiempo total de viaje
        .take(3)                          // los tres mejores (o menos)
  }

  def itinerariosEscalas(vuelos: List[Vuelo], aeropuertos: List[Aeropuerto]): (String, String, Int) => List[Itinerario] = {
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

  def itinerariosAire(vuelos: List[Vuelo], aeropuertos: List[Aeropuerto]): (String, String) => List[Itinerario] = {

    // Conversion de horas locales a minutos UTC

    // mapa, codigo aeropuerto - offset en minutos
    val offsetPorCodigo: Map[String, Int] =
      aeropuertos
        .map(a => a.Cod -> ((a.GMT / 100) * 60))
        .toMap

    // Devuelve el offset del aeropuerto en minutos
    def offsetMinutos(cod: String): Int =
      offsetPorCodigo.get(cod).fold(0)(offset => offset)

    // Convierte una hora local (h:m) en "minutos UTC"
    def minutosUTC(cod: String, h: Int, m: Int): Int =
      h * 60 + m - offsetMinutos(cod)

    // Diferencia minima no negativa entre dos eventos
    def diferenciaMinutos(cod1: String, h1: Int, m1: Int,
                          cod2: String, h2: Int, m2: Int): Int = {
      val t1 = minutosUTC(cod1, h1, m1)
      val t2 = minutosUTC(cod2, h2, m2)
      val d  = t2 - t1
      if (d >= 0) d else d + 24 * 60 // Si la diferencia es negativa le suma 24 horas en minutos
    }

    // Construcción de la secuencia de eventos (salidas y llegadas) de un itinerario

    type Evento = (String, Int, Int) // (código aeropuerto, hora, minuto) en hora local

    def eventos(it: Itinerario): List[Evento] = it match {
      case Nil => Nil
      case v0 :: vs =>
        def construirEventos(actual: Vuelo, resto: List[Vuelo]): List[Evento] =
          resto match {
            case Nil =>
              // Solo falta la llegada del ultimo vuelo
              List((actual.Dst, actual.HL, actual.ML))
            case siguiente :: cola =>
              // llegada del actual, salida del siguiente, y se sigue
              (actual.Dst, actual.HL, actual.ML) ::
                (siguiente.Org, siguiente.HS, siguiente.MS) ::
                construirEventos(siguiente, cola)
          }

        // Salida del primer vuelo seguida del resto de eventos
        (v0.Org, v0.HS, v0.MS) :: construirEventos(v0, vs)
    }

    // Tiempo total de viaje (vuelos + esperas) de un itinerario

    def tiempoVuelo(it: Itinerario): Int = {
      def sumarVuelo(evts: List[Evento]): Int = evts match {
        case Nil           => 0
        case _ :: Nil      => 0
        case (c1,h1,m1) :: (c2,h2,m2) :: resto =>
          // Solo sumar los tiempos de vuelo, es decir, la diferencia entre la llegada de un vuelo y la salida del siguiente
          diferenciaMinutos(c1, h1, m1, c2, h2, m2) + sumarVuelo((c2, h2, m2) :: resto)
      }

      sumarVuelo(eventos(it))
    }


    // Utilizando la función itinerarios base para obtener todos los itinerarios

    val todosItinerarios = itinerarios(vuelos, aeropuertos)

    // Función final que obtiene los tres itinerarios con el tiempo más corto en aire
    (cod1: String, cod2: String) =>
      todosItinerarios(cod1, cod2)            // Todos los itinerarios c1 -> c2
        .sortBy(tiempoVuelo)                  // Ordenar por tiempo total de vuelo
        .take(3)                              // Tomar los 3 mejores
  }

}
