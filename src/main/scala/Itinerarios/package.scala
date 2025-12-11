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

  def itinerariosEscalas(vuelos: List[Vuelo], aeropuertos: List[Aeropuerto]): (String, String) => List[Itinerario] = {

    val todosIts = itinerarios(vuelos, aeropuertos)

    (origen: String, destino: String) => {
      val lista = todosIts(origen, destino)

      def escalas(it: Itinerario): Int =
        if (it.isEmpty) 0
        else (it.length - 1) + it.map(_.Esc).sum

      lista.sortBy(escalas).take(3)
    }
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

      it.map { v =>
        diferenciaMinutos(v.Org, v.HS, v.MS, v.Dst, v.HL, v.ML)
      }.sum

    }



    // Utilizando la función itinerarios base para obtener todos los itinerarios

    val todosItinerarios = itinerarios(vuelos, aeropuertos)

    // Función final que obtiene los tres itinerarios con el tiempo más corto en aire
    (cod1: String, cod2: String) =>
      todosItinerarios(cod1, cod2)            // Todos los itinerarios c1 -> c2
        .sortBy(tiempoVuelo)                  // Ordenar por tiempo total de vuelo
        .take(3)                              // Tomar los 3 mejores
  }

  def itinerarioSalida(vuelos: List[Vuelo], aeropuertos: List[Aeropuerto]): (String, String, Int, Int) => Itinerario = {

    // Convierte hora local a minutos desde medianoche
    def horaEnMinutos(h: Int, m: Int): Int = h * 60 + m

    def horaSalida(it: Itinerario): Int = it match {
      case Nil => 0
      case vuelo :: _ => horaEnMinutos(vuelo.HS, vuelo.MS)
    }

    def horaLlegada(it: Itinerario): Int = it match {
      case Nil => 0
      case _ => horaEnMinutos(it.last.HL, it.last.ML)
    }

    val todosItinerarios = itinerarios(vuelos, aeropuertos)

    (cod1: String, cod2: String, hCita: Int, mCita: Int) => {

      val candidatos = todosItinerarios(cod1, cod2)

      candidatos match {
        case Nil => Nil

        case _ =>
          val citaMinutos = horaEnMinutos(hCita, mCita)

          // Filtrar los itinerarios que llegan el mismo día y el día anterior
          val mismoDia = candidatos.filter { it =>
            horaLlegada(it) <= citaMinutos
          }

          val diaAnterior = candidatos.filter { it =>
            horaLlegada(it) > citaMinutos
          }

          // SIEMPRE elegir el de salida más tarde, priorizando mismo día
          mismoDia match {
            case Nil =>
              // Todos son del día anterior, elegir el de salida más tarde
              diaAnterior.maxBy(horaSalida)

            case _ =>
              // Hay del mismo día, elegir el de salida más tarde
              mismoDia.maxBy(horaSalida)
          }
      }
    }
  }

}
