import datos._
import common._
import Itinerarios._
import scala.collection.parallel.CollectionConverters . _
import scala.collection.parallel.ParSeq

package object ItinerariosPar {

    // ----------------------------------------------
    // 1 Cálculo de todos los itinerarios
    // ----------------------------------------------
    def itinerariosPar(vuelos: List[Vuelo], aeropuertos: List[Aeropuerto]): (String, String) => List[Itinerario] = {

      def construir(actual: String, destino: String, visitados: Set[String]): List[Itinerario] = {

        val umbral = 4
        // Caso base
        if (actual == destino)
          List(Nil)
        else {
          // Vuelos que salen del aeropuerto actual (Secuencial)
          val salientes =
            for {
              v <- vuelos
              if v.Org == actual
              if !visitados(v.Dst)
            } yield v

          // Función auxiliar que usa common.parallel
          def parExplore(vuelosPorExplorar: List[Vuelo]): List[Itinerario] = {
            val n = vuelosPorExplorar.length

            if (n <= umbral) {
              // Caso base de la exploración paralela
              for {
                vuelo <- vuelosPorExplorar
                resto <- construir(vuelo.Dst, destino, visitados + vuelo.Dst)
              } yield vuelo :: resto

            } else {
              // Paso recursivo paralelo
              val (left, right) = vuelosPorExplorar.splitAt(n / 2)

              // Llama a parallel para ejecutar las dos mitades en paralelo
              val (resLeft, resRight) = parallel(
                parExplore(left),
                parExplore(right)
              )

              resLeft ++ resRight
            }
          }

          // Inicia la exploración paralela
          parExplore(salientes)
        }
      }

      // Función final
      (origen: String, destino: String) =>
        construir(origen, destino, Set(origen))

    }

  // ----------------------------------------------
  // 2. Minimización de tiempo total de viaje (paralelo)
  // ----------------------------------------------
  def itinerariosTiempoPar(vuelos: List[Vuelo], aeropuertos: List[Aeropuerto]): (String, String) => List[Itinerario] = {

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
        def construirEventos(actual: Vuelo, resto: List[Vuelo]): List[Evento] =
          resto match {
            case Nil =>
              List((actual.Dst, actual.HL, actual.ML))
            case siguiente :: cola =>
              (actual.Dst, actual.HL, actual.ML) ::
                (siguiente.Org, siguiente.HS, siguiente.MS) ::
                construirEventos(siguiente, cola)
          }

        // Salida del primer vuelo seguida del resto de eventos
        (v0.Org, v0.HS, v0.MS) :: construirEventos(v0, vs)
    }

    // Tiempo total de viaje (vuelos + esperas) de un itinerario

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

    // Reutilizar itinerariosPar para generar todos los itinerarios
    val todosItinerariosPar = itinerariosPar(vuelos, aeropuertos)

    // Función que retorna los 3 mejores itinerarios por tiempo
    (origen: String, destino: String) => {
      val todosItinerarios = todosItinerariosPar(origen, destino)

      // Función para calcular tiempos en paralelo
      def calcularTiemposParalelo(its: List[Itinerario]): List[(Itinerario, Int)] = {
        its match {
          case Nil => Nil
          case _ =>
            val n = its.length
            val umbral = 10

            if (n <= umbral) {
              // Caso base: calcular secuencialmente
              its.map(it => (it, tiempoTotal(it)))
            } else {
              // Dividir y conquistar en paralelo
              val (left, right) = its.splitAt(n / 2)
              val (resLeft, resRight) = parallel(
                calcularTiemposParalelo(left),
                calcularTiemposParalelo(right)
              )
              resLeft ++ resRight
            }
        }
      }

      todosItinerarios match {
        case Nil => Nil
        case _ =>
          calcularTiemposParalelo(todosItinerarios)
            .sortBy(_._2)       // Ordenar por tiempo (menor primero)
            .take(3)            // Tomar los 3 mejores
            .map(_._1)          // Extraer solo los itinerarios
      }
    }
  }


}







