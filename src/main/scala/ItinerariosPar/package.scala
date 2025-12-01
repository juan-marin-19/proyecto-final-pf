import datos._
import common._
import Itinerarios._
import scala.collection.parallel.CollectionConverters . _
import scala.collection.parallel.ParSeq

package object ItinerariosPar {


    // ... (Tu función itinerarios secuencial aquí) ...

    // ----------------------------------------------
    // 1b. Cálculo de todos los itinerarios (PARALELO)
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




}







