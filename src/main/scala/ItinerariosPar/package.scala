import datos._
import common._
import Itinerarios._

import scala.collection.parallel._
import scala.collection.parallel.CollectionConverters._

package object ItinerariosPar {

  // ----------------------------------------------
  // 1. Cálculo de todos los itinerarios (paralelo)
  // ----------------------------------------------
  def itinerariosPar(vuelos: List[Vuelo], aeropuertos: List[Aeropuerto]): (String, String) => List[Itinerario] = {

    // IMPLEMENTACIÓN AQUÍ
    ???
  }

  // ----------------------------------------------
  // 2. Minimización de tiempo total de viaje (paralelo)
  // ----------------------------------------------
  def itinerariosTiempoPar(vuelos: List[Vuelo], aeropuertos: List[Aeropuerto]): (String, String) => List[Itinerario] = {

    // IMPLEMENTACIÓN AQUÍ
    ???
  }

  // ----------------------------------------------
  // 3. Minimización de escalas (paralelo)
  // ----------------------------------------------
  def itinerariosEscalasPar(vuelos: List[Vuelo], aeropuertos: List[Aeropuerto]): (String, String) => List[Itinerario] = {

    // IMPLEMENTACIÓN AQUÍ
    ???
  }

  // ----------------------------------------------
  // 4. Minimización de tiempo en el aire (paralelo)
  // ----------------------------------------------
  def itinerariosAirePar(vuelos: List[Vuelo], aeropuertos: List[Aeropuerto]): (String, String) => List[Itinerario] = {

    // IMPLEMENTACIÓN AQUÍ
    ???
  }

  // ----------------------------------------------
  // 5. Optimización de la hora de salida (paralelo)
  // ----------------------------------------------
  def itinerarioSalidaPar(vuelos: List[Vuelo], aeropuertos: List[Aeropuerto]): (String, String, Int, Int) => Itinerario = {

    // IMPLEMENTACIÓN AQUÍ
    ???
  }
}
