import datos._                                   // Importa TODO lo que está definido en el paquete `datos` (Vuelo, Aeropuerto, Itinerario, etc.)
import common._                                  // Importa funciones comunes, en especial `parallel` para paralelismo divide & conquer
import Itinerarios._                             // Importa las funciones secuenciales definidas en el objeto/package `Itinerarios`
import scala.collection.parallel.CollectionConverters._ // Permite convertir colecciones normales (List, Seq, etc.) a versiones paralelas (`par`)
import scala.collection.parallel.ParSeq          // Importa el tipo `ParSeq`, una secuencia que se procesa en paralelo

package object ItinerariosPar {                  // Define un `package object` llamado ItinerariosPar, donde se agrupan funciones relacionadas

  // ----------------------------------------------
  // 1 Cálculo de todos los itinerarios
  // ----------------------------------------------

  /*
   * FUNCIÓN: itinerariosPar
   * -----------------------------------
   * ¿Qué hace?
   *  - Dado:
   *      - una lista de vuelos (`vuelos`)
   *      - una lista de aeropuertos (`aeropuertos`)  <-- aquí realmente no se usan, pero se recibe por simetría con otras funciones
   *    construye una FUNCIÓN que, cuando le pasas:
   *      - un código de aeropuerto de origen (String)
   *      - un código de aeropuerto de destino (String)
   *    te devuelve TODOS los itinerarios posibles (caminos) desde origen hasta destino,
   *    sin repetir aeropuertos para evitar ciclos.
   *
   * ¿Cómo lo hace?
   *  1. Define una función interna `construir(actual, destino, visitados)`:
   *      - `actual`: aeropuerto donde estoy actualmente.
   *      - `destino`: aeropuerto al que quiero llegar.
   *      - `visitados`: conjunto de aeropuertos que ya visité para no volver a entrar en ciclos.
   *
   *  2. Si `actual == destino`, significa que llegué al destino:
   *      - Devuelvo una lista con un único itinerario vacío: `List(Nil)`.
   *        Esto representa: “el camino ya está completo, no agrego más vuelos”.
   *
   *  3. Si NO estoy en el destino:
   *      - Busco TODOS los vuelos que salen del aeropuerto `actual` y que llevan a un aeropuerto
   *        que aún no esté en `visitados`.
   *
   *  4. Luego uso una función auxiliar `parExplore` que:
   *      - Recibe una lista de vuelos `vuelosPorExplorar`.
   *      - Si la lista es "pequeña" (menor o igual que un umbral), explora secuencialmente.
   *      - Si la lista es "grande", la divide en dos mitades (`left` y `right`) y explora ambas en paralelo
   *        usando `parallel( ... , ... )`.
   *
   *  5. En cada exploración:
   *      - Para cada vuelo `vuelo` en la lista:
   *          - Llama recursivamente a `construir` desde el destino de ese vuelo.
   *          - Prependa el vuelo al resto del itinerario (`vuelo :: resto`).
   *
   *  6. Al final, `itinerariosPar(vuelos, aeropuertos)` devuelve una FUNCIÓN:
   *      (origen: String, destino: String) => construir(origen, destino, Set(origen))
   *      Es decir, empieza la búsqueda desde el origen y marcando el origen como ya visitado.
   */

  def itinerariosPar(                            // Define la función `itinerariosPar`
                                                 vuelos: List[Vuelo],                         // Parámetro: lista de vuelos disponibles
                                                 aeropuertos: List[Aeropuerto]                // Parámetro: lista de aeropuertos (no se usa dentro, pero se recibe igual)
                    ): (String, String) => List[Itinerario] = {    // Tipo de retorno: una función que va de (origen, destino) a lista de itinerarios

    def construir(                               // Define una función interna recursiva llamada `construir`
                                                 actual: String,                            // `actual`: aeropuerto en el que estamos ahora
                                                 destino: String,                           // `destino`: aeropuerto al que queremos llegar
                                                 visitados: Set[String]                     // `visitados`: conjunto de aeropuertos que ya hemos visitado
                 ): List[Itinerario] = {                      // Devuelve una lista de itinerarios (cada itinerario es una lista de vuelos)

      val umbral = 10                            // Umbral para decidir cuándo dejar de paralelizar y hacer la exploración secuencial
      // Caso base
      if (actual == destino)                     // Si el aeropuerto actual es igual al destino...
        List(Nil)                                // ...entonces ya llegamos: devolvemos una lista que contiene un itinerario vacío
      else {                                     // Si NO estamos en el destino, seguimos explorando
        // Vuelos que salen del aeropuerto actual (Secuencial)
        val salientes =                          // Definimos `salientes`: lista de vuelos que salen de `actual`
          for {                                  // Comenzamos un for-comprehension (azúcar sintáctico para map/flatMap/filter)
            v <- vuelos                          // Tomamos cada vuelo `v` de la lista de vuelos
            if v.Org == actual                   // Filtro: solo vuelos cuyo aeropuerto de origen `Org` sea el `actual`
            if !visitados(v.Dst)                 // Filtro: solo vuelos cuyo destino `Dst` NO haya sido visitado todavía
          } yield v                              // Resultado del for: una lista de vuelos `v` que cumplen las condiciones

        // Función auxiliar que usa common.parallel
        def parExplore(                          // Definimos una función auxiliar `parExplore` para explorar vuelos en paralelo
                                                 vuelosPorExplorar: List[Vuelo]         // Recibe una lista de vuelos a explorar
                      ): List[Itinerario] = {                  // Devuelve una lista de itinerarios generados a partir de esos vuelos
          val n = vuelosPorExplorar.length       // Calculamos el tamaño de la lista de vuelos a explorar

          if (n <= umbral) {                     // Si la lista tiene pocos vuelos (menor o igual al umbral)...
            // Caso base de la exploración paralela
            for {                                // Hacemos exploración SECUNCIAL con un for-comprehension
              vuelo <- vuelosPorExplorar         // Tomamos cada vuelo individualmente
              resto <- construir(                // Para cada vuelo, llamamos recursivamente a `construir`
                vuelo.Dst,                       // El nuevo aeropuerto actual será el destino del vuelo
                destino,                         // El destino sigue siendo el mismo
                visitados + vuelo.Dst            // Añadimos el destino del vuelo al conjunto de visitados
              )
            } yield vuelo :: resto               // Construimos un nuevo itinerario anteponiendo `vuelo` a la lista `resto`

          } else {                               // Si la lista de vuelos es grande (n > umbral)...
            // Paso recursivo paralelo
            val (left, right) =                  // Dividimos la lista de vuelos en dos mitades
              vuelosPorExplorar.splitAt(n / 2)   // `splitAt` parte la lista en índice `n/2`: left (primera mitad), right (segunda)

            // Llama a parallel para ejecutar las dos mitades en paralelo
            val (resLeft, resRight) = parallel(  // Llamamos a `parallel` (de `common`) para ejecutar dos tareas a la vez
              parExplore(left),                  // Primera tarea: explorar la mitad izquierda recursivamente
              parExplore(right)                  // Segunda tarea: explorar la mitad derecha recursivamente
            )

            resLeft ++ resRight                  // Concatenamos los resultados de ambas mitades y los devolvemos
          }
        }

        // Inicia la exploración paralela
        parExplore(salientes)                    // Comenzamos la exploración paralela con la lista de vuelos `salientes`
      }
    }

    // Función final
    (origen: String, destino: String) =>         // Esta es la función anónima final que se devuelve
      construir(origen, destino, Set(origen))    // Llama a `construir` empezando desde el origen y marcando el origen como visitado

  }

  // ----------------------------------------------
  // 2. Minimización de tiempo total de viaje (paralelo)
  // ----------------------------------------------

  /*
   * FUNCIÓN: itinerariosTiempoPar
   * -----------------------------------
   * ¿Qué hace?
   *  - Devuelve, para cada par (origen, destino), los 3 itinerarios
   *    con MENOR TIEMPO TOTAL (vuelos + esperas).
   *
   * Pasos:
   *  1. Convierte la información de los aeropuertos en un mapa:
   *        código aeropuerto -> offset de GMT en minutos.
   *
   *  2. Define funciones para:
   *        - pasar una hora local (h, m) de un aeropuerto a "minutos UTC".
   *        - calcular la diferencia no negativa (módulo 24 horas) entre dos eventos de tiempo.
   *
   *  3. Define `eventos(it)` que transforma un itinerario (lista de vuelos) en una lista de eventos:
   *        (código, hora, minuto) para salidas y llegadas, en orden.
   *
   *  4. Define `tiempoTotal(it)` que:
   *        - toma la lista de eventos
   *        - suma las diferencias consecutivas entre cada par de eventos (en minutos UTC)
   *        - eso incluye tanto vuelos como esperas.
   *
   *  5. Usa `itinerariosPar` para generar TODOS los itinerarios posibles entre origen y destino.
   *
   *  6. Define `calcularTiemposParalelo`, que:
   *        - recibe una lista de itinerarios
   *        - si es pequeña, calcula (it, tiempoTotal(it)) de forma secuencial
   *        - si es grande, la divide en dos y calcula en paralelo con `parallel`.
   *
   *  7. Ordena la lista de pares (itinerario, tiempo) por el tiempo
   *     y devuelve solo los 3 mejores itinerarios.
   */

  def itinerariosTiempoPar(                      // Define la función `itinerariosTiempoPar`
                                                 vuelos: List[Vuelo],                         // Lista de vuelos disponibles
                                                 aeropuertos: List[Aeropuerto]                // Lista de aeropuertos disponibles
                          ): (String, String) => List[Itinerario] = {    // Devuelve una función (origen, destino) => lista de itinerarios

    // --- Conversion de horas locales a minutos UTC ---

    // mapa, codigo aeropuerto -> offset en minutos
    val offsetPorCodigo: Map[String, Int] =      // Creamos un mapa de códigos de aeropuerto a offset en minutos
      aeropuertos                                // Tomamos la lista de aeropuertos
        .map(a => a.Cod -> ((a.GMT / 100) * 60)) // Para cada aeropuerto `a`, asociamos el código con su GMT (en horas) convertido a minutos
        .toMap                                   // Convertimos la lista de pares (clave, valor) en un Map

    // Devuelve el offset del aeropuerto en minutos
    def offsetMinutos(cod: String): Int =        // Función que dado un código de aeropuerto devuelve el offset en minutos
      offsetPorCodigo.get(cod).fold(0)(offset => offset) // Si el código existe, devuelve el offset; si no, devuelve 0 por defecto

    // Convierte una hora local (h:m) en "minutos UTC"
    def minutosUTC(                              // Función que convierte hora local a "minutos UTC"
                                                 cod: String,                               // Código del aeropuerto
                                                 h: Int,                                    // Hora local
                                                 m: Int                                     // Minuto local
                  ): Int =                                     // Devuelve un entero: minutos UTC
      h * 60 + m - offsetMinutos(cod)            // Convierte h:m a minutos y resta el offset de ese aeropuerto

    // Diferencia minima no negativa entre dos eventos
    def diferenciaMinutos(                       // Función para calcular la diferencia de tiempo entre 2 eventos
                                                 cod1: String, h1: Int, m1: Int,            // Primer evento: aeropuerto, hora, minuto
                                                 cod2: String, h2: Int, m2: Int             // Segundo evento: aeropuerto, hora, minuto
                         ): Int = {                                   // Devuelve la diferencia en minutos, ajustada al día siguiente si es negativa
      val t1 = minutosUTC(cod1, h1, m1)          // Convertimos el primer evento a minutos UTC
      val t2 = minutosUTC(cod2, h2, m2)          // Convertimos el segundo evento a minutos UTC
      val d  = t2 - t1                           // Calculamos la diferencia de tiempos t2 - t1
      if (d >= 0) d else d + 24 * 60             // Si es negativa, sumamos 24 horas (en minutos) para simular pasar al día siguiente
    }

    // --- Construcción de la secuencia de eventos (salidas y llegadas) de un itinerario ---

    type Evento = (String, Int, Int)             // Definimos un tipo alias `Evento`: (código aeropuerto, hora, minuto) en hora local

    def eventos(it: Itinerario): List[Evento] = it match { // Función que construye la lista de eventos a partir de un itinerario
      case Nil => Nil                            // Si el itinerario está vacío, no hay eventos
      case v0 :: vs =>                           // Si hay al menos un vuelo: `v0` es el primero, `vs` es la cola
        def construirEventos(                    // Función interna recursiva para construir el resto de eventos
                                                 actual: Vuelo,                         // El vuelo actual
                                                 resto: List[Vuelo]                     // Lista de vuelos restantes
                            ): List[Evento] =                        // Devuelve la lista de eventos generada por estos vuelos
          resto match {                          // Analizamos la lista de vuelos restantes
            case Nil =>                          // Si no quedan más vuelos...
              List((actual.Dst, actual.HL, actual.ML)) // Solo agregamos la llegada del último vuelo
            case siguiente :: cola =>            // Si hay un siguiente vuelo...
              (actual.Dst, actual.HL, actual.ML) :: // Agregamos la llegada del vuelo actual
                (siguiente.Org, siguiente.HS, siguiente.MS) :: // Agregamos la salida del siguiente vuelo
                construirEventos(siguiente, cola) // Y continuamos recursivamente con el siguiente y el resto
          }

        // Salida del primer vuelo seguida del resto de eventos
        (v0.Org, v0.HS, v0.MS) ::                // Primero agregamos la salida del primer vuelo
          construirEventos(v0, vs)               // Luego construimos los eventos del resto de vuelos
    }

    // Tiempo total de viaje (vuelos + esperas) de un itinerario

    def tiempoTotal(it: Itinerario): Int = {     // Función que calcula el tiempo total de viaje de un itinerario
      def sumar(evts: List[Evento]): Int = evts match { // Función interna que suma las diferencias entre eventos
        case Nil           => 0                  // Si no hay eventos, el tiempo total es 0
        case _ :: Nil      => 0                  // Si solo hay un evento, no hay intervalo que sumar
        case (c1,h1,m1) :: (c2,h2,m2) :: resto =>// Si hay al menos dos eventos...
          diferenciaMinutos(c1,h1,m1,c2,h2,m2) + // Sumamos la diferencia de tiempo entre el primero y el segundo
            sumar((c2,h2,m2) :: resto)           // Y seguimos recursivamente con el resto de eventos
      }

      sumar(eventos(it))                         // Calculamos primero los eventos del itinerario y luego sumamos sus diferencias
    }

    // Reutilizar itinerariosPar para generar todos los itinerarios
    val todosItinerariosPar =                    // Guardamos una referencia a la función resultante de `itinerariosPar`
      itinerariosPar(vuelos, aeropuertos)        // Esta función genera TODOS los itinerarios posibles entre dos aeropuertos

    // Función que retorna los 3 mejores itinerarios por tiempo
    (origen: String, destino: String) => {       // Definimos la función final que recibe origen y destino
      val todosItinerarios =                     // Calculamos todos los itinerarios posibles entre origen y destino
        todosItinerariosPar(origen, destino)

      // Función para calcular tiempos en paralelo
      def calcularTiemposParalelo(               // Función que asocia cada itinerario con su tiempo total (en paralelo)
                                                 its: List[Itinerario]                    // Recibe una lista de itinerarios
                                 ): List[(Itinerario, Int)] = {             // Devuelve una lista de pares (itinerario, tiempo total)
        its match {                              // Hacemos pattern matching con la lista
          case Nil => Nil                        // Si está vacía, no hay nada que calcular
          case _ =>                              // Si NO está vacía...
            val n = its.length                   // Obtenemos cuántos itinerarios hay
            val umbral = 10                      // Definimos un umbral para decidir si paralelizamos o no

            if (n <= umbral) {                   // Si la cantidad de itinerarios es pequeña...
              // Caso base: calcular secuencialmente
              its.map(it => (it, tiempoTotal(it))) // Calculamos su tiempo total uno por uno, de forma secuencial
            } else {                             // Si la lista es grande...
              // Dividir y conquistar en paralelo
              val (left, right) = its.splitAt(n / 2) // La partimos en dos listas

              val (resLeft, resRight) = parallel(    // Llamamos a `parallel` para calcular en paralelo
                calcularTiemposParalelo(left),       // Calculamos tiempos para la mitad izquierda
                calcularTiemposParalelo(right)       // Calculamos tiempos para la mitad derecha
              )
              resLeft ++ resRight                // Unimos los resultados obtenidos de ambas mitades
            }
        }
      }

      todosItinerarios match {                   // Revisamos los itinerarios obtenidos
        case Nil => Nil                          // Si no hay itinerarios, devolvemos Nil
        case _ =>                                // Si hay al menos uno...
          calcularTiemposParalelo(todosItinerarios) // Calculamos los tiempos en paralelo
            .sortBy(_._2)                       // Ordenamos la lista por el segundo elemento del par (el tiempo total)
            .take(3)                            // Tomamos los 3 primeros (los más rápidos)
            .map(_._1)                          // Nos quedamos solo con el itinerario, sin el tiempo
      }
    }
  }

  // -------------------------------------------------------
  //  Itinerarios con límite de escala (versión paralela)
  // -------------------------------------------------------

  /*
   * FUNCIÓN: itinerariosEscalasPar
   * -----------------------------------
   * ¿Qué hace?
   *  - Genera itinerarios paralelamente y devuelve los 3 que minimizan
   *    una métrica basada en ESCALAS:
   *        (cantidad de vuelos - 1) + suma de Esc de cada vuelo.
   *
   * Pasos:
   *  1. Usa una función recursiva `construirPar` muy parecida a `construir` de antes,
   *     pero con paralelización divide & conquer.
   *
   *  2. Desde un aeropuerto `actual`, busca todos los vuelos salientes a destinos no visitados.
   *
   *  3. Paraleliza la exploración de esos vuelos:
   *      - Si hay pocos vuelos, explora secuencialmente.
   *      - Si hay muchos, divide en dos mitades y usa `parallel`.
   *
   *  4. Genera todos los itinerarios posibles del origen al destino.
   *
   *  5. Define `escalasMedida(it)` que calcula:
   *        (it.length - 1) + sum(it.map(_.Esc)).
   *
   *  6. Convierte la lista de itinerarios a una colección paralela (`par`),
   *     calcula la métrica para cada uno y se queda con los 3 mejores.
   */

  def itinerariosEscalasPar(                     // Define la función `itinerariosEscalasPar`
                                                 vuelos: List[Vuelo],                         // Lista de vuelos disponibles
                                                 aeropuertos: List[Aeropuerto]                // Lista de aeropuertos (no se usa aquí directamente)
                           ): (String, String) => List[Itinerario] = {    // Devuelve una función (origen, destino) => lista de itinerarios

    // ---------------------------------------
    // 1. Paralelizar exploración (Divide & Conquer)
    // ---------------------------------------
    def construirPar(                            // Función interna recursiva que construye itinerarios en paralelo
                                                 actual: String,                            // Aeropuerto actual
                                                 destino: String,                           // Aeropuerto destino
                                                 visitados: Set[String]                     // Conjunto de aeropuertos ya visitados
                    ): List[Itinerario] = {                      // Devuelve lista de itinerarios
      val umbral = 8                             // Umbral de tamaño para decidir si paralelizamos o no (más pequeño que antes)

      // Caso base: llegó al destino
      if (actual == destino)                     // Si el aeropuerto actual es el destino...
        List(Nil)                                // Devolvemos una lista de un itinerario vacío: ya llegamos
      else {                                     // Si aún no llegamos al destino...
        // vuelos salientes
        val salientes =                          // Calculamos los vuelos que salen desde `actual` a destinos no visitados
          for {
            v <- vuelos                          // Tomamos cada vuelo `v`
            if v.Org == actual                   // Filtro: deben salir del aeropuerto `actual`
            if !visitados(v.Dst)                 // Filtro: el destino del vuelo NO debe estar en `visitados`
          } yield v                              // Resultado: lista de vuelos que cumplen ambas condiciones

        // Sin vuelos, no hay itinerarios
        if (salientes.isEmpty) Nil               // Si no hay vuelos salientes, no existe itinerario desde aquí, devolvemos Nil
        else {

          def explorar(vs: List[Vuelo]): List[Itinerario] = { // Función interna que explora una lista de vuelos (paralelo/secuencial)
            val n = vs.length                     // Obtenemos cuántos vuelos hay en la lista `vs`

            // Secuencial si es pequeño
            if (n <= umbral) {                    // Si hay pocos vuelos...
              for {                               // Exploramos secuencialmente con for-comprehension
                v <- vs                           // Tomamos cada vuelo `v`
                resto <- construirPar(            // Llamamos recursivamente a `construirPar`
                  v.Dst,                          // Nuevo aeropuerto actual: destino del vuelo
                  destino,                        // Mantenemos el mismo destino final
                  visitados + v.Dst               // Marcamos el nuevo aeropuerto como visitado
                )
              } yield v :: resto                  // Anteponemos el vuelo al resto del itinerario
            } else {                              // Si hay muchos vuelos...
              // dividir en dos
              val (izq, der) = vs.splitAt(n / 2) // Dividimos la lista de vuelos en dos mitades

              val (resI, resD) = parallel(       // Llamamos a `parallel` para explorar cada mitad en paralelo
                explorar(izq),                   // Exploramos la mitad izquierda
                explorar(der)                    // Exploramos la mitad derecha
              )

              resI ++ resD                        // Juntamos los itinerarios devueltos por ambas mitades
            }
          }

          explorar(salientes)                     // Iniciamos la exploración con la lista de vuelos salientes
        }
      }
    }

    // ---------------------------------------
    // 2. Métrica combinada para escalas
    // ---------------------------------------
    def escalasMedida(it: Itinerario): Int =     // Función que calcula la "medida" de escalas de un itinerario
      if (it.isEmpty) 0                          // Si el itinerario está vacío, su medida es 0
      else (it.length - 1) +                     // Número de escalas "básicas": cantidad de vuelos - 1
        it.map(_.Esc).sum                        // Más la suma del campo `Esc` de cada vuelo

    // ---------------------------------------
    // 3. Función final pública
    // ---------------------------------------
    (origen: String, destino: String) => {       // Función final que recibe origen y destino
      val listaIts =                             // Calcula todos los itinerarios posibles en paralelo
        construirPar(origen, destino, Set(origen)) // Empieza desde `origen` con `origen` en el conjunto de visitados

      if (listaIts.isEmpty) Nil                  // Si no hay itinerarios, devolvemos Nil
      else {
        // Paralelización de datos
        val pares =                              // Creamos una lista de pares (itinerario, medida) pero en paralelo
          listaIts.par                           // Convertimos la lista de itinerarios a colección paralela
            .map(it => (it, escalasMedida(it)))  // Para cada itinerario, calculamos su medida y formamos un par
            .toList                              // Volvemos a lista secuencial

        // Tomar los 3 mejores
        pares.sortBy(_._2)                       // Ordenamos los pares por la medida (segundo elemento del par)
          .take(3)                               // Tomamos los 3 con menor medida
          .map(_._1)                             // Nos quedamos solo con el itinerario
      }
    }
  }

  /*
   * FUNCIÓN: itinerariosAirePar
   * -----------------------------------
   * ¿Qué hace?
   *  - Devuelve los 3 itinerarios que tienen MENOR TIEMPO EN AIRE (no total, solo vuelo).
   *
   * Parecido a `itinerariosTiempoPar`, pero:
   *  - La función `tiempoVuelo` se enfoca solo en los tiempos de vuelo.
   *
   * Pasos:
   *  1. Crea el mapa código aeropuerto -> offset GMT en minutos.
   *  2. Define funciones:
   *      - `minutosUTC` para convertir a minutos UTC.
   *      - `diferenciaMinutos` para calcular la diferencia entre eventos.
   *  3. Usa `eventos(it)` para transformar el itinerario en la secuencia de
   *     (código, hora, minuto) de salidas y llegadas.
   *  4. `tiempoVuelo(it)` suma diferencias (en minutos) entre pares de eventos.
   *  5. Usa `itinerariosPar` para generar todos los itinerarios.
   *  6. Usa `calcularTiemposParalelo` con `parallel` para asociar cada itinerario con su tiempo de vuelo.
   *  7. Ordena por tiempo de vuelo y devuelve los 3 mejores itinerarios.
   */

  def itinerariosAirePar(                        // Define la función `itinerariosAirePar`
                                                 vuelos: List[Vuelo],                         // Lista de vuelos
                                                 aeropuertos: List[Aeropuerto]                // Lista de aeropuertos
                        ): (String, String) => List[Itinerario] = {    // Devuelve función (origen, destino) => lista de itinerarios

    // --- Conversion de horas locales a minutos UTC ---

    // mapa, codigo aeropuerto -> offset en minutos
    val offsetPorCodigo: Map[String, Int] =      // Mapa de código de aeropuerto a offset de GMT en minutos
      aeropuertos
        .map(a => a.Cod -> ((a.GMT / 100) * 60)) // Convertimos el GMT (p.ej. -500) a horas y luego a minutos
        .toMap

    // Devuelve el offset del aeropuerto en minutos
    def offsetMinutos(cod: String): Int =        // Función para obtener el offset de un aeropuerto
      offsetPorCodigo.get(cod).fold(0)(offset => offset) // Si no está, devolvemos 0

    // Convierte una hora local (h:m) en "minutos UTC"
    def minutosUTC(                              // Convierte una hora local a minutos UTC
                                                 cod: String,                               // Código del aeropuerto
                                                 h: Int,                                    // Hora local
                                                 m: Int                                     // Minuto local
                  ): Int =
      h * 60 + m - offsetMinutos(cod)            // Minutos desde medianoche menos offset

    // Diferencia minima no negativa entre dos eventos
    def diferenciaMinutos(                       // Calcula diferencia de tiempos entre dos eventos
                                                 cod1: String, h1: Int, m1: Int,            // Primer evento
                                                 cod2: String, h2: Int, m2: Int             // Segundo evento
                         ): Int = {
      val t1 = minutosUTC(cod1, h1, m1)          // Evento 1 en minutos UTC
      val t2 = minutosUTC(cod2, h2, m2)          // Evento 2 en minutos UTC
      val d  = t2 - t1                           // Diferencia entre ellos
      if (d >= 0) d else d + 24 * 60             // Ajuste para saltar al día siguiente si es negativo
    }

    // --- Construcción de la secuencia de eventos (salidas y llegadas) de un itinerario ---

    type Evento = (String, Int, Int)             // Alias de tipo para eventos: (código, hora, minuto)

    def eventos(it: Itinerario): List[Evento] = it match { // Convierte un itinerario en su lista de eventos
      case Nil => Nil                            // Si no hay vuelos, no hay eventos
      case v0 :: vs =>                           // Tomamos el primer vuelo y el resto
        def construirEventos(                    // Función interna recursiva para construir los eventos
                                                 actual: Vuelo,                         // Vuelo actual
                                                 resto: List[Vuelo]                     // Resto de vuelos
                            ): List[Evento] =
          resto match {                          // Miramos la lista `resto`
            case Nil =>                          // Si no hay más vuelos...
              // Solo falta la llegada del ultimo vuelo
              List((actual.Dst, actual.HL, actual.ML)) // Agregamos la llegada del vuelo actual
            case siguiente :: cola =>            // Si hay un siguiente vuelo...
              // llegada del actual, salida del siguiente, y se sigue
              (actual.Dst, actual.HL, actual.ML) :: // Llegada del vuelo actual
                (siguiente.Org, siguiente.HS, siguiente.MS) :: // Salida del siguiente
                construirEventos(siguiente, cola) // Recursión con el siguiente y su cola
          }

        // Salida del primer vuelo seguida del resto de eventos
        (v0.Org, v0.HS, v0.MS) ::                // Agregamos la salida del primer vuelo
          construirEventos(v0, vs)               // Y luego los eventos del resto de vuelos
    }

    // --- Tiempo total de viaje (vuelos + esperas) de un itinerario --- (solo tiempo en aire)

    def tiempoVuelo(it: Itinerario): Int = {     // Función que calcula el "tiempo en aire" de un itinerario
      def sumarVuelo(evts: List[Evento]): Int = evts match { // Suma las diferencias entre eventos
        case Nil           => 0                  // Sin eventos, tiempo 0
        case _ :: Nil      => 0                  // Un solo evento, no hay intervalo
        case (c1,h1,m1) :: (c2,h2,m2) :: resto =>// Dos o más eventos
          // Solo sumar los tiempos de vuelo, es decir, la diferencia entre la llegada de un vuelo y la salida del siguiente
          diferenciaMinutos(c1, h1, m1, c2, h2, m2) + // Diferencia entre estos dos eventos
            sumarVuelo((c2, h2, m2) :: resto)    // Seguimos recursivamente con el resto
      }

      sumarVuelo(eventos(it))                    // Calculamos eventos del itinerario y sumamos tiempos de vuelo
    }

    // --- Paralelizar el cálculo de itinerarios ---

    val todosItinerariosPar =                    // Usamos itinerariosPar para generar todos los itinerarios posibles
      itinerariosPar(vuelos, aeropuertos)

    // Función final que devuelve los 3 itinerarios con menor tiempo en aire
    (cod1: String, cod2: String) => {            // Función final: recibe código origen y destino
      val todosItinerarios =                     // Genera todos los itinerarios entre esos códigos
        todosItinerariosPar(cod1, cod2)

      // Función para calcular tiempos en paralelo
      def calcularTiemposParalelo(               // Función que devuelve lista de pares (itinerario, tiempoVuelo)
                                                 its: List[Itinerario]
                                 ): List[(Itinerario, Int)] = {
        its match {
          case Nil => Nil                        // Si no hay itinerarios, devolvemos Nil
          case _ =>
            val n = its.length                   // Número de itinerarios
            val umbral = 10                      // Umbral de tamaño para decidir paralelizar

            if (n <= umbral) {                   // Si hay pocos itinerarios
              // Caso base: calcular secuencialmente
              its.map(it => (it, tiempoVuelo(it))) // Asociamos cada itinerario con su tiempo de vuelo
            } else {                             // Si hay muchos itinerarios
              // Dividir y conquistar en paralelo
              val (left, right) = its.splitAt(n / 2) // Separamos en dos mitades
              val (resLeft, resRight) = parallel(    // Calculamos en paralelo los tiempos de cada mitad
                calcularTiemposParalelo(left),
                calcularTiemposParalelo(right)
              )
              resLeft ++ resRight               // Juntamos resultados de ambas mitades
            }
        }
      }

      todosItinerarios match {                   // Revisamos si hay itinerarios
        case Nil => Nil                          // Si no hay, devolvemos Nil
        case _ =>
          calcularTiemposParalelo(todosItinerarios) // Calculamos tiempos de vuelo en paralelo
            .sortBy(_._2)                       // Ordenamos por tiempo de vuelo (segundo elemento del par)
            .take(3)                            // Tomamos los 3 más rápidos en aire
            .map(_._1)                          // Devolvemos solo el itinerario, no el tiempo
      }
    }
  }

  /*
   * FUNCIÓN: itinerarioSalidaPar
   * -----------------------------------
   * ¿Qué hace?
   *  - Dado:
   *      - origen y destino
   *      - hora de una cita (hCita, mCita)
   *    devuelve UN SOLO itinerario:
   *      - que llegue ANTES o IGUAL a la hora de la cita si es posible,
   *      - si no hay ninguno que llegue a tiempo, elige alguno que llega después.
   *    En ambos casos, el criterio es:
   *      - elegir siempre el itinerario con la HORA DE SALIDA MÁS TARDE posible
   *        (priorizando los que llegan el mismo día).
   *
   * Ojo:
   *  - Esta función reusa `itinerarios` (la VERSIÓN SECUENCIAL) para generar candidatos.
   *  - Luego usa colecciones paralelas (`.par`) solo para filtrar y buscar máximo.
   *
   * Pasos:
   *  1. Define `horaEnMinutos(h, m)` para convertir una hora a minutos desde medianoche.
   *  2. Define `horaSalida(it)` como la hora del primer vuelo del itinerario.
   *  3. Define `horaLlegada(it)` como la hora de llegada del último vuelo.
   *  4. Usa `itinerarios(vuelos, aeropuertos)` para generar todos los itinerarios posibles.
   *  5. Para una cita dada (hCita, mCita):
   *        - convierte la cita a minutos (`citaMinutos`).
   *        - separa en paralelo:
   *            - `mismosDiaPar`: itinerarios que llegan ANTES O IGUAL que la cita.
   *            - `diaAnteriorPar`: itinerarios que llegan DESPUÉS de la cita.
   *  6. Si hay itinerarios en `mismosDiaPar`, devuelve el que tenga mayor hora de salida.
   *  7. Si no hay itinerarios que lleguen a tiempo, usa `diaAnteriorPar` y devuelve el que
   *     tenga mayor hora de salida.
   */

  def itinerarioSalidaPar(                       // Define la función `itinerarioSalidaPar`
                                                 vuelos: List[Vuelo],                         // Lista de vuelos
                                                 aeropuertos: List[Aeropuerto]                // Lista de aeropuertos
                         ): (String, String, Int, Int) => Itinerario = { // Devuelve una función (origen, destino, hCita, mCita) => un itinerario

    // Convierte hora local a minutos desde medianoche
    def horaEnMinutos(h: Int, m: Int): Int =     // Convierte (hora, minuto) a un solo número en minutos
      h * 60 + m                                 // Multiplica la hora por 60 y le suma los minutos

    def horaSalida(it: Itinerario): Int = it match { // Devuelve la hora de salida del itinerario
      case Nil => 0                              // Si el itinerario está vacío, devolvemos 0 (caso raro)
      case vuelo :: _ =>                         // Si hay al menos un vuelo, el primero marca la salida
        horaEnMinutos(vuelo.HS, vuelo.MS)        // Convertimos la hora de salida del primer vuelo a minutos
    }

    def horaLlegada(it: Itinerario): Int = it match { // Devuelve la hora de llegada del itinerario
      case Nil => 0                              // Si está vacío, devolvemos 0
      case _ =>
        horaEnMinutos(                           // Convertimos la hora de llegada a minutos
          it.last.HL,                            // Hora de llegada del último vuelo del itinerario
          it.last.ML                             // Minutos de llegada del último vuelo
        )
    }

    val todosItinerarios =                       // Obtenemos la FUNCIÓN secuencial `itinerarios`
      itinerarios(vuelos, aeropuertos)           // No es paralela: es la versión del package Itinerarios

    (cod1: String, cod2: String, hCita: Int, mCita: Int) => { // Función final que recibe origen, destino y hora de cita
      val candidatos =                           // Calculamos todos los itinerarios posibles entre origen y destino
        todosItinerarios(cod1, cod2)

      candidatos match {
        case Nil => Nil                          // Si no hay ningún itinerario, devolvemos Nil
        case _ =>
          val citaMinutos =                      // Convertimos la hora de la cita a minutos
            horaEnMinutos(hCita, mCita)

          // Filtrar los itinerarios que llegan el mismo día y el día anterior usando colecciones paralelas
          val mismosDiaPar =                     // Colección paralela con itinerarios que llegan ANTES O IGUAL a la cita
            candidatos.par                       // Convertimos `candidatos` en una colección paralela
              .filter { it =>                    // Filtramos
                horaLlegada(it) <= citaMinutos   // Condición: hora de llegada menor o igual a la hora de la cita
              }

          val diaAnteriorPar =                   // Colección paralela con itinerarios que llegan DESPUÉS de la cita
            candidatos.par                       // De nuevo convertimos a paralelo
              .filter { it =>
                horaLlegada(it) > citaMinutos    // Condición: hora de llegada mayor a la hora de la cita
              }

          // SIEMPRE elegir el de salida más tarde, priorizando mismo día
          if (mismosDiaPar.isEmpty) {            // Si NO hay itinerarios que lleguen a tiempo...
            // Todos son del día anterior, elegir el de salida más tarde
            diaAnteriorPar.maxBy(horaSalida)     // Elegimos de `diaAnteriorPar` el itinerario con salida más tardía
          } else {
            // Hay del mismo día, elegir el de salida más tarde
            mismosDiaPar.maxBy(horaSalida)       // Elegimos de `mismosDiaPar` el itinerario con salida más tardía
          }
      }
    }
  }

}                                                 // Cierre del package object ItinerariosPar
