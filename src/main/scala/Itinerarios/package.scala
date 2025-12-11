import datos._  // Importa TODO lo que está definido en el paquete `datos` (tipos como Vuelo, Aeropuerto, Itinerario, etc.), para poder usarlos sin escribir `datos.` delante

package object Itinerarios {  // Define un "package object" llamado `Itinerarios`, que es como un contenedor de funciones y definiciones que estarán disponibles en el paquete

  // ----------------------------------------------
  // 1. Cálculo de todos los itinerarios
  // ----------------------------------------------

  /*
    FUNCIÓN: itinerarios
    ---------------------------------------------------------
    ¿QUÉ RECIBE?
      - `vuelos`: una lista (List) de objetos `Vuelo`.
      - `aeropuertos`: una lista (List) de objetos `Aeropuerto`.
        (En ESTA función, la lista de aeropuertos realmente no se usa, pero se pasa por consistencia con las demás funciones.)

    ¿QUÉ DEVUELVE?
      - Devuelve una FUNCIÓN.
      - Esa función toma dos `String`:
          * `origen` (código de aeropuerto de salida, p.ej. "CLO")
          * `destino` (código de aeropuerto de llegada, p.ej. "MAD")
        y devuelve una `List[Itinerario]`.

      - Un `Itinerario` es una lista de `Vuelo`. O sea: List[Vuelo].

      En resumen:
        itinerarios(...)  ---->  (origen, destino) => List[Itinerario]

    IDEA PRINCIPAL:
      - Encontrar TODOS los caminos posibles (sin repetir aeropuertos) desde un aeropuerto `origen` hasta un aeropuerto `destino`.
      - Cada camino es una secuencia de vuelos conectados (el destino de uno es el origen del siguiente).
      - Usamos una función interna recursiva llamada `construir`.

    PASO A PASO (dentro de `construir`):
      1. Parámetros de `construir`:
         - `actual`: aeropuerto donde estoy ahora.
         - `destino`: aeropuerto al que quiero llegar.
         - `visitados`: conjunto (Set) de códigos de aeropuertos que YA se han visitado, para evitar ciclos.

      2. CASO BASE:
         - Si `actual == destino`:
             -> ya llegué al destino.
             -> devuelvo `List(Nil)`.
             ¿Por qué `List(Nil)`?
             * `Nil` es una lista vacía de vuelos: representa "no hay más vuelos por añadir".
             * `List(Nil)` significa "una lista que contiene un itinerario vacío".
             * En la recursión, se irán agregando vuelos delante de este `Nil`.

      3. CASO RECURSIVO:
         - Si `actual != destino`:
             a) Busco todos los vuelos que salen del aeropuerto `actual` y que NO van a un aeropuerto ya visitado.
                Eso se guarda en `salientes`.

             b) Para cada vuelo `vuelo` en `salientes`:
                  - Llamo recursivamente a `construir`, empezando desde `vuelo.Dst`
                    (el aeropuerto de destino de ese vuelo),
                    con el mismo `destino`,
                    y agregando `vuelo.Dst` al conjunto `visitados`.

                  - La llamada recursiva me devuelve itinerarios (listas de vuelos) que van desde `vuelo.Dst` hasta `destino`.

                  - A cada uno de esos itinerarios le agrego el vuelo actual al principio con `vuelo :: resto`.

             c) El `for` de comprensión:
                  for {
                    vuelo <- salientes
                    resto <- construir(...)
                  } yield vuelo :: resto

                recoge todos esos itinerarios `vuelo :: resto` y devuelve una lista de itinerarios.

      4. FUNCIÓN FINAL:
         - `itinerarios(vuelos, aeropuertos)` devuelve:
             (origen: String, destino: String) =>
               construir(origen, destino, Set(origen))

           Es decir, empieza la búsqueda desde el `origen`, con `visitados` = conjunto que contiene solo el `origen`.
  */

  def itinerarios(                                // Define una función llamada `itinerarios`
                                                  vuelos: List[Vuelo],                          // Parámetro 1: lista de vuelos disponibles en el sistema
                                                  aeropuertos: List[Aeropuerto]                 // Parámetro 2: lista de aeropuertos (NO se usa en esta función, pero se recibe igual)
                 ): (String, String) => List[Itinerario] = {     // Tipo de retorno: una función que recibe (String, String) y devuelve una lista de itinerarios

    def construir(                                // Define una función interna llamada `construir`
                                                  actual: String,                             // Parámetro: `actual` es el código del aeropuerto donde estoy ahora
                                                  destino: String,                            // Parámetro: `destino` es el código del aeropuerto al que quiero llegar
                                                  visitados: Set[String]                      // Parámetro: `visitados` es un conjunto de aeropuertos que ya visité (para evitar ciclos)
                 ): List[Itinerario] = {                       // Esta función devuelve una lista de itinerarios (cada itinerario es una lista de vuelos)

      // Caso base
      if (actual == destino)                      // Si el aeropuerto actual es IGUAL al aeropuerto destino...
        List(Nil)                                 // Entonces ya llegué: devuelvo una lista que contiene un itinerario vacío (`Nil`)
      else {                                      // Si NO he llegado todavía al destino...
        // Vuelos que salen del aeropuerto actual
        val salientes =                           // Defino la variable `salientes` que guardará los vuelos que salen del aeropuerto actual
          for {                                   // Comienzo un for-comprehension (azúcar sintáctico de Scala para map/flatMap/filter)
            v <- vuelos                           // Recorro cada vuelo `v` de la lista de vuelos
            if v.Org == actual                    // Filtro: solo quiero los vuelos cuyo aeropuerto de origen (`Org`) sea el aeropuerto actual
            if !visitados(v.Dst)                  // Filtro: solo quiero vuelos cuyo aeropuerto de destino (`Dst`) NO esté en el conjunto `visitados`
          } yield v                               // El resultado del for es una lista de vuelos `v` que cumplen los filtros

        // Expansión recursiva
        for {                                     // Otro for-comprehension para construir los itinerarios recursivamente
          vuelo <- salientes                      // Para cada `vuelo` en la lista de vuelos salientes
          resto <- construir(                     // Llamo recursivamente a `construir` para extender el itinerario desde el destino de este vuelo
            vuelo.Dst,                            // Nuevo aeropuerto actual: el destino del vuelo actual (`vuelo.Dst`)
            destino,                              // El aeropuerto destino final sigue siendo el mismo
            visitados + vuelo.Dst                 // Nuevo conjunto de visitados: el conjunto anterior + el nuevo aeropuerto destino
          )
        } yield vuelo :: resto                    // Por cada `resto` de itinerario devuelto, agrego al inicio el `vuelo` actual con `::`
      }
    }

    // Función final
    (origen: String, destino: String) =>          // Defino y devuelvo una función anónima que recibe `origen` y `destino`
      construir(origen, destino, Set(origen))     // Llama a `construir` empezando en `origen`, con `visitados` = conjunto que contiene solo `origen`

  }

  /*
    FUNCIÓN: itinerariosTiempo
    ---------------------------------------------------------
    ¿QUÉ HACE?
      - Usa la función `itinerarios` (la que acabamos de ver) para generar TODOS los itinerarios posibles
        entre dos aeropuertos.
      - Luego calcula el TIEMPO TOTAL DE VIAJE (en minutos) de cada itinerario.
        Ese tiempo incluye:
          * tiempo volando,
          * tiempo esperando entre un vuelo y otro.
      - Al final, devuelve SOLO los 3 itinerarios más rápidos (los de menor tiempo total).

    IDEAS IMPORTANTES:
      1. HAY ZONAS HORARIAS:
         - Cada aeropuerto tiene un atributo `GMT` (ej: -500 para GMT-5).
         - Convertimos eso a un "offset" de minutos.
         - Luego convertimos horas locales (h, m) en minutos "UTC".

      2. CONSTRUCCIÓN DE EVENTOS:
         - De un itinerario (lista de vuelos), construimos una lista de "eventos":
             (códigoAeropuerto, hora, minuto)
           en el orden de ocurrencia: salida, llegada, salida, llegada, etc.

      3. TIEMPO TOTAL:
         - Se recorre la lista de eventos de dos en dos:
             (evento1, evento2)
           se calcula cuánto tiempo pasa entre esos dos eventos (usando zonas horarias).
         - Se suman todas esas diferencias.

      4. ORDENAMIENTO:
         - Calculamos el tiempo total de cada itinerario.
         - Ordenamos la lista de itinerarios por ese tiempo.
         - Nos quedamos con los primeros 3.

    ENTRADA:
      - `vuelos`: lista de vuelos.
      - `aeropuertos`: lista de aeropuertos.

    SALIDA:
      - Función: (cod1: String, cod2: String) => List[Itinerario]
        * `cod1`: código de aeropuerto origen.
        * `cod2`: código de aeropuerto destino.
        * Devuelve: los 3 mejores itinerarios (o menos, si hay menos de 3).
  */

  def itinerariosTiempo(                          // Define la función `itinerariosTiempo`
                                                  vuelos: List[Vuelo],                          // Parámetro: lista de vuelos disponibles
                                                  aeropuertos: List[Aeropuerto]                 // Parámetro: lista de aeropuertos disponibles
                       ): (String, String) => List[Itinerario] = {     // Devuelve una función que, dado (origen, destino), devuelve lista de itinerarios

    // --- Conversion de horas locales a minutos UTC ---

    // mapa, codigo aeropuerto -> offset en minutos
    val offsetPorCodigo: Map[String, Int] =       // Define un mapa que relaciona código de aeropuerto con su offset horario en minutos
      aeropuertos                                 // Toma la lista de aeropuertos
        .map(a => a.Cod -> ((a.GMT / 100) * 60))  // Para cada aeropuerto `a`, crea un par (a.Cod, offsetEnMinutos) donde offset = (GMT/100)*60
        .toMap                                    // Convierte la lista de pares en un Map (clave: código, valor: offset)

    // Devuelve el offset del aeropuerto en minutos
    def offsetMinutos(cod: String): Int =         // Función que recibe un código de aeropuerto y devuelve su offset en minutos
      offsetPorCodigo                             // Usa el mapa `offsetPorCodigo`
        .get(cod)                                 // Intenta obtener el offset asociado al código `cod`
        .fold(0)(offset => offset)                // Si no existe el código, devuelve 0; si existe, devuelve el offset encontrado

    // Convierte una hora local (h:m) en "minutos UTC"
    def minutosUTC(                               // Función que convierte hora local a minutos UTC
                                                  cod: String,                                // Código del aeropuerto (para saber qué offset usar)
                                                  h: Int,                                     // Hora local (entero)
                                                  m: Int                                      // Minutos locales (entero)
                  ): Int =                                      // Devuelve el número de minutos UTC
      h * 60 + m - offsetMinutos(cod)             // Convierte h:m a minutos desde medianoche y le resta el offset del aeropuerto

    // Diferencia minima no negativa entre dos eventos
    def diferenciaMinutos(                        // Función que calcula la diferencia de tiempo entre dos eventos
                                                  cod1: String, h1: Int, m1: Int,             // Datos del primer evento: aeropuerto1, hora1, minuto1
                                                  cod2: String, h2: Int, m2: Int              // Datos del segundo evento: aeropuerto2, hora2, minuto2
                         ): Int = {                                    // Devuelve la diferencia en minutos (no negativa, ajustando día si toca)
      val t1 = minutosUTC(cod1, h1, m1)           // Calcula los minutos UTC del primer evento
      val t2 = minutosUTC(cod2, h2, m2)           // Calcula los minutos UTC del segundo evento
      val d  = t2 - t1                            // Calcula la diferencia de tiempo: segundo - primero
      if (d >= 0) d else d + 24 * 60              // Si la diferencia es negativa, suma 24 horas (en minutos) para pasar al día siguiente
    }

    // --- Construcción de la secuencia de eventos (salidas y llegadas) de un itinerario ---

    type Evento = (String, Int, Int)              // Define un alias de tipo: `Evento` es (códigoAeropuerto, hora, minuto) en hora local

    def eventos(it: Itinerario): List[Evento] = it match { // Función que convierte un itinerario en una lista de eventos
      case Nil => Nil                             // Si el itinerario está vacío, no hay eventos -> devuelve lista vacía
      case v0 :: vs =>                            // Si el itinerario tiene al menos un vuelo: v0 es el primero, vs es el resto de vuelos
        def construirEventos(                     // Función interna que construye los eventos para el resto de vuelos
                                                  actual: Vuelo,                          // `actual`: vuelo actual que estoy procesando
                                                  resto: List[Vuelo]                      // `resto`: lista de vuelos que quedan por procesar
                            ): List[Evento] = resto match {           // Hago pattern matching sobre la lista `resto`
          case Nil =>                             // Si ya no quedan más vuelos...
            List((actual.Dst, actual.HL, actual.ML)) // Devuelvo una lista con UN solo evento: la llegada del vuelo actual
          case siguiente :: cola =>               // Si todavía queda al menos un vuelo `siguiente`...
            (actual.Dst, actual.HL, actual.ML) :: // Agrego como evento la llegada del vuelo `actual`
              (siguiente.Org, siguiente.HS, siguiente.MS) :: // Agrego como evento la salida del vuelo `siguiente`
              construirEventos(siguiente, cola)   // Llamo recursivamente para seguir con el siguiente vuelo y el resto
        }
        (v0.Org, v0.HS, v0.MS) ::                 // Primer evento: la salida del primer vuelo v0
          construirEventos(v0, vs)                // Después, construyo todos los demás eventos llamando a `construirEventos`
    }

    // --- Tiempo total de viaje (vuelos + esperas) de un itinerario ---

    def tiempoTotal(it: Itinerario): Int = {      // Función que calcula el tiempo total (en minutos) de un itinerario
      def sumar(evts: List[Evento]): Int = evts match { // Función interna recursiva que suma diferencias entre eventos
        case Nil           => 0                   // Si la lista de eventos está vacía, el tiempo total es 0
        case _ :: Nil      => 0                   // Si solo hay un evento, no hay intervalo que sumar, tiempo 0
        case (c1,h1,m1) :: (c2,h2,m2) :: resto => // Si hay al menos dos eventos:
          diferenciaMinutos(c1,h1,m1,c2,h2,m2) +  // Calcula la diferencia en minutos entre el primer evento y el segundo
            sumar((c2,h2,m2) :: resto)           // Suma esa diferencia con la suma recursiva del resto de eventos (a partir del segundo)
      }

      sumar(eventos(it))                          // Llama a `eventos` para obtener la lista de eventos del itinerario y luego la suma con `sumar`
    }

    val todosItinerarios =                        // Define una variable que contiene la función generada por `itinerarios`
      itinerarios(vuelos, aeropuertos)            // Llama a `itinerarios` con la lista de vuelos y aeropuertos, obtiene función (origen,destino)=>itinerarios

    // Funcion final
    (cod1: String, cod2: String) =>               // Devuelve una función anónima que recibe `cod1` (origen) y `cod2` (destino)
      todosItinerarios(cod1, cod2)               // Llama a la función `todosItinerarios` para obtener TODOS los itinerarios `cod1 -> cod2`
        .sortBy(tiempoTotal)                     // Ordena la lista de itinerarios usando `tiempoTotal` como clave (de menor a mayor tiempo)
        .take(3)                                 // Se queda solo con los primeros 3 itinerarios de la lista (los más rápidos)

  }

  /*
    FUNCIÓN: itinerariosEscalas
    ---------------------------------------------------------
    ¿QUÉ HACE?
      - Usa la función base `itinerarios` para obtener TODOS los itinerarios entre dos aeropuertos.
      - Después los ordena según una MÉTRICA que mide "cuántas escalas hace" el itinerario.

    MÉTRICA DE ESCALAS:
      - Si el itinerario está vacío: 0.
      - Si el itinerario tiene vuelos:
          (it.length - 1) + it.map(_.Esc).sum

        Donde:
          - `it.length - 1` ≈ número de escalas "básicas" (si hay 2 vuelos, hay 1 escala intermedia, etc.).
          - `it.map(_.Esc).sum` suma el campo `Esc` de cada vuelo (una penalización extra que depende de cada vuelo).

    ENTRADA:
      - `vuelos`: lista de vuelos.
      - `aeropuertos`: lista de aeropuertos (no se usa directamente aquí, pero se pasa a `itinerarios`).

    SALIDA:
      - Función: (origen: String, destino: String) => List[Itinerario]
        Devuelve los 3 itinerarios con MENOR número de escalas (según la métrica).
  */

  def itinerariosEscalas(                         // Define la función `itinerariosEscalas`
                                                  vuelos: List[Vuelo],                         // Lista de vuelos disponibles
                                                  aeropuertos: List[Aeropuerto]                // Lista de aeropuertos disponibles
                        ): (String, String) => List[Itinerario] = {    // Devuelve una función que, dado (origen, destino), devuelve lista de itinerarios

    val todosIts =                               // Define `todosIts` como la función que genera todos los itinerarios
      itinerarios(vuelos, aeropuertos)           // Llama a la función `itinerarios` y guarda la función resultante

    (origen: String, destino: String) => {       // Devuelve una función anónima que recibe `origen` y `destino`
      val lista =                                // Define la variable `lista` con todos los itinerarios desde `origen` hasta `destino`
        todosIts(origen, destino)                // Llama a `todosIts` con `origen` y `destino` para obtener todos los itinerarios

      def escalas(it: Itinerario): Int =         // Define una función interna `escalas` que calcula la métrica de escalas de un itinerario
        if (it.isEmpty) 0                        // Si el itinerario está vacío, la cantidad de escalas es 0
        else (it.length - 1) +                   // Si no está vacío: empieza con (número de vuelos - 1), que representa escalas "básicas"
          it.map(_.Esc).sum                      // Le suma la suma de los campos `Esc` de cada vuelo (penalización extra)

      lista                                      // Toma la lista de todos los itinerarios de origen a destino
        .sortBy(escalas)                         // Los ordena según la función `escalas` (de menor a mayor cantidad de escalas)
        .take(3)                                 // Se queda con los 3 itinerarios que tienen menos escalas (según la métrica)
    }
  }

  /*
    FUNCIÓN: itinerariosAire
    ---------------------------------------------------------
    ¿QUÉ HACE?
      - Similar a `itinerariosTiempo`, pero se enfoca en el TIEMPO EN AIRE, es decir,
        el tiempo que se pasa volando entre aeropuertos.

      OJO: tal como está el código, la suma se hace igual que en `itinerariosTiempo`,
      pero el comentario aclara la intención: sumar solo tiempos de vuelo.

    PASOS GENERALES:
      1. Crear mapa `offsetPorCodigo` para convertir GMT de cada aeropuerto a minutos.
      2. Definir funciones:
         - `offsetMinutos`  -> devuelve offset en minutos para un aeropuerto.
         - `minutosUTC`     -> convierte una hora local a minutos UTC.
         - `diferenciaMinutos` -> diferencia no negativa entre dos eventos.
      3. Construir lista de eventos para cada itinerario:
         (código, hora, minuto).
      4. Definir `tiempoVuelo(it)` para sumar las diferencias entre eventos que representan vuelo.
      5. Usar `itinerarios` para obtener todos los itinerarios entre dos aeropuertos.
      6. Ordenar esos itinerarios por `tiempoVuelo`.
      7. Devolver los 3 itinerarios con menor tiempo en aire.

    ENTRADA:
      - `vuelos`: lista de vuelos.
      - `aeropuertos`: lista de aeropuertos.

    SALIDA:
      - Función: (cod1: String, cod2: String) => List[Itinerario]
        Devuelve los 3 itinerarios que minimizan el tiempo en aire.
  */

  def itinerariosAire(                           // Define la función `itinerariosAire`
                                                 vuelos: List[Vuelo],                         // Lista de vuelos
                                                 aeropuertos: List[Aeropuerto]                // Lista de aeropuertos
                     ): (String, String) => List[Itinerario] = {    // Devuelve una función que, dado (origen, destino), devuelve lista de itinerarios

    // Conversion de horas locales a minutos UTC

    // mapa, codigo aeropuerto - offset en minutos
    val offsetPorCodigo: Map[String, Int] =      // Mapa que relaciona código de aeropuerto con su offset horario en minutos
      aeropuertos
        .map(a => a.Cod -> ((a.GMT / 100) * 60)) // Convierte el campo `GMT` del aeropuerto `a` a minutos y lo asocia a `a.Cod`
        .toMap                                   // Convierte la lista de pares en un Map

    // Devuelve el offset del aeropuerto en minutos
    def offsetMinutos(cod: String): Int =        // Función que recibe un código de aeropuerto y devuelve su offset horario en minutos
      offsetPorCodigo.get(cod).fold(0)(offset => offset) // Si el código no existe en el mapa, devuelve 0; si sí, devuelve el offset asociado

    // Convierte una hora local (h:m) en "minutos UTC"
    def minutosUTC(                              // Función que convierte hora local a minutos UTC
                                                 cod: String,                               // Código de aeropuerto
                                                 h: Int,                                    // Hora local
                                                 m: Int                                     // Minutos locales
                  ): Int =
      h * 60 + m - offsetMinutos(cod)            // Pasa h:m a minutos desde medianoche y resta el offset horario del aeropuerto

    // Diferencia minima no negativa entre dos eventos
    def diferenciaMinutos(                       // Función que calcula la diferencia de tiempo (minutos) entre 2 eventos
                                                 cod1: String, h1: Int, m1: Int,            // Datos del primer evento
                                                 cod2: String, h2: Int, m2: Int             // Datos del segundo evento
                         ): Int = {
      val t1 = minutosUTC(cod1, h1, m1)          // Convierte el primer evento a minutos UTC
      val t2 = minutosUTC(cod2, h2, m2)          // Convierte el segundo evento a minutos UTC
      val d  = t2 - t1                           // Calcula diferencia en minutos: segundo - primero
      if (d >= 0) d else d + 24 * 60             // Si la diferencia es negativa, suma un día en minutos (24*60) para ajustar
    }

    // Construcción de la secuencia de eventos (salidas y llegadas) de un itinerario

    type Evento = (String, Int, Int)             // Alias de tipo: Evento = (códigoAeropuerto, hora, minuto)

    def eventos(it: Itinerario): List[Evento] = it match { // Función que transforma un itinerario en lista de eventos
      case Nil => Nil                            // Si el itinerario está vacío, no hay eventos -> devuelve lista vacía
      case v0 :: vs =>                           // Si el itinerario tiene al menos un vuelo: `v0` es el primero, `vs` es el resto
        def construirEventos(                    // Función interna que construye eventos a partir de los vuelos restantes
                                                 actual: Vuelo,                         // Vuelo actual que se está procesando
                                                 resto: List[Vuelo]                     // Lista de vuelos que quedan por procesar
                            ): List[Evento] =
          resto match {                          // Pattern matching con la lista restante
            case Nil =>                          // Caso en el que ya no hay más vuelos
              // Solo falta la llegada del ultimo vuelo
              List((actual.Dst, actual.HL, actual.ML)) // Devuelve lista con un único evento: llegada del vuelo actual
            case siguiente :: cola =>            // Si hay un vuelo `siguiente` y una lista `cola` restante
              // llegada del actual, salida del siguiente, y se sigue
              (actual.Dst, actual.HL, actual.ML) :: // Agrega la llegada del vuelo actual
                (siguiente.Org, siguiente.HS, siguiente.MS) :: // Agrega la salida del siguiente vuelo
                construirEventos(siguiente, cola) // Llama recursivamente para procesar el siguiente vuelo y la cola
          }

        // Salida del primer vuelo seguida del resto de eventos
        (v0.Org, v0.HS, v0.MS) ::                // Primer evento: salida del primer vuelo v0
          construirEventos(v0, vs)               // Luego, los eventos generados a partir de `v0` y el resto `vs`
    }

    // Tiempo total de viaje (vuelos + esperas) de un itinerario

    def tiempoVuelo(it: Itinerario): Int = {     // Función que calcula el "tiempo de vuelo" (según la lógica de eventos)
      def sumarVuelo(evts: List[Evento]): Int = evts match { // Función interna que suma diferencias entre eventos
        case Nil           => 0                  // Si no hay eventos, el tiempo es 0
        case _ :: Nil      => 0                  // Si hay un solo evento, no hay intervalo de tiempo entre eventos, tiempo 0
        case (c1,h1,m1) :: (c2,h2,m2) :: resto =>// Si hay al menos dos eventos
          // Solo sumar los tiempos de vuelo, es decir, la diferencia entre la llegada de un vuelo y la salida del siguiente
          diferenciaMinutos(c1, h1, m1, c2, h2, m2) + // Calcula diferencia entre dos eventos consecutivos
            sumarVuelo((c2, h2, m2) :: resto)    // Llama recursivamente con la lista a partir del segundo evento
      }

      sumarVuelo(eventos(it))                    // Genera la lista de eventos del itinerario y la pasa a `sumarVuelo` para obtener el tiempo total
    }


    // Utilizando la función itinerarios base para obtener todos los itinerarios

    val todosItinerarios =                       // Variable que guarda la función generada por `itinerarios`
      itinerarios(vuelos, aeropuertos)           // Llama a `itinerarios` con los vuelos y aeropuertos

    // Función final que obtiene los tres itinerarios con el tiempo más corto en aire
    (cod1: String, cod2: String) =>              // Devuelve una función anónima que recibe `cod1` (origen) y `cod2` (destino)
      todosItinerarios(cod1, cod2)              // Obtiene TODOS los itinerarios que van de `cod1` a `cod2`
        .sortBy(tiempoVuelo)                    // Ordena esos itinerarios por el tiempo de vuelo (de menor a mayor)
        .take(3)                                // Toma los primeros 3 (o menos, si hay menos de 3 itinerarios)

  }

  /*
    FUNCIÓN: itinerarioSalida
    ---------------------------------------------------------
    ¿QUÉ HACE?
      - Dado:
          * una ciudad de origen (`cod1`),
          * una ciudad de destino (`cod2`),
          * una hora de cita (`hCita`, `mCita`),
        elige UN solo itinerario.

      CRITERIO DE ELECCIÓN:
        1. Se calculan todos los itinerarios posibles entre `cod1` y `cod2` usando `itinerarios`.
        2. Se divide la lista en dos grupos:
           - `mismoDia`: itinerarios cuya HORA DE LLEGADA es menor o igual a la hora de la cita.
           - `diaAnterior`: itinerarios cuya HORA DE LLEGADA es mayor que la hora de la cita.
        3. Si hay itinerarios en `mismoDia`:
             -> se elige el que tenga la HORA DE SALIDA MÁS TARDE (maxBy(horaSalida)).
           Si NO hay itinerarios en `mismoDia` (lista vacía):
             -> se asume que entonces "todos son del día anterior" (en el sentido de que no llegan a tiempo),
                y se elige el itinerario en `diaAnterior` con la HORA DE SALIDA MÁS TARDE.

      ENTRADA:
        - `vuelos`: lista de vuelos.
        - `aeropuertos`: lista de aeropuertos.

      SALIDA:
        - Función: (cod1: String, cod2: String, hCita: Int, mCita: Int) => Itinerario
          Devuelve UN (1) itinerario, el elegido según el criterio descrito.
  */

  def itinerarioSalida(                          // Define la función `itinerarioSalida`
                                                 vuelos: List[Vuelo],                         // Lista de vuelos
                                                 aeropuertos: List[Aeropuerto]                // Lista de aeropuertos
                      ): (String, String, Int, Int) => Itinerario = { // Devuelve una función: (origen, destino, hCita, mCita) => un itinerario

    // Convierte hora local a minutos desde medianoche
    def horaEnMinutos(h: Int, m: Int): Int =     // Función que convierte una hora y minuto a un único número en minutos
      h * 60 + m                                 // Multiplica las horas por 60 y suma los minutos

    def horaSalida(it: Itinerario): Int = it match { // Función que calcula la hora de salida (en minutos) de un itinerario
      case Nil => 0                              // Si el itinerario está vacío, devuelve 0 como valor por defecto
      case vuelo :: _ =>                         // Si el itinerario tiene al menos un vuelo...
        horaEnMinutos(vuelo.HS, vuelo.MS)        // Toma la hora de salida del primer vuelo y la convierte a minutos
    }

    def horaLlegada(it: Itinerario): Int = it match { // Función que calcula la hora de llegada (en minutos) de un itinerario
      case Nil => 0                              // Si el itinerario está vacío, devuelve 0
      case _ =>                                  // Si el itinerario NO está vacío...
        horaEnMinutos(                           // Convierte a minutos la hora de llegada del último vuelo
          it.last.HL,                            // Hora de llegada (HL) del último vuelo del itinerario
          it.last.ML                             // Minutos de llegada (ML) del último vuelo del itinerario
        )
    }

    val todosItinerarios =                       // Variable que guarda la función generada por `itinerarios`
      itinerarios(vuelos, aeropuertos)           // Llama a `itinerarios` con la lista de vuelos y aeropuertos

    (cod1: String, cod2: String, hCita: Int, mCita: Int) => { // Función anónima final que recibe origen, destino y hora de cita

      val candidatos =                           // Variable `candidatos` contendrá TODOS los itinerarios entre `cod1` y `cod2`
        todosItinerarios(cod1, cod2)             // Llamada a la función `todosItinerarios` con `cod1` y `cod2`

      candidatos match {                         // Pattern matching sobre la lista de candidatos
        case Nil => Nil                          // Si NO hay ningún itinerario (lista vacía), devuelve `Nil` (itinerario vacío)

        case _ =>                                // Si SÍ hay al menos un itinerario...
          val citaMinutos =                      // Calcula la hora de la cita en minutos desde medianoche
            horaEnMinutos(hCita, mCita)          // Usa `horaEnMinutos` con `hCita` y `mCita`

          // Filtrar los itinerarios que llegan el mismo día y el día anterior
          val mismoDia =                         // Variable `mismoDia`: itinerarios que llegan antes o en la hora de la cita
            candidatos.filter { it =>            // Filtra la lista de candidatos
              horaLlegada(it) <= citaMinutos     // Condición: la hora de llegada del itinerario es <= hora de la cita
            }

          val diaAnterior =                      // Variable `diaAnterior`: itinerarios que llegan después de la hora de la cita
            candidatos.filter { it =>            // Filtra la lista de candidatos
              horaLlegada(it) > citaMinutos      // Condición: la hora de llegada del itinerario es > hora de la cita
            }

          // SIEMPRE elegir el de salida más tarde, priorizando mismo día
          mismoDia match {                       // Pattern matching sobre la lista `mismoDia`
            case Nil =>                          // Si `mismoDia` está vacío (no hay itinerarios que lleguen a tiempo)...
              // Todos son del día anterior, elegir el de salida más tarde
              diaAnterior.maxBy(horaSalida)      // Elige de la lista `diaAnterior` el itinerario cuyo `horaSalida` sea máxima

            case _ =>                            // Si SÍ hay itinerarios en `mismoDia`...
              // Hay del mismo día, elegir el de salida más tarde
              mismoDia.maxBy(horaSalida)         // Elige de `mismoDia` el itinerario que sale más tarde (mayor `horaSalida`)
          }
      }
    }
  }

}  // Cierre del `package object Itinerarios`
