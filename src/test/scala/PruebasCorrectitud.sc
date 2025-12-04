import datos._
import Itinerarios._
import ItinerariosPar._

// ========== Helpers sencillos para imprimir ==========
def titulo(txt: String): Unit = {
  println("\n" + "=" * 80)
  println(txt)
  println("=" * 80)
}

def contar(label: String, its: List[Itinerario]): Unit =
  println(s"$label -> ${its.length} itinerarios")

// -----------------------------------------------------------------------------------
// 1. PRUEBAS CON DATOS DEL CURSO (aeropuertosCurso, vuelosCurso)
// -----------------------------------------------------------------------------------
titulo("PRUEBAS CON DATOS DEL CURSO (vuelosCurso / aeropuertosCurso)")

/*val itsCursoSeq  = itinerarios(vuelosCurso, aeropuertosCurso)
val itsCursoParF = itinerariosPar(vuelosCurso, aeropuertosCurso)*/

val itsTpoCursoSeq = itinerariosTiempo(vuelosCurso, aeropuertosCurso)
val itsTpoCursoPar = itinerariosTiempoPar(vuelosCurso, aeropuertosCurso)

/*val itsEscCursoSeq = itinerariosEscalas(vuelosCurso, aeropuertosCurso)
val itsEscCursoPar = itinerariosEscalasPar(vuelosCurso, aeropuertosCurso)

val itsAirCursoSeq = itinerariosAire(vuelosCurso, aeropuertosCurso)
val itsAirCursoPar = itinerariosAirePar(vuelosCurso, aeropuertosCurso)

val itSalCursoSeq = itinerarioSalida(vuelosCurso, aeropuertosCurso)
val itSalCursoPar = itinerarioSalidaPar(vuelosCurso, aeropuertosCurso)*/

val paresCurso = List(
  ("MID", "SVCS"),
  ("CLO", "SVCS"), // incomunicados
  ("CLO", "SVO"),
  ("CLO", "MEX"),
  ("CTG", "PTY")
)

for ((o, d) <- paresCurso) {
  println(s"\n--- Origen = $o , Destino = $d ---")

  // itinerarios base
  /*val lSeqBase = itsCursoSeq(o, d).length
  val lParBase = itsCursoParF(o, d).length
  println(s"itinerarios:     seq = $lSeqBase , par = $lParBase")*/

  // itinerariosTiempo
  val lSeqT   = itsTpoCursoSeq(o, d).length
  val lParT   = itsTpoCursoPar(o, d).length
  println(s"itinerariosTiempo: seq = $lSeqT , par = $lParT")

  // itinerariosEscalas
  /*val lSeqE   = itsEscCursoSeq(o, d).length
  val lParE   = itsEscCursoPar(o, d).length
  println(s"itinerariosEscalas: seq = $lSeqE , par = $lParE")

  // itinerariosAire
  val lSeqA   = itsAirCursoSeq(o, d).length
  val lParA   = itsAirCursoPar(o, d).length
  println(s"itinerariosAire:   seq = $lSeqA , par = $lParA")

  // itinerarioSalida (0 o 1 itinerario)
  val salSeq  = itSalCursoSeq(o, d, 11, 40)
  val salPar  = itSalCursoPar(o, d, 11, 40)
  println(s"itinerarioSalida:  seq vacio? ${salSeq.isEmpty} , par vacio? ${salPar.isEmpty}")*/
}

// -----------------------------------------------------------------------------------
// 2. PRUEBAS CON A1 (15 vuelos, USA) HOU -> BNA
// -----------------------------------------------------------------------------------
titulo("PRUEBAS CON 15 VUELOS (vuelosA1) - HOU -> BNA")

/*val itsA1Seq  = itinerarios(vuelosA1, aeropuertos)
val itsA1ParF = itinerariosPar(vuelosA1, aeropuertos)*/

val itsTpoA1Seq = itinerariosTiempo(vuelosA1, aeropuertos)
val itsTpoA1Par = itinerariosTiempoPar(vuelosA1, aeropuertos)

/*val itsEscA1Seq = itinerariosEscalas(vuelosA1, aeropuertos)
val itsEscA1Par = itinerariosEscalasPar(vuelosA1, aeropuertos)

val itsAirA1Seq = itinerariosAire(vuelosA1, aeropuertos)
val itsAirA1Par = itinerariosAirePar(vuelosA1, aeropuertos)

val itSalA1Seq  = itinerarioSalida(vuelosA1, aeropuertos)
val itSalA1Par  = itinerarioSalidaPar(vuelosA1, aeropuertos)*/

val oA1 = "HOU"
val dA1 = "BNA"

/*contar("itinerarios A1 seq", itsA1Seq(oA1, dA1))
contar("itinerarios A1 par", itsA1ParF(oA1, dA1))*/

contar("itinerariosTiempo A1 seq", itsTpoA1Seq(oA1, dA1))
contar("itinerariosTiempo A1 par", itsTpoA1Par(oA1, dA1))

/*contar("itinerariosEscalas A1 seq", itsEscA1Seq(oA1, dA1))
contar("itinerariosEscalas A1 par", itsEscA1Par(oA1, dA1))

contar("itinerariosAire A1 seq", itsAirA1Seq(oA1, dA1))
contar("itinerariosAire A1 par", itsAirA1Par(oA1, dA1))

println(s"itinerarioSalida A1 seq vacío? ${itSalA1Seq(oA1, dA1, 18, 30).isEmpty}")
println(s"itinerarioSalida A1 par vacío? ${itSalA1Par(oA1, dA1, 18, 30).isEmpty}")*/

// -----------------------------------------------------------------------------------
// 3. PRUEBAS CON B1 (40 vuelos) DFW -> ORD
// -----------------------------------------------------------------------------------
titulo("PRUEBAS CON 40 VUELOS (vuelosB1) - DFW -> ORD")

/*val itsB1Seq  = itinerarios(vuelosB1, aeropuertos)
val itsB1ParF = itinerariosPar(vuelosB1, aeropuertos)*/

val itsTpoB1Seq = itinerariosTiempo(vuelosB1, aeropuertos)
val itsTpoB1Par = itinerariosTiempoPar(vuelosB1, aeropuertos)

/*val itsEscB1Seq = itinerariosEscalas(vuelosB1, aeropuertos)
val itsEscB1Par = itinerariosEscalasPar(vuelosB1, aeropuertos)

val itsAirB1Seq = itinerariosAire(vuelosB1, aeropuertos)
val itsAirB1Par = itinerariosAirePar(vuelosB1, aeropuertos)

val itSalB1Seq  = itinerarioSalida(vuelosB1, aeropuertos)
val itSalB1Par  = itinerarioSalidaPar(vuelosB1, aeropuertos)*/

val oB1 = "DFW"
val dB1 = "ORD"

/*contar("itinerarios B1 seq", itsB1Seq(oB1, dB1))
contar("itinerarios B1 par", itsB1ParF(oB1, dB1))*/

contar("itinerariosTiempo B1 seq", itsTpoB1Seq(oB1, dB1))
contar("itinerariosTiempo B1 par", itsTpoB1Par(oB1, dB1))

/*contar("itinerariosEscalas B1 seq", itsEscB1Seq(oB1, dB1))
contar("itinerariosEscalas B1 par", itsEscB1Par(oB1, dB1))

contar("itinerariosAire B1 seq", itsAirB1Seq(oB1, dB1))
contar("itinerariosAire B1 par", itsAirB1Par(oB1, dB1))

println(s"itinerarioSalida B1 seq vacío? ${itSalB1Seq(oB1, dB1, 18, 30).isEmpty}")
println(s"itinerarioSalida B1 par vacío? ${itSalB1Par(oB1, dB1, 18, 30).isEmpty}")*/

// -----------------------------------------------------------------------------------
// 4. PRUEBAS CON C1 (100 vuelos) ORD -> TPA
// -----------------------------------------------------------------------------------
titulo("PRUEBAS CON 100 VUELOS (vuelosC1) - ORD -> TPA")

/*val itsC1Seq  = itinerarios(vuelosC1, aeropuertos)
val itsC1ParF = itinerariosPar(vuelosC1, aeropuertos)*/

val itsTpoC1Seq = itinerariosTiempo(vuelosC1, aeropuertos)
val itsTpoC1Par = itinerariosTiempoPar(vuelosC1, aeropuertos)

/*val itsEscC1Seq = itinerariosEscalas(vuelosC1, aeropuertos)
val itsEscC1Par = itinerariosEscalasPar(vuelosC1, aeropuertos)

val itsAirC1Seq = itinerariosAire(vuelosC1, aeropuertos)
val itsAirC1Par = itinerariosAirePar(vuelosC1, aeropuertos)

val itSalC1Seq  = itinerarioSalida(vuelosC1, aeropuertos)
val itSalC1Par  = itinerarioSalidaPar(vuelosC1, aeropuertos)*/

val oC1 = "ORD"
val dC1 = "TPA"

/*contar("itinerarios C1 seq", itsC1Seq(oC1, dC1))
contar("itinerarios C1 par", itsC1ParF(oC1, dC1))*/

contar("itinerariosTiempo C1 seq", itsTpoC1Seq(oC1, dC1))
contar("itinerariosTiempo C1 par", itsTpoC1Par(oC1, dC1))

/*contar("itinerariosEscalas C1 seq", itsEscC1Seq(oC1, dC1))
contar("itinerariosEscalas C1 par", itsEscC1Par(oC1, dC1))

contar("itinerariosAire C1 seq", itsAirC1Seq(oC1, dC1))
contar("itinerariosAire C1 par", itsAirC1Par(oC1, dC1))

println(s"itinerarioSalida C1 seq vacío? ${itSalC1Seq(oC1, dC1, 18, 30).isEmpty}")
println(s"itinerarioSalida C1 par vacío? ${itSalC1Par(oC1, dC1, 18, 30).isEmpty}")*/

// -----------------------------------------------------------------------------------
// 5. PRUEBAS CON 200 VUELOS (C1 ++ C2) ORD -> TPA
// -----------------------------------------------------------------------------------
titulo("PRUEBAS CON 200 VUELOS (vuelosC1 ++ vuelosC2) - ORD -> TPA")

val vuelos200 = vuelosC1 ++ vuelosC2

/*val its200Seq  = itinerarios(vuelos200, aeropuertos)
val its200ParF = itinerariosPar(vuelos200, aeropuertos)*/

val itsTpo200Seq = itinerariosTiempo(vuelos200, aeropuertos)
val itsTpo200Par = itinerariosTiempoPar(vuelos200, aeropuertos)

/*val itsEsc200Seq = itinerariosEscalas(vuelos200, aeropuertos)
val itsEsc200Par = itinerariosEscalasPar(vuelos200, aeropuertos)

val itsAir200Seq = itinerariosAire(vuelos200, aeropuertos)
val itsAir200Par = itinerariosAirePar(vuelos200, aeropuertos)

val itSal200Seq  = itinerarioSalida(vuelos200, aeropuertos)
val itSal200Par  = itinerarioSalidaPar(vuelos200, aeropuertos)

contar("itinerarios 200 seq", its200Seq(oC1, dC1))
contar("itinerarios 200 par", its200ParF(oC1, dC1))*/

contar("itinerariosTiempo 200 seq", itsTpo200Seq(oC1, dC1))
contar("itinerariosTiempo 200 par", itsTpo200Par(oC1, dC1))

/*contar("itinerariosEscalas 200 seq", itsEsc200Seq(oC1, dC1))
contar("itinerariosEscalas 200 par", itsEsc200Par(oC1, dC1))

contar("itinerariosAire 200 seq", itsAir200Seq(oC1, dC1))
contar("itinerariosAire 200 par", itsAir200Par(oC1, dC1))

println(s"itinerarioSalida 200 seq vacío? ${itSal200Seq(oC1, dC1, 18, 30).isEmpty}")
println(s"itinerarioSalida 200 par vacío? ${itSal200Par(oC1, dC1, 18, 30).isEmpty}")*/

println("\nPRUEBAS DE CORRECTITUD COMPLETADAS")
