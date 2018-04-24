import org.scalacheck.{Gen, Properties}
import org.scalacheck.Prop.{forAll}

object FuncionesTest extends Properties("FuncionesTest"){

  // =============================================================

  // TRIÁNGULO DE PASCAL

  val MAXIMO = 20
  val coordenadasExtremos = for {
    fila <- Gen.choose(0,MAXIMO)
    columna <- Gen.oneOf(0, fila)
  } yield (fila, columna)

  property("Elementos en lados del triángulo valen 1") = {
    forAll(coordenadasExtremos) { (i) => {
      val resultado = Funciones.calcularValorTrianguloPascal(i._1, i._2)
      resultado == 1
    }}
  }

  val coordenadasInternas = for {
    fila <- Gen.choose(2,MAXIMO)
    columna <- Gen.choose(1,fila-1)
  } yield (fila, columna)

  property("Elementos internos del triángulo correctos") = {
    forAll(coordenadasInternas) { (i) => {
      val resultado = Funciones.calcularValorTrianguloPascal(i._1, i._2)
      resultado == Funciones.calcularValorTrianguloPascal(i._1 -1, i._2 -1) +
                  Funciones.calcularValorTrianguloPascal(i._1 -1, i._2)
    }}
  }

  // =============================================================

  // CHEQUEAR PARÉNTESIS

  val strGen = (n: Int) =>
    Gen.listOfN(n, Gen.oneOf('(',')',Gen.alphaChar.sample.get))

  def comprobarParentesis(lista: List[Char]): Boolean = {

    for (i <- 1 to lista.length){
      val sublista = lista.slice(0,i)
      if (sublista.count("("==) < sublista.count(")"==)) return false
    }
    return true
  }

  property("El resultado del balance de paréntesis es correcto") = {
    forAll(strGen(10)) { (cadena) => {

      // En este caso la propiedad de que las subcadenas tengan al menos el mismo número de paréntesis
      // de apertura que de cierre sirve para confirmar que la cadena es incorrecta, pero no para
      // detectar si la cadena está bien formada. Por tanto, en la propiedad interpreto que los resultados
      // de ambos métodos sea false o que el de la propiedad sea true.
      (!Funciones.chequearBalance(cadena) == !comprobarParentesis(cadena)) || comprobarParentesis(cadena)
    }}
  }
}
