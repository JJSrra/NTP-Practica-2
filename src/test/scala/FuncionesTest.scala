import org.scalacheck.{Gen, Properties}
import org.scalacheck.Prop.{forAll}

object FuncionesTest extends Properties("FuncionesTest"){

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
}
