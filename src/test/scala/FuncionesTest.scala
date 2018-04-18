import org.scalacheck.{Gen, Properties}
import org.scalacheck.Prop.{forAll}

object FuncionesTest extends Properties("FuncionesTest"){

  val coordenadasExtremos = for {
    fila <- Gen.choose(0,20)
    columna <- Gen.oneOf(0, fila)
  } yield (fila, columna)

  property("Elementos en lados del triángulo valen 1") = {
    forAll(coordenadasExtremos) { (i) => {
      val resultado = Funciones.calcularValorTrianguloPascal(i._1, i._2)
      resultado == 1
    }}
  }
}
