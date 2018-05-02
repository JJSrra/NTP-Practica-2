import org.scalacheck.Properties
import org.scalacheck.Prop.{forAll, throws, AnyOperators}
import org.scalacheck.Gen._

object ListaTest extends Properties("ListaTest"){
  // Método de generación de listas de valores enteros
  val secuenciaEnteros = listOfN(10,choose(0,10))

  secuenciaEnteros.sample
  println(secuenciaEnteros)

  property("Longitud de lista") =
    forAll(secuenciaEnteros) {
      xs => {
        // Creo una lista a partir de la secuencia
        val lista : Lista[Int] = Lista(xs : _*)
        val longitudList = xs.length
        val longitudLista = Lista.longitud(lista)
        longitudList == longitudLista
      }
    }

  property("Suma de enteros") =
    forAll(secuenciaEnteros){
      xs => {
        // Creamos objeto de clase Lista
        val lista : Lista[Int] = Lista(xs : _*)
        val sumaList = xs.map(x => x.toDouble).sum
        val sumaLista = Lista.sumaEnteros(lista)
        sumaList == sumaLista
      }
    }

  property("Producto de enteros") =
    forAll(secuenciaEnteros){
      xs => {
        val lista : Lista[Int] = Lista(xs : _*)
        val multList = xs.map(x => x.toDouble).product
        val multLista = Lista.productoEnteros(lista)
        multList == multLista
      }
    }

  property("Concatenación de cadenas") =
    forAll(secuenciaEnteros){
      xs => {
        val lista : Lista[Int] = Lista(xs : _*)
        val concatList = xs:::xs
        val concatLista = Lista.toList(Lista.concatenar(lista,lista))
        concatList == concatLista
      }
    }

  property("Fold right") =
    forAll(secuenciaEnteros){
      xs => {
        val lista : Lista[Int] = Lista(xs : _*)
        val foldRightList = xs.foldRight(0)(_ - _)
        val foldRightLista = Lista.foldRight(lista,0)(_ - _)
        foldRightList == foldRightLista
      }
    }

  property("Suma fold right") =
    forAll(secuenciaEnteros){
      xs => {
        val lista : Lista[Int] = Lista(xs : _*)
        val sumaFoldRightList = xs.foldRight(0)(_ + _)
        val sumaFoldRightLista = Lista.sumaFoldRight(lista)
        sumaFoldRightList == sumaFoldRightLista
      }
    }

  property("Producto fold right") =
    forAll(secuenciaEnteros){
      xs => {
        val lista : Lista[Int] = Lista(xs : _*)
        val prodFoldRightList = xs.product
        val prodFoldRightLista = Lista.productoFoldRight(lista)
        prodFoldRightList == prodFoldRightLista
      }
    }
}
