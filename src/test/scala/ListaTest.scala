import org.scalacheck.{Gen, Properties}
import org.scalacheck.Prop.{AnyOperators, forAll, throws}
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

  property("Asignación de cabeza") =
    forAll(secuenciaEnteros){
      xs => {
        val lista : Lista[Int] = Lista(xs : _*)
        val nuevaCabeza = Gen.choose(0,10)

        val listCambiada = xs match {
          case List() => List(nuevaCabeza)
          case cabeza::cola => nuevaCabeza::cola
        }

        val listaCambiada = Lista.toList(Lista.asignarCabeza(lista, nuevaCabeza))

        listCambiada == listaCambiada
      }
    }

  property("Eliminación de cabeza") =
    forAll(secuenciaEnteros){
      xs => {
        val lista : Lista[Int] = Lista(xs : _*)

        val listSinCabeza = xs.tail
        val listaSinCabeza = Lista.toList(Lista.tail(lista))

        listSinCabeza == listaSinCabeza
      }
    }

  property("Eliminación de N elementos") =
    forAll(secuenciaEnteros){
      xs => {
        val lista : Lista[Int] = Lista(xs : _*)
        val aEliminar = Gen.choose(0,15)

        val listConEliminados = xs.drop(5)
        val listaConEliminados = Lista.toList(Lista.eliminar(lista, 5))

        listConEliminados == listaConEliminados
      }
    }

  property("Eliminación mientras criterio") =
    forAll(secuenciaEnteros){
      xs => {
        val lista : Lista[Int] = Lista(xs : _*)
        val criterio = (a:Int) => a < 5

        val listConEliminados = xs.dropWhile(criterio)
        val listaConEliminados = Lista.toList(Lista.eliminarMientras(lista, criterio))

        listConEliminados == listaConEliminados
      }
    }

  property("Eliminación de último elemento") =
    forAll(secuenciaEnteros){
      xs => {
        val lista : Lista[Int] = Lista(xs : _*)

        val listSinUltimo = xs.dropRight(1)
        val listaSinUltimo = Lista.toList(Lista.eliminarUltimo(lista))

        listSinUltimo == listaSinUltimo
      }
    }
}
