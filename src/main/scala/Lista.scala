/**
  * Interfaz genérica para la lista
  * @tparam A
  */
sealed trait Lista[+A]

/**
  * Objeto para definir lista vacía
  */
case object Nil extends Lista[Nothing]

/**
  * Clase para definir la lista como compuesta por elemento inicial
  * (cabeza) y resto (cola)
  * @param cabeza
  * @param cola
  * @tparam A
  */
case class Cons[+A](cabeza : A, cola : Lista[A]) extends Lista[A]

/**
  * Objeto para desarrollar las funciones pedidas
  */
object Lista {

   /**
     * Método para permitir crear listas sin usar new
     * @param elementos secuencia de elementos a incluir en la lista
     * @tparam A
     * @return
     */
   def apply[A](elementos : A*) : Lista[A] = {
      if (elementos.isEmpty) Nil
      else Cons(elementos.head, apply(elementos.tail : _*))
   }

   /**
     * Obtiene la longitud de una lista
     * @param lista
     * @tparam A
     * @return
     */
   def longitud[A](lista : Lista[A]) : Int = {
      lista match {
         case Nil => 0
         case Cons(cabeza, cola) => 1 + longitud(cola)
      }
   }

   /**
     * Método para sumar los valores de una lista de enteros
     * @param enteros
     * @return
     */
   def sumaEnteros(enteros : Lista[Int]) : Double = {
      enteros match {
         case Nil => 0.0
         case Cons(cabeza, cola) => cabeza.toDouble + sumaEnteros(cola)
      }
   }

   /**
     * Metodo para multiplicar los valores de una lista de enteros
     * @param enteros
     * @return
     */
   def productoEnteros(enteros : Lista[Int]) : Double = {
      enteros match {
         case Nil => 1.0
         case Cons(cabeza, cola) => cabeza.toDouble * productoEnteros(cola)
      }
   }

   /**
     * Método para agregar el contenido de dos listas
     * @param lista1
     * @param lista2
     * @tparam A
     * @return
     */
   def concatenar[A](lista1: Lista[A], lista2: Lista[A]): Lista[A] = {
     lista1 match {
       case Nil => lista2
       case Cons(cabeza, cola) => Cons(cabeza, concatenar(cola, lista2))
     }
   }

  /**
    * Método para pasar un objeto de tipo Lista a tipo List, para hacer comprobaciones
    * @param lista
    * @tparam A
    * @return
    */
   def toList[A](lista : Lista[A]):List[A] = {
     lista match {
       case Nil => List()
       case Cons(cabeza, cola) => cabeza::toList(cola)
     }
   }

   /**
     * Función de utilidad para aplicar una funcion de forma sucesiva a los
     * elementos de la lista
     * @param lista
     * @param neutro
     * @param funcion
     * @tparam A
     * @tparam B
     * @return
     */
   def foldRight[A, B](lista : Lista[A], neutro : B)
                      (funcion : (A, B) => B): B = {
     lista match {
       case Nil => neutro
       case Cons(cabeza, cola) => funcion(cabeza, foldRight(cola, neutro)(funcion))
     }
   }

   /**
     * Suma mediante foldRight
     * @param listaEnteros
     * @return
     */
   def sumaFoldRight(listaEnteros : Lista[Int]) : Double = {
     foldRight(listaEnteros,0)(_ + _)
   }

   /**
     * Producto mediante foldRight
     * @param listaEnteros
     * @return
     */
   def productoFoldRight(listaEnteros : Lista[Int]) : Double = {
     foldRight(listaEnteros,1)(_ * _)
   }

   /**
     * Reemplaza la cabeza por nuevo valor. Se asume que si la lista esta vacia
     * se devuelve una lista con el nuevo elemento
     *
     * @param lista
     * @param cabezaNueva
     * @tparam A
     * @return
     */
   def asignarCabeza[A](lista : Lista[A], cabezaNueva : A) : Lista[A] = {
     lista match {
       case Nil => Cons(cabezaNueva, Nil)
       case Cons(cabeza, cola) => Cons(cabezaNueva, cola)
     }
   }

   /**
     * Elimina el elemento cabeza de la lista
     * @param lista
     * @tparam A
     * @return
     */
   def tail[A](lista : Lista[A]): Lista[A] = {
     lista match {
       case Nil => Nil
       case Cons(cabeza, cola) => cola
     }
   }

   /**
     * Elimina los n primeros elementos de una lista
     * @param lista lista con la que trabajar
     * @param n numero de elementos a eliminar
     * @tparam A tipo de datos
     * @return
     */
   def eliminar[A](lista : Lista[A], n: Int) : Lista[A] = ???

   /**
     * Elimina elementos mientra se cumple la condicion pasada como
     * argumento
     * @param lista lista con la que trabajar
     * @param criterio predicado a considerar para continuar con el borrado
     * @tparam A tipo de datos a usar
     * @return
     */
   def eliminarMientras[A](lista : Lista[A], criterio: A => Boolean) : Lista[A] = ???

   /**
     * Elimina el ultimo elemento de la lista. Aqui no se pueden compartir
     * datos en los objetos y hay que generar una nueva lista copiando
     * datos
     * @param lista lista con la que trabajar
     * @tparam A tipo de datos de la lista
     * @return
     */
   def eliminarUltimo[A](lista : Lista[A]) : Lista[A] = ???

   /**
     * foldLeft con recursividad tipo tail
     * @param lista lista con la que trabajar
     * @param neutro elemento neutro
     * @param funcion funcion a aplicar
     * @tparam A parametros de tipo de elementos de la lista
     * @tparam B parametro de tipo del elemento neutro
     * @return
     */
   //@annotation.tailrec
   def foldLeft[A, B](lista : Lista[A], neutro: B)(funcion : (B, A) => B): B = ???
}
