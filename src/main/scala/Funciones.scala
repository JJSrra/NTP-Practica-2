
/**
  * Objeto singleton para probar la funcionalidad del triangulo
  * de Pascal
  */
object Funciones {
  /**
    * Metodo main: en realidad no es necesario porque el desarrollo
    * deberia guiarse por los tests de prueba
    *
    * @param args
    */
  def main(args: Array[String]) {
    println("................... Triangulo de Pascal ...................")

    // Se muestran 10 filas del trinagulo de Pascal
    for (row <- 0 to 10) {
      // Se muestran 10 10 columnas
      for (col <- 0 to row)
        print(calcularValorTrianguloPascal(col, row) + " ")

      // Salto de linea final para mejorar la presentacion
      println()
    }

    // Se muestra el valor que debe ocupar la columna 5 en la fila 10
    println(calcularValorTrianguloPascal(10, 15))
    println(calcularValorTrianguloPascal(0, 0))
    println(chequearBalance(List('(','(',')',')',')','(')))
  }

  /**
    * Ejercicio 1: función para generar el triángulo de Pascal
    *
    * @param columna Número de columna del triángulo
    * @param fila Número de fila del triángulo
    * @return Valor del triángulo de Pascal en esa posición
    */
  def calcularValorTrianguloPascal(columna: Int, fila: Int): Int = {
    if (columna == 0 || columna == fila) 1
    else calcularValorTrianguloPascal(columna-1,fila-1) + calcularValorTrianguloPascal(columna,fila-1)
  }

  /**
    * Ejercicio 2: función para chequear el balance de paréntesis
    *
    * @param cadena cadena a analizar
    * @return valor booleano con el resultado de la operación
    */
  def chequearBalance(cadena: List[Char]): Boolean = {
    @annotation.tailrec
    def go(lista: List[Char], acum: Int): Boolean ={
      if (lista.isEmpty) acum == 0
      else if (acum < 0) false
      else if (lista.head == '(') go(lista.tail, acum+1)
      else if (lista.head == ')') go(lista.tail, acum-1)
      else go(lista.tail,acum)
    }
    go (cadena, 0)
  }

  /**
    * Ejercicio 3: funcion para determinar las posibles formas de devolver el
    * cambio de una determinada cantidad con un conjunto de monedas
    *
    * @param cantidad
    * @param monedas
    * @return contador de numero de vueltas posibles
    */
  def contarCambiosPosibles(cantidad: Int, monedas: List[Int]): Int = ???

  /**
   * Metodo generico para busqueda binaria
   * @param coleccion conjunto de datos sobre los que buscar
   * @param aBuscar elemento a buscar
   * @param criterio para comparar dos elementos de tipo A
   * @tparam A parametro de tipo
   * @return posicion del valor buscado o -1 en caso de no hallarlo
   */
  def busquedaBinaria[A](coleccion : Array[A], aBuscar: A, 
                       criterio : (A,A) => Boolean) : Int = ???
}
