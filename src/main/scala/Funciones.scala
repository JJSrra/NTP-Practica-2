
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
        print(calcularValorTrianguloPascal(row, col) + " ")

      // Salto de linea final para mejorar la presentacion
      println()
    }

    // Se muestra el valor que debe ocupar la columna 5 en la fila 10
    println(calcularValorTrianguloPascal(15, 10))
    println(calcularValorTrianguloPascal(0, 0))
    println(chequearBalance(List('(','(',')',')',')','(')))
    println(contarCambiosPosibles(23, List(1,2,5,10)))

    val numeros = Array(1,2,4,5,7,9,11,15,18,20,22,27)
    def criterioMenor = (a: Int, b: Int) => {a < b}
    println(busquedaBinaria(numeros, 15, criterioMenor))
  }

  /**
    * Ejercicio 1: función para generar el triángulo de Pascal
    *
    * @param columna Número de columna del triángulo
    * @param fila Número de fila del triángulo
    * @return Valor del triángulo de Pascal en esa posición
    */
  def calcularValorTrianguloPascal(fila: Int, columna: Int): Int = {
    if (columna == 0 || columna == fila) 1
    else calcularValorTrianguloPascal(fila-1, columna-1) + calcularValorTrianguloPascal(fila-1, columna)
  }

  /**
    * Ejercicio 2: función para chequear el balance de paréntesis,
    * mediante Tail Recursive
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
    * Ejercicio 3: función para determinar las posibles formas de devolver el
    * cambio de una determinada cantidad con un conjunto de monedas dado
    *
    * @param cantidad Cantidad a devolver
    * @param monedas Lista de valores de monedas de las que se dispone
    * @return Contador de número de vueltas posibles
    */
  def contarCambiosPosibles(cantidad: Int, monedas: List[Int]): Int = {
    def cambios(cantidad: Int, monedas: List[Int]): Int = {
      if (cantidad == 0) 1
      else if (monedas.isEmpty) 0
      else if (cantidad < monedas.head) 0
      else {
        var acum = 0
        for (i <- 0 to cantidad by monedas.head){
          acum += cambios(cantidad-i, monedas.tail)
        }
        acum
      }
    }

    val monedasOrdenadas = monedas.sorted
    if (cantidad > 0) cambios(cantidad, monedas)
    else 0

  }

  /**
   * Método genérico para búsqueda binaria
   * @param coleccion conjunto de datos sobre los que buscar
   * @param aBuscar elemento a buscar
   * @param criterio para comparar dos elementos de tipo A
   * @tparam A parámetro de tipo
   * @return posición del valor buscado o -1 en caso de no hallarlo
   */
  def busquedaBinaria[A](coleccion : Array[A], aBuscar: A, 
                       criterio : (A,A) => Boolean) : Int = {
    
    @annotation.tailrec
    def go(coleccion : Array[A], acum: Int): Int = {
      val medio = coleccion.length / 2
      val valorMedio = coleccion(medio)
      if (valorMedio == aBuscar) acum+medio
      else if (coleccion.length == 1) -1
      else {
        if (criterio(valorMedio,aBuscar) == true){
          if (medio+1 == coleccion.length) -1
          else go(coleccion.slice(medio+1,coleccion.length),acum+medio+1)
        }
        else{
          if (medio == 0) -1
          else go(coleccion.slice(0,medio),acum)
        }
      }
    }

    go(coleccion,0)
  }
}
