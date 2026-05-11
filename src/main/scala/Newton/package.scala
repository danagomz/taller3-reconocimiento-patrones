package object Newton {

  // DEFINICIÓN DE LA EXPRESIÓN
  trait Expr
  case class Numero(valor: Double) extends Expr
  case class Atomo(variable: Char) extends Expr
  case class Suma(termino1: Expr, termino2: Expr) extends Expr
  case class Producto(factor1: Expr, factor2: Expr) extends Expr
  case class Resta(termino1: Expr, termino2: Expr) extends Expr
  case class Division(dividendo: Expr, divisor: Expr) extends Expr
  case class Potencia(base: Expr, exponente: Expr) extends Expr
  case class Logaritmo(argumento: Expr) extends Expr

  // 1. FUNCIÓN MOSTRAR (eta incompleta)
  def mostrar(expresion: Expr): String = {
    expresion match {
      case Numero(valor) => valor.toString
      case Atomo(variable) => variable.toString
      case Suma(t1, t2) => s"(${mostrar(t1)} + ${mostrar(t2)})"
      case Resta(t1, t2) => s"(${mostrar(t1)} - ${mostrar(t2)})"
      case Producto(f1, f2) => s"(${mostrar(f1)} * ${mostrar(f2)})"
      case Division(d1, d2) => s"(${mostrar(d1)} / ${mostrar(d2)})"
      case Potencia(b, e) => s"(${mostrar(b)} ^ ${mostrar(e)})"
      case Logaritmo(arg) => s"lg(${mostrar(arg)})"
    }
  }

  // 2. FUNCIÓN DERIVAR
  def derivar(funcion: Expr, variable: Atomo): Expr = {
    (funcion, variable) match {
      case (Numero(_), _) => Numero(0.0)
      case (Atomo(x), Atomo(y)) if x == y => Numero(1.0)
      case (Atomo(_), _) => Numero(0.0)
      case (Suma(t1, t2), _) => Suma(derivar(t1, variable), derivar(t2, variable))
      case (Resta(t1, t2), _) => Resta(derivar(t1, variable), derivar(t2, variable))
      case (Producto(f1, f2), _) => Suma(
        Producto(derivar(f1, variable), f2),
        Producto(f1, derivar(f2, variable))
      )

      case (Division(dividendo, divisor), _) => Division(
        Resta(
          Producto(derivar(dividendo, variable), divisor),
          Producto(dividendo, derivar(divisor, variable))
        ),
        Potencia(divisor, Numero(2.0))
      )

      case (Logaritmo(arg), _) => Division(
        derivar(arg, variable),
        arg
      )
      case (Potencia(base, exponente), _) => Producto(
        Potencia(base, exponente),
        Suma(
          Division(
            Producto(derivar(base, variable), exponente),
            base
          ),
          Producto(derivar(exponente, variable), Logaritmo(base))
        )
      )
    }
  }

  // 3. FUNCIÓN EVALUAR
  // def evaluar(expresion: Expr, variable: Atomo, valor: Double): Double = ???

  // 4. FUNCIÓN LIMPIAR
  // def limpiar(expresion: Expr): Expr = ???


  // 5. FUNCIÓN RAIZNEWTON
  // def raizNewton(funcion: Expr, variable: Atomo, x0: Double, criterioParada: (Expr, Atomo, Double) => Boolean): Double = ???
}


