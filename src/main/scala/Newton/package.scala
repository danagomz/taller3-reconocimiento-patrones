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
  case class Expo(e1: Expr, e2: Expr) extends Expr
  case class Logaritmo(argumento: Expr) extends Expr

  // 1. FUNCIÓN MOSTRAR (ta completa)

  def mostrar(e: Expr): String = e match {
    case Numero(d) => d.toString
    case Atomo(x) => x.toString
    case Suma(e1, e2) => "(" + mostrar(e1) + " + " + mostrar(e2) + ")"
    case Resta(e1, e2) => "(" + mostrar(e1) + " - " + mostrar(e2) + ")"
    case Producto(e1, e2) => "(" + mostrar(e1) + " * " + mostrar(e2) + ")"
    case Division(e1, e2) => "(" + mostrar(e1) + " / " + mostrar(e2) + ")"
    case Expo(e1, e2) => "(" + mostrar(e1) + " ^ " + mostrar(e2) + ")"
    case Logaritmo(e1) => "(lg(" + mostrar(e1) + "))"
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
  def evaluar(f: Expr, a: Atomo, v: Double): Double = f match {
    case Numero(d) => d
    case Atomo(_) => v // único átomo en la fórmula
    case Suma(e1, e2) => evaluar(e1, a, v) + evaluar(e2, a, v)
    case Resta(e1, e2) => evaluar(e1, a, v) - evaluar(e2, a, v)
    case Producto(e1, e2) => evaluar(e1, a, v) * evaluar(e2, a, v)
    case Division(e1, e2) => evaluar(e1, a, v) / evaluar(e2, a, v)
    case Expo(e1, e2) => math.pow(evaluar(e1, a, v), evaluar(e2, a, v))
    case Logaritmo(e1) => math.log(evaluar(e1, a, v))
  }
  // 4. FUNCIÓN LIMPIAR
  // def limpiar(expresion: Expr): Expr = ???

  def limpiar(f: Expr): Expr = f match {
    case Numero(d) => Numero(d)
    case Atomo(x) => Atomo(x)

    // Suma:   0 + e = e ;  e + 0 = e
    case Suma(e1, e2) =>
      val l1 = limpiar(e1)
      val l2 = limpiar(e2)
      (l1, l2) match {
        case (Numero(0.0), _) => l2
        case (_, Numero(0.0)) => l1
        case _ => Suma(l1, l2)
      }

    // Resta:  e - 0 = e ;  0 - 0 = 0
    case Resta(e1, e2) =>
      val l1 = limpiar(e1)
      val l2 = limpiar(e2)
      (l1, l2) match {
        case (Numero(0.0), Numero(0.0)) => Numero(0.0)
        case (_, Numero(0.0)) => l1
        case _ => Resta(l1, l2)
      }

    // Prod:   0*e = 0 ;  e*0 = 0 ;  1*e = e ;  e*1 = e
    case Producto(e1, e2) =>
      val l1 = limpiar(e1)
      val l2 = limpiar(e2)
      (l1, l2) match {
        case (Numero(0.0), _) => Numero(0.0)
        case (_, Numero(0.0)) => Numero(0.0)
        case (Numero(1.0), _) => l2
        case (_, Numero(1.0)) => l1
        case _ => Producto(l1, l2)
      }

    // Div:    0/e = 0 ;  e/1 = e
    case Division(e1, e2) =>
      val l1 = limpiar(e1)
      val l2 = limpiar(e2)
      (l1, l2) match {
        case (Numero(0.0), _) => Numero(0.0)
        case (_, Numero(1.0)) => l1
        case _ => Division(l1, l2)
      }

    // Expo:   e^0 = 1 ;  e^1 = e ;  0^e = 0 ;  1^e = 1
    case Expo(e1, e2) =>
      val l1 = limpiar(e1)
      val l2 = limpiar(e2)
      (l1, l2) match {
        case (_, Numero(0.0)) => Numero(1.0)
        case (_, Numero(1.0)) => l1
        case (Numero(0.0), _) => Numero(0.0)
        case (Numero(1.0), _) => Numero(1.0)
        case _ => Expo(l1, l2)
      }

    // Logaritmo: limpia argumento
    case Logaritmo(e1) => Logaritmo(limpiar(e1))
  }


  // 5. FUNCIÓN RAIZNEWTON
  // def raizNewton(funcion: Expr, variable: Atomo, x0: Double, criterioParada: (Expr, Atomo, Double) => Boolean): Double = ???
  // Hola Gomez
}


