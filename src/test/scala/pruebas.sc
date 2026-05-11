import Newton._

// DEFINICIÓN DE EXPRESIONES
val expr1 = Suma(Atomo('x'), Numero(8.0))
val expr2 = Producto(Atomo('x'), Atomo('x'))
val expr3 = Suma(expr1, Potencia(expr2, Numero(7.0)))
val expr4 = Logaritmo(Atomo('x'))
val expr5 = Producto(Division(expr1, expr2), Resta(expr3, expr4))
val expr6 = Potencia(Atomo('x'), Numero(20.0))

//PRUEBAS MOSTRAR


// PRUEBAS DE DERIVAR
mostrar(derivar(expr6, Atomo('x')))
mostrar(derivar(expr2, Atomo('x')))
mostrar(derivar(expr2, Atomo('y')))
mostrar(derivar(Suma(Atomo('k'), Producto(Numero(3.0), Atomo('x'))), Atomo('x')))

// FUNCIONES PENDIENTES

// evaluar(Numero(5.0), Atomo('x'), 1.0)
// evaluar(Atomo('x'), Atomo('x'), 5.0)
// evaluar(Suma(expr1, expr2), Atomo('x'), 5.0)
// evaluar(Prod(expr1, expr2), Atomo('x'), 5.0)
// evaluar(Resta(expr1, expr2), Atomo('x'), 5.0)
// evaluar(Div(expr1, expr2), Atomo('x'), 5.0)
// evaluar(Expo(expr1, expr2), Atomo('x'), 5.0)
// evaluar(Logaritmo(expr1), Atomo('x'), 5.0)

// limpiar(derivar(Suma(Atomo('k'), Prod(Numero(3.0), Atomo('x'))), Atomo('x')))
// mostrar(limpiar(derivar(Suma(Atomo('k'), Prod(Numero(3.0), Atomo('x'))), Atomo('x'))))

// def buenaAprox(f: Expr, a: Atomo, d: Double): Boolean = {
//   evaluar(f, a, d) < 0.001
// }

// val e1 = Resta(Prod(Atomo('x'), Atomo('x')), Numero(2.0))
// val e2 = Resta(Prod(Atomo('x'), Atomo('x')), Numero(4.0))
// val e3 = Suma(Resta(Prod(Atomo('x'), Atomo('x')), Numero(4.0)), Prod(Numero(3.0), Atomo('x')))

// raizNewton(e1, Atomo('x'), 2.0, buenaAprox)
// raizNewton(e2, Atomo('x'), 2.0, buenaAprox)
// raizNewton(e3, Atomo('x'), 2.0, buenaAprox)