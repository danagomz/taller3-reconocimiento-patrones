import Newton._

// DEFINICIÓN DE EXPRESIONES
val expr1 = Suma(Atomo('x'), Numero(2.0))
val expr2 = Producto(Atomo('x'), Atomo('x'))
val expr3 = Suma(expr1, Expo(expr2, Numero(5.0)))
val expr4 = Logaritmo(Atomo('x'))
val expr5 = Producto(Division(expr1, expr2), Resta(expr3, expr4))
val expr6 = Expo(Atomo('x'), Numero(3.0))

//PRUEBAS MOSTRAR
mostrar(expr1)   // (x + 2.0)
mostrar(expr2)   // (x * x)
mostrar(expr3)   // ((x + 2.0) + ((x * x) ^ 5.0))
mostrar(expr4)   // (lg(x))
mostrar(expr5)   // (((x + 2.0) / (x * x)) * (((x + 2.0) + ((x * x) ^ 5.0)) - (lg(x))))
mostrar(expr6)   // (x ^ 3.0)

// PRUEBAS DE DERIVAR
mostrar(derivar(expr6, Atomo('x')))
mostrar(derivar(expr2, Atomo('x')))
mostrar(derivar(expr2, Atomo('y')))
mostrar(derivar(Suma(Atomo('k'), Producto(Numero(3.0), Atomo('x'))), Atomo('x')))



//PRUEBAS EVALUAR
mostrar(Numero(5.0))                                  // 5.0
evaluar(Numero(5.0), Atomo('x'), 1.0)                 // 5.0
mostrar(Atomo('x'))                                   // x
evaluar(Atomo('x'), Atomo('x'), 5.0)                  // 5.0
mostrar(Suma(expr1, expr2))                           // ((x + 2.0) + (x * x))
evaluar(Suma(expr1, expr2), Atomo('x'), 5.0)          // 32.0
mostrar(Producto(expr1, expr2))                       // ((x + 2.0) * (x * x))
evaluar(Producto(expr1, expr2), Atomo('x'), 5.0)      // 175.0
mostrar(Resta(expr1, expr2))                          // ((x + 2.0) - (x * x))
evaluar(Resta(expr1, expr2), Atomo('x'), 5.0)         // -18.0
mostrar(Division(expr1, expr2))                       // ((x + 2.0) / (x * x))
evaluar(Division(expr1, expr2), Atomo('x'), 5.0)      // 0.28
mostrar(Expo(expr1, expr2))                           // ((x + 2.0) ^ (x * x))
evaluar(Expo(expr1, expr2), Atomo('x'), 5.0)          // 1.341068619663965E21
mostrar(Logaritmo(expr1))                             // (lg((x + 2.0)))
evaluar(Logaritmo(expr1), Atomo('x'), 5.0)            // 1.9459101490553132

// PRUEBAS LIMPIAR

limpiar(derivar(Suma(Atomo('k'), Producto(Numero(3.0), Atomo('x'))), Atomo('x')))
// Numero(3.0)

mostrar(limpiar(derivar(Suma(Atomo('k'), Producto(Numero(3.0), Atomo('x'))), Atomo('x'))))
// 3.0

// Pruebas adicionales de limpiar
mostrar(limpiar(Suma(Numero(0.0), Atomo('x'))))                // x
mostrar(limpiar(Producto(Numero(1.0), Atomo('x'))))            // x
mostrar(limpiar(Producto(Numero(0.0), Atomo('x'))))            // 0.0
mostrar(limpiar(Expo(Atomo('x'), Numero(1.0))))                // x
mostrar(limpiar(Expo(Atomo('x'), Numero(0.0))))                // 1.0
mostrar(limpiar(Division(Numero(0.0), Atomo('x'))))            // 0.0

// def buenaAprox(f: Expr, a: Atomo, d: Double): Boolean = {
//   evaluar(f, a, d) < 0.001
// }

// val e1 = Resta(Prod(Atomo('x'), Atomo('x')), Numero(2.0))
// val e2 = Resta(Prod(Atomo('x'), Atomo('x')), Numero(4.0))
// val e3 = Suma(Resta(Prod(Atomo('x'), Atomo('x')), Numero(4.0)), Prod(Numero(3.0), Atomo('x')))

// raizNewton(e1, Atomo('x'), 2.0, buenaAprox)
// raizNewton(e2, Atomo('x'), 2.0, buenaAprox)
// raizNewton(e3, Atomo('x'), 2.0, buenaAprox)