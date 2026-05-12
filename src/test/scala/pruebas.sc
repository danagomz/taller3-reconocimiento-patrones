import Newton._

// DEFINICIÓN DE EXPRESIONES
val expr1 = Suma(Atomo('x'), Numero(2.0))
val expr2 = Prod(Atomo('x'), Atomo('x'))
val expr3 = Suma(expr1, Expo(expr2, Numero(5.0)))
val expr4 = Logaritmo(Atomo('x'))
val expr5 = Prod(Div(expr1, expr2), Resta(expr3, expr4))
val expr6 = Expo(Atomo('x'), Numero(3.0))

// PRUEBAS MOSTRAR
mostrar(expr1)
mostrar(expr2)
mostrar(expr3)
mostrar(expr4)
mostrar(expr5)

// PRUEBAS DE DERIVAR
mostrar(derivar(expr6, Atomo('x')))
mostrar(derivar(expr2, Atomo('x')))
mostrar(derivar(expr2, Atomo('y')))
mostrar(derivar(Suma(Atomo('k'), Prod(Numero(3.0), Atomo('x'))), Atomo('x')))
mostrar(derivar(Div(Atomo('x'), Numero(2.0)), Atomo('x')))

// PRUEBAS EVALUAR
mostrar(Numero(5.0))
evaluar(Numero(5.0), Atomo('x'), 1.0)

mostrar(Suma(expr1, expr2))
evaluar(Suma(expr1, expr2), Atomo('x'), 5.0)

mostrar(Prod(expr1, expr2))
evaluar(Prod(expr1, expr2), Atomo('x'), 5.0)

mostrar(Expo(expr1, expr2))
evaluar(Expo(expr1, expr2), Atomo('x'), 5.0)

mostrar(Logaritmo(expr1))
evaluar(Logaritmo(expr1), Atomo('x'), 5.0)

// PRUEBAS LIMPIAR
limpiar(derivar(Suma(Atomo('k'), Prod(Numero(3.0), Atomo('x'))), Atomo('x')))
mostrar(limpiar(derivar(Suma(Atomo('k'), Prod(Numero(3.0), Atomo('x'))), Atomo('x'))))
mostrar(limpiar(Suma(Numero(0.0), Atomo('x'))))
mostrar(limpiar(Prod(Numero(1.0), Atomo('x'))))
mostrar(limpiar(Expo(Atomo('x'), Numero(1.0))))
mostrar(limpiar(Div(Numero(0.0), Atomo('x'))))

// PRUEBAS RAIZNEWTON
def buenaAprox(f: Expr, a: Atomo, d: Double): Boolean = {
  math.abs(evaluar(f, a, d)) < 0.001
}
val e1 = Resta(Atomo('x'), Numero(5.0))
val e2 = Resta(Prod(Numero(2.0), Atomo('x')), Numero(8.0))
val e3 = Resta(Expo(Atomo('x'), Numero(3.0)), Numero(8.0))
val e4 = Logaritmo(Atomo('x'))
val e5 = Resta(Expo(Numero(math.E), Atomo('x')), Numero(10.0))

raizNewton(e1, Atomo('x'), 10.0, buenaAprox)
raizNewton(e2, Atomo('x'), 10.0, buenaAprox)
raizNewton(e3, Atomo('x'), 3.0, buenaAprox)
raizNewton(e4, Atomo('x'), 2.0, buenaAprox)
raizNewton(e5, Atomo('x'), 3.0, buenaAprox)