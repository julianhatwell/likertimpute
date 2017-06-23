library(gramEvol)

distance <- c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9)
period <- c(0, 2.1, 2.55, 2.74, 2.8, 2.85, 2.89, 2.93, 2.95, 2.96)

distance <- x
period <- y

ruleDef <- list(expr = grule(op(expr, expr)
                             , func(expr), var)
                , func = grule(sin, cos, log, sqrt, exp)
                , op = grule(`+`, `-`, `*`, `/`)
                , var = grule(distance, distance^n, n)
                , n = grule(1, 2, 3, 4))
grammarDef <- CreateGrammar(ruleDef)
print(grammarDef)

SymRegFitFunc <- function(expr) {
  result <- eval(expr)
  if (any(is.nan(result))) {
    return(Inf)
  }
  return (mean(log(1 + abs(period - result))))
}

ge <- GrammaticalEvolution(grammarDef
                           , SymRegFitFunc
                           , terminationCost = 0.021)
ge
best.expression <- ge$best$expression
data.frame(distance
           , period
           , GE = eval(best.expression))
