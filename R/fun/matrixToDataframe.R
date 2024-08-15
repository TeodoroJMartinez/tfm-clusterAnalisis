## Creamos la matriz con los datos de ejemplo `condenas`
condenas <-
  matrix(
    c(2, 10, 15, 3),
    nrow = 2,
    dimnames = list(
      "TipoEmbarazo" = c("Dicigótico", "Monocigótico"),
      "Condenas" = c("Condenado", "No condenado")
    )
  )
## Convertimos en un data.frame, anidando dos funciones
as.data.frame(as.table(condenas))
