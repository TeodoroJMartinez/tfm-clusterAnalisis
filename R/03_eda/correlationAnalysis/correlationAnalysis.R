# Importar el conjunto de datos diamonds
data(diamonds, package = "ggplot2")

# Calcular los coeficientes de correlación
cor(diamonds[, c("carat", "depth", "table", "price")])

# Calcular la matriz de correlación
cor(diamonds[, c("carat", "depth", "table", "price")], method = "pearson")

# Visualizar los resultados
ggplot2::ggplot(
  data = diamonds, 
  mapping = ggplot2::aes(
    x = carat, 
    y = price
    )
  ) + 
  ggplot2::geom_point() + 
  ggplot2::theme_bw()
