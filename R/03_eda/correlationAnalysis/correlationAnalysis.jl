# Importar el conjunto de datos diamonds
using RDatasets
diamonds = dataset("ggplot2", "diamonds")

# Calcular los coeficientes de correlación
using Statistics
Statistics.cor(diamonds[:, 1], diamonds[:,5])
Statistics.cor(diamonds[:, 1], diamonds[:,6])
Statistics.cor(diamonds[:, 1], diamonds[:,7])

# Visualizar los resultados
using Plots
scatter(diamonds[:, 1], diamonds[:, 7], xlabel = "Carat", ylabel = "Price")
