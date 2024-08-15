# Importar el conjunto de datos diamonds
import seaborn as sns
diamonds = sns.load_dataset('diamonds')

# Calcular los coeficientes de correlación
diamonds[['carat', 'depth', 'table', 'price']].corr()

# Calcular la matriz de correlación
diamonds[['carat', 'depth', 'table', 'price']].corr(method='pearson')

# Visualizar los resultados
import matplotlib.pyplot as plt
plt.scatter(diamonds['carat'], diamonds['price'])
plt.xlabel('Carat')
plt.ylabel('Price')
plt.show()

