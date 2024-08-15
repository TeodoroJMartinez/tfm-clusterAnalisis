# Crea una estructura básica de comentarios para documentación

#' @title Evalúa los criterios de calidad del dato para la fase de ingesta
#'
#' @description La función evalúa los siguientes aspectos:
#' 1.- Número de observaciones
#' 2.- Número de variables
#' 3.- Tipo de variables
#' 4.- Análisis de datos faltantes
#'
#' Incluye información sobre otras dimensiones a evaluar:
#' 5.- Consistencia de las variables ingeridas con lo esperado
#' 6.- Consistencia de los valores ingeridos respecto a lo esperado
#' 7.- Las relaciones de las variables son acordes a lo esperado
#'
#' @usage dataQual_ingest(data)
#'
#' @returns Devuelve una lista de cuatro elementos:
#' 1.- nRegistrosRaw: El número de registros del fichero ingerido (integer)
#' 2.- nVariablesRaw: El número de variables del fichero ingerido (integer)
#' 3.- claseVariablesRaw: Una lista con la clase de las variables ingeridas (lista)
#' 4.- datosFaltantesRaw: Una evaluación de los datos faltantes (lista:
#' 4a. nObservacionesIncompletas: Número de observaciones incompletas (integer)
#' 4b. nVariablesIncompletas: Número de variables a las que le faltan registros(integer)
#' 4c. nRegistrosIncompletos_xVariable: Número de registros sin datos para cada variable (integer)
#' 4d. idxObservacionesIncompletas: Índice de observaciones con NAs (lógico)
#' 4e. idxVariablesIncompletas: Índice de números de columna con NAs (lógico)
#' 4f. idxRegistrosIncompletos_xVariable: Lista de índices con NAs, por variable (lista)
#' 4g. graphAggregationPlot: Parametrización para el gráfico de NAs del paquete VIM (lista)
#' Para dibujar el gráfico, debe utilizarse la función plot con el elemento 4g.
#' Describe la salida de la función. Típicamente, 1-2 frases que describen el tipo de salida, pero puede incluir también discusión sobre posibles errores o advertencias importantes
#'
#' @param data data.frame sobre el que se quiere hacer la evaluación de calidad de la ingesta
#'
#' @examples# dataQualEval_ingest(iris): Código de R ejecutable que demuestra el trabajo de la función. El código debe correr sin errores.
#' @examplesIf condición# Código de ejemplo : Ejecuta los ejemplos sólo cuando la condición es TRUE.
#'
#' @noRd Suprime la generación de .Rd para un bloque. Se utiliza para bloque de documentación que sólo deben verse en el código fuente.
#' @rawRd rd: Inserta texto literal directamente en el fichero .Rd

dataQual_ingest <- function(data) {
  dataQualEval_ingest <- list(
    # 1. How many records does this dataset contain?
    nRegistrosRaw = nrow(data),

    # 2. How many fields (i.e., variables) are included in each record?
    nVariablesRaw = ncol(data),

    # 3. What kinds of variables are these? (e.g., real numbers, integers, categorical variables like “city” or “type,” or something else?)
    claseVariablesRaw = lapply(data, function(x) paste(class(x), collapse = ', ')),

    # 4. Are these variables always observed? (i.e., is missing data an issue? If so, how are missing values represented?)
    datosFaltantesRaw = list(
      nObservacionesIncompletas = sum(!complete.cases(data)),
      nVariablesIncompletas = sum(!complete.cases(as.data.frame(t(data)))),
      nRegistrosIncompletos_xVariable = lapply(data, function(x) sum(is.na(x))),
      idxObservacionesIncompletas = !complete.cases(data),
      idxVariablesIncompletas = !complete.cases(as.data.frame(t(data))),
      idxRegistrosIncompletos_xVariable = lapply(data, is.na),
      graphAggregationPlot = VIM::aggr(
        data,
        col = c(
          'skyblue', # Color para los datos presentes
          'red',     # Color para los datos faltantes
          'orange'   # Color para los datos imputados
        ),
        bars = TRUE, # Pequeña barra con las frecuencias de las distintas combinaciones
        numbers = FALSE, # Muestra la proporción de datos faltantes
        prop = TRUE, # Utiliza la proporción (T) o el número absoluto (F)
        combined = FALSE, # Combina (o no) los dos gráficos en uno sólo
        varheight = FALSE, # Altura de las celdas proporcionar al n correspondiente
        only.miss = FALSE, # Si bars = T, muestra la barra sólo para combinaciones con NAs
        border = par("fg"), # Color del borde de las barras
        sortVars = FALSE, # Barras ordenadas (T) o no (F) por el n de NAs
        sortCombs = TRUE, # Combinaciones ordenadas (T) o no (F) pro frecuencia de ocurrencia
        ylabs = NULL,
        axes = TRUE, # Muestra (T) u oculta (F) los ejes
        labels = TRUE, # Muestra (o no) las etiquetas en el eje x
        cex.lab = 1.2,
        cex.axis = par("cex"),
        cex.numbers = par("cex"),
        gap = 4 # Espacio entre los dos gráficos
      )
    )
    # 5. Are the variables included in the dataset the ones we were expecting?
    # 6. Are the values of these variables consistent with what we expect?
    # 7. Do the variables in the dataset seem to exhibit the kinds of relationships we expect? (Indeed, what relationships do we expect, and why?)
  )
}
