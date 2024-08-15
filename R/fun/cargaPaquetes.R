# ilpac - Install and Load Packages
# Fuente - https://towardsdatascience.com/fastest-way-to-install-load-libraries-in-r-f6fd56e3e4c4

# Se elige la opción con apply para evitar una dependencia del paquete pacman

cargaPaquetes <- function(
    libs = defaultPackages # Por defecto, instala las librerías predeterminadas
    ){
  
  ## Definir el vector de paquetes por defecto que se cargarán
  defaultPackages <- c(
    
    # Paquetes por defecto de la instalación de R
    'datasets', # Create Data Frames that are Easier to Exchange and Reuse
    'utils',    # The R utils package
    'grDevices',# The R Graphics Devices and Support for Colours and Fonts
    'graphics', # Data and Functions from the Book R Graphics, Third Edition
    'stats',    # Interactive Document for Working with Basic Statistical Analysis
    'methods',  # Formal Methods and Classes
    
    # Paquetes globales
    'devtools',    # Tools to Make Developing R Packages Easier
    'here',        # A Simpler Way to Find Your Files
    'BiocManager', # Access the Bioconductor Project Package Repository
      
    # Pruebas de hipótesis
    'rstantools',  # Tools for Developing R Packages Interfacing with 'Stan'
    'car',         # Companion to Applied Regression
    'DescTools',   # Tools for Descriptive Statistics
    'ggplot2',     # Create Elegant Data Visualisations Using the Grammar of Graphics
    'ggstatsplot', # 'ggplot2' Based Plots with Statistical Details
    'gmodels',     # Various R Programming Tools for Model Fitting
    'dplyr',       # A Grammar of Data Manipulation
    'rstatix',     # Pipe-Friendly Framework for Basic Statistical Tests
    'WRS2',        # A Collection of Robust Statistical Methods
    'ggpubr'       # 'ggplot2' Based Publication Ready Plots
    )
  
  ## Fase 1.- Instalación
  ### Identificar los paquetes a instalar que no están instalados
  new.libs <- libs[!(libs %in% installed.packages()[, "Package"])]
  
  ### Si el vector new.libs no tiene longitud 0, instalar los paquetes faltantes
  if (length(new.libs))
    install.packages(new.libs, dependencies = TRUE)
  
  
  ## Fase 2.- Carga
  ### Cargar todos los paquetes requeridos
  sapply(libs, require, character.only = TRUE)
  
  ## Fase 3.- Comprobación del proceso
  # 2b. Comprueba que la instalación ha sido correcta
  notInstalledLibs <- new.libs %in% rownames(utils::installed.packages())
  if (sum(notInstalledLibs) > 0) {
    cat('No se han instalado los paquetes: ', libs[!notInstalledLibs])
    cat('\nPor favor, revise las advertencias (warnings) y mensajes de error que ha recibido en consola para saber cómo debe proceder para poder instalarlos.\n')
    stop()
  }else{
    cat('Se han instalado todos los paquetes necesarios')
  }
}

