correlationAnalysis <- function( 
  ## Test de hipótesis para la Correlación Agriculture-Examination
  var_x,
  var_y,
  meaning_x_sp = 'Descripción variable X',
  meaning_y_sp = 'Descripción variable Y',
  meaning_x_en = 'Description variable X',
  meaning_y_en = 'Description variable Y', 
  alternative = c("two.sided", "less", "greater"),
  metodo = c("pearson", "kendall", "spearman"),
  exacto = NULL,
  interv.confianza = 0.95,
  continuidad = FALSE
  ){
  eval(
    parse(
      text = paste0(
        'corTest <<- cor.test(',
        'x = var_x, ',
        'y = var_y, ',
        'method = metodo, ', 
        'exact = exacto, ',
        'conf.level = interv.confianza, ',
        'continuity = continuidad',
        ')'
      )
    )
  )
  
  corTest$interpretacion <<- list(
    existeCorrelacion_sp = dplyr::case_when(
      corTest$p.value >= 0.05 ~ 'No existe',
      corTest$p.value <  0.05 ~ 'Existe' 
    ),
    existeCorrelacion_en = dplyr::case_when(
      corTest$p.value >= 0.05 ~ 'There was no',
      corTest$p.value <  0.05 ~ 'There was' 
    ),
    sentidoCorrelacion_sp = dplyr::case_when(
      sign(corTest$estimate) ==  1 ~ 'positiva',
      sign(corTest$estimate) == -1 ~ 'negativa'
    ),
    sentidoCorrelacion_en = dplyr::case_when(
      sign(corTest$estimate) ==  1 ~ 'positive',
      sign(corTest$estimate) == -1 ~ 'negative'
    ),
    fuerzaCorrelacion_sp = dplyr::case_when(
      abs(corTest$estimate) == 1                                  ~ 'perfecta',
      abs(corTest$estimate) <  1   & abs(corTest$estimate) >= 0.8 ~ 'muy fuerte',
      abs(corTest$estimate) <  0.8 & abs(corTest$estimate) >= 0.6 ~ 'fuerte',
      abs(corTest$estimate) <  0.6 & abs(corTest$estimate) >= 0.4 ~ 'moderada',
      abs(corTest$estimate) <  0.4 & abs(corTest$estimate) >= 0.2 ~ 'débil',
      abs(corTest$estimate) <  0.2 & abs(corTest$estimate) >= 0   ~ 'muy débil o inexistente'
    ),
    fuerzaCorrelacion_en = dplyr::case_when(
      abs(corTest$estimate) == 1                                  ~ 'perfect',
      abs(corTest$estimate) <  1   & abs(corTest$estimate) >= 0.8 ~ 'very strong',
      abs(corTest$estimate) <  0.8 & abs(corTest$estimate) >= 0.6 ~ 'strong',
      abs(corTest$estimate) <  0.6 & abs(corTest$estimate) >= 0.4 ~ 'moderate',
      abs(corTest$estimate) <  0.4 & abs(corTest$estimate) >= 0.2 ~ 'weak',
      abs(corTest$estimate) <  0.2 & abs(corTest$estimate) >= 0   ~ 'very weak or inexistent'
    ),
    significacionCorrelacion = dplyr::case_when(
      corTest$p.value <  0.001                            ~ 'p < .001',
      corTest$p.value <  0.01 & corTest$p.value >=  0.001 ~ 'p < .01',
      corTest$p.value <  0.05 & corTest$p.value >=  0.01  ~ 'p < .05',
      corTest$p.value >= 0.05                             ~ paste0(
        'p = ', 
        round(corTest$p.value, 3)
      )
    )
  )
  
  corTest$interpretacion$testApa <<- paste0(
    '($\\hat\\rho$ (', 
    nrow(swiss) - 2,
    ') = ', 
    round(corTest$estimate, 2),
    ', ',
    corTest$interpretacion$significacionCorrelacion,
    ')'
  )  

    corTest$interpretacion$texto_sp <- dplyr::case_when(
      corTest$interpretacion$existeCorrelacion_sp == 'Existe' ~ paste0(
        'Se ha calculado un test de ',
        stringr::str_to_sentence(metodo),
        ' para valorar la correlación entre la variable *',
        eval(parse(text = 'meaning_x_sp')),
        '* y la variable *',
        eval(parse(text = 'meaning_y_sp')),
        '*.\n\n',
        corTest$interpretacion$existeCorrelacion_sp,
        ' una correlación significativa, ',
        corTest$interpretacion$sentidoCorrelacion_sp,
        ' y ',
        corTest$interpretacion$fuerzaCorrelacion_sp,
        ' entre ambas variables ',
        corTest$interpretacion$testApa,
        '.\n'
      ) ,
    corTest$interpretacion$existeCorrelacion_sp == 'No existe' ~ paste0(
        'Se ha calculado un test de ',
        stringr::str_to_sentence(metodo),
        ' para valorar la correlación entre la variable *',
        eval(parse(text = 'meaning_x_sp')),
        '* y la variable *',
        eval(parse(text = 'meaning_y_sp')),
        '*.\n\n',
        corTest$interpretacion$existeCorrelacion_sp,
        ' una correlación significativa entre ambas variables ',
        corTest$interpretacion$testApa,
        '.\n'
      )
    )
  
  corTest$interpretacion$texto_en <- dplyr::case_when(
      corTest$interpretacion$existeCorrelacion_en == 'There was' ~ 
    paste0(
    stringr::str_to_sentence(metodo),
    '’s rank correlation was computed to assess the relationship between variable *',
    eval(parse(text = 'meaning_x_en')),
    '* and variable *',
    eval(parse(text = 'meaning_y_en')),
    '*.\n\n',
    corTest$interpretacion$existeCorrelacion_en,
    ' a significative, ',
    corTest$interpretacion$sentidoCorrelacion_en,
    ' and ',
    corTest$interpretacion$fuerzaCorrelacion_en,
    ' correlation between the two variables ',
    corTest$interpretacion$testApa,
    '.\n'
  ),
  corTest$interpretacion$existeCorrelacion_en == 'There was no' ~
    paste0(
    stringr::str_to_sentence(metodo),
    '’s rank correlation was computed to assess the relationship between variable *',
    eval(parse(text = 'meaning_x_en')),
    '* and variable *',
    eval(parse(text = 'meaning_y_en')),
    '*.\n\n',
    corTest$interpretacion$existeCorrelacion_en,
    ' significative correlation between the two variables ',
    corTest$interpretacion$testApa,
    '.\n'
  )
  )
  
  corTest <<- corTest
}