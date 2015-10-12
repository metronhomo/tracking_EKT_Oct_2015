fluidPage(
  column(
    3, wellPanel(
      helpText('Selecciona las variables sobre las que quieres filtrar.'),
      checkboxGroupInput(
        'filtroEdad', 
        label = h4('Edad'),
        choices = list(
          '25 - 35' = '25-35',
          '36 - 55' = '36-55'),
        selected = c('25-35', '36-55')),
      checkboxGroupInput(
        'filtroGen', 
        label = h4('Género'),
        choices = list(
          'Masculino' = 'M',
          'Femenino' = 'F'),
        selected = c('M', 'F')),
      checkboxGroupInput(
        'filtroNiv', 
        label = h4('Nivel'),
        choices = list(
          'C-' = 'C-',
          'D+' = 'D+',
          'D' = 'D'),
        selected = c(
          'C-',
          'D+',
          'D')),
      checkboxGroupInput(
        'filtroTipoCliente', 
        label = h4('Tipo de cliente'),
        choices = list(
          'Frecuente de Elektra' = 'El',
          'Frecuente de la competencia' = 'Comp'),
        selected = c('El', 'Comp')),
      checkboxGroupInput(
        'filtroTipoProducto', 
        label = h4('Tipo de producto que compró en los últimos 12 meses'),
        choices = list(
          'Cómputo' = 'C',
          'Electrónica' = 'E',
          'Línea blanca' = 'LB',
          'Muebles' = 'M',
          'Telefonía' = 'T'),
        selected = c(
          'C',
          'E',
          'LB',
          'M',
          'T')),
      submitButton(text = "Graficar"),
      wellPanel(
        helpText("Selecciona la variable con la que quieres cruzar."),
        selectInput(
          "variable",
          label = "",
          choices = list(
            "Total de cuartos en el hogar",
            "Número de baños",
            "Regadera funcional",
            "Número de focos en la vivienda",
            "Tipo de piso",
            "Número de automóviles propios",
            "Estufa de gas o eléctrica",
            "Último año de estudios de jefe del hogar"
          )
        )
      )
    )
  )
)