library(shiny)
library(shinythemes)

shinyUI(navbarPage("", theme = shinytheme("flatly"),
                   fluidPage(
                     column(
                       3, wellPanel(
                         helpText('Selecciona las variables sobre las que quieres filtrar.'),
                         radioButtons(
                           'filtroEdad', 
                           label = h4('Edad'),
                           choices = list(
                             '25 - 35' = '25-35',
                             '36 - 55' = '36-55',
                             'Todos' = 'Todos')),
                         radioButtons(
                           'filtroGen', 
                           label = h4('Género'),
                           choices = list(
                             'Masculino' = 'M',
                             'Femenino' = 'F',
                             'Todos' = 'TODOS')),
                         radioButtons(
                           'filtroNiv', 
                           label = h4('Nivel'),
                           choices = list(
                             'AB' = 'AB',
                             'C+' = 'C+',
                             'C' = 'C',
                             'C-' = 'C-',
                             'D+' = 'D+',
                             'D' = 'D',
                             'E' = 'E',
                             'Todos' = 'TODOS')),
                         radioButtons(
                           'filtroTipoCliente', 
                           label = h4('Tipo de cliente'),
                           choices = list(
                             'Frecuente de Elektra' = 'El',
                             'Frecuente de la competencia' = 'Comp',
                             'Todos' = 'TODOS')),
                         radioButtons(
                           'filtroTipoProducto', 
                           label = h4('Tipo de producto que compró en los últimos 12 meses'),
                           choices = list(
                             'Cómputo' = 'C',
                             'Electrónica' = 'E',
                             'Línea blanca' = 'LB',
                             'Muebles' = 'M',
                             'Telefonía' = 'T',
                             'Todos' = 'TODOS'))
                       )
                     )
                   ),
                   
            navbarMenu("Conocimiento y uso",
                       tabPanel("Top of Mind y espontáneo de marca"
                                
                              ),
                       tabPanel("Top of mind y espontáneo de publicidad"),
                       tabPanel("Conocimiento de logos"),
                       tabPanel("Recordación de marca"),
                       tabPanel("Consideración de marca"),
                       tabPanel("Compras recientes"),
                       tabPanel("Compras anteriores"),
                       tabPanel("Compras frecuentes"),
                       tabPanel("Segundas opciones de compra"),
                       tabPanel("Satisfacción")
                       ),
            tabPanel("Medidas de imagen de las marcas"),
            navbarMenu("Equity",
                       tabPanel("Pregunta 13"),
                       tabPanel("Pobabilidad de compra en tiendas")),
            navbarMenu("Evaluación Publicitaria",
                       tabPanel("Lugares donde recuerda haner visto publicidad"),
                       tabPanel("Recordación de publicidad de Elektra"),
                       tabPanel("Principales ideas de comercial de Elektra"),
                       tabPanel("Sentimiento basado en el comercial"),
                       tabPanel("Afirmaciones sobre comercial"))
         

))


