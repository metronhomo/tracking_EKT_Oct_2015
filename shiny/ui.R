library(shiny)
library(shinythemes)

shinyUI(navbarPage("Tracking de marca", theme = shinytheme("flatly"), 
            tabPanel("Objetivo del estudio"),
            navbarMenu("Conocimiento y uso",
                       tabPanel("Top of Mind y espontáneo de marca",
                                fluidPage(
                                  column(
                                    10,
                                    source('menu_graficas.R'),
                                  column(
                                    10,
                                    plotOutput("descriptivo",height=1000,width=1300),
                                    align="center",
                                    downloadButton('downloadFile', 'Descargar archivo'))
                                  ))),
                       tabPanel("Top of mind y espontáneo de publicidad",
                                source('menu_graficas.R')),
                       tabPanel("Conocimiento de logos",
                                source('menu_graficas.R')),
                       tabPanel("Recordación de marca",
                                source('menu_graficas.R')),
                       tabPanel("Consideración de marca",
                                source('menu_graficas.R')),
                       tabPanel("Compras recientes",
                                source('menu_graficas.R')),
                       tabPanel("Compras anteriores",
                                source('menu_graficas.R')),
                       tabPanel("Compras frecuentes",
                                source('menu_graficas.R')),
                       tabPanel("Segundas opciones de compra",
                                source('menu_graficas.R')),
                       tabPanel("Satisfacción",
                                source('menu_graficas.R'))
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
                       tabPanel("Afirmaciones sobre comercial")),
            tabPanel("Conclusiones"),
            tabPanel("Contacto",
                     column(12,imageOutput('contacto'),align="center"))
         

))


