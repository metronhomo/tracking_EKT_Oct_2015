library(shiny)
library(shinythemes)

shinyUI(navbarPage("Tracking de marca", theme = shinytheme("flatly"), 
                   tabPanel("Objetivo del estudio"),
                   tabPanel("Tipo de producto compró U12"),
                   navbarMenu("Conocimiento y uso",
                              tabPanel("Top of Mind y espontáneo de marca",
                                       fluidPage(
                                         column(
                                           10,
                                           menu(),
                                           column(
                                             10,
                                             plotOutput("",height=1000,width=1300),
                                             align="center",
                                             downloadButton('downloadFile', 'Descargar archivo'))
                                         ))),
                              tabPanel("Top of mind y espontáneo de publicidad",
                                       menu()),
                              tabPanel("Conocimiento de logos",
                                       menu()),
                              tabPanel("Recordación de marca",
                                       menu()),
                              tabPanel("Consideración de marca",
                                       menu()),
                              tabPanel("Compras recientes",
                                       menu()),
                              tabPanel("Compras anteriores",
                                       menu()),
                              tabPanel("Compras frecuentes",
                                       menu()),
                              tabPanel("Segundas opciones de compra",
                                       menu()),
                              tabPanel("Satisfacción",
                                       menu())
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