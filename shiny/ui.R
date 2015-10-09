library(shiny)
library(shinythemes)

shinyUI(navbarPage("", theme = shinytheme("flatly"),
            navbarMenu("Conocimiento y uso",
                       tabPanel("Top of Mind y espontáneo de marca"),
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
                       tabPanel("Pregunta 14")),
            navbarMenu("Evaluación Publicitaria",
                       tabPanel("Pregunta 15"),
                       tabPanel("Pregunta 16"))
         

))


