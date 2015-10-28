library(shiny)
library(shinythemes)


shinyUI(navbarPage("",theme = shinytheme("flatly"),
                   tabPanel("Objetivo",
                            column(12,imageOutput('objetivo'), align="center")),
                   tabPanel("Tipo de producto compró U12",
                            sidebarLayout(
                              sidebarPanel(
                                menu1(),
                                conditionalPanel(condition ="assert:<output.contenido>",
                                                 downloadButton('d1',
                                                                'Descarga la base filtrada')
                                ),
                                width = 2
                              ),
                              mainPanel(
                                h3(textOutput("txt_p2"),align="center"),
                                column(4,
                                       plotOutput("plot_p2",height=1000,width=1300),align="center"
                                )
                              )
                            )
                   ),
                   tabPanel("Conocimiento y uso",
                            sidebarLayout(
                              sidebarPanel(
                                menu2(),
                                conditionalPanel(condition ="assert:<output.contenido>",
                                                 downloadButton('d2',
                                                                'Descarga la base filtrada')
                                ),
                                width = 2
                              ),    
                              mainPanel(
                                tabsetPanel(
                                  tabPanel("Top y share of Mind de marca",
                                           h3(textOutput("txt_p3"),align="center"),
                                           br(),
                                           column(5,plotOutput("plot_p3",height=100,width=1300)),align="center"),
                                  tabPanel("Top y share of Mind de publicidad",
                                           h3(textOutput("txt_p4"),align="center"),
                                           br(),
                                           column(4,plotOutput("plot_p4",height=100,width=1300)),align="center"),
#                                   tabPanel("Guiado de marca",
#                                            h3(textOutput("txt_p5"),align="center"),
#                                            br(),
#                                            column(4,plotOutput("plot_p5_Guiado",height=100,width=1300)),align="center"),
                                  tabPanel("Total Conocimiento - Publicidad",
                                           h3(textOutput("txt_p6"),align="center"),
                                           br(),
                                           column(4,plotOutput("plot_p6_Guiado",height=100,width=1300)),align="center"),
#                                   tabPanel("Consideración de uso",
#                                            h3(textOutput("txt_p7"),align="center"),
#                                            br(),
#                                            column(4,plotOutput("plot_p7",height=100,width=1300)),align="center"),
                                  tabPanel("Uso alguna vez",
                                         h3(textOutput("txt_p10"),align="center"),
                                         br(),
                                         column(4,plotOutput("plot_p10",height=100,width=1300)),align="center"),
                                  tabPanel("Uso U12",
                                           h3(textOutput("txt_p9"),align="center"),
                                           br(),
                                           column(4,plotOutput("plot_p9",height=100,width=1300)),align="center"),
                                  tabPanel("Uso actual",
                                           h3(textOutput("txt_p8"),align="center"),
                                           br(),
                                           column(4,plotOutput("plot_p8",height=100,width=1300)),align="center"),
                                  tabPanel("Uso de mayor frecuencia",
                                           h3(textOutput("txt_p11"),align="center"),
                                           br(),
                                           column(4,plotOutput("plot_p11",height=100,width=1300)),align="center"),
                                  tabPanel("Alternativa de compra",
                                           h3(textOutput("txt_p11a"),align="center"),
                                           br(),
                                           column(4,plotOutput("plot_p11a",height=100,width=1300)),align="center"),
                                  tabPanel("Uso frecuente vs alternativa de compra",
                                           #h3(textOutput("txt_p11a"),align="center"),
                                           br(),
                                           column(4,plotOutput("plot_p11_vs_p11a",height=100,width=1300)),align="center"),
                                  tabPanel("Funnel de Consideración a Total",
                                           column(12,imageOutput('consideraciontotal'),align="center")),
                                  tabPanel("Funnel de Consideración Clientes EKT",
                                           column(10,imageOutput('consideracionekt'),align="center")),
                                  tabPanel("Funnel de Consideración Competencia",
                                           column(10,imageOutput('consideracioncomp'),align="center"))
                                )
                              )
                            )
                   ),
                   tabPanel("Imagen de Marca",
                            sidebarLayout(
                              sidebarPanel(
                                menu3(),
                                conditionalPanel(condition ="assert:<output.contenido>",
                                                 downloadButton('d3',
                                                                'Descarga la base filtrada')
                                ),
                                width = 3
                              ),
                              mainPanel(tabsetPanel(
                                tabPanel('Resultados de la batería',
                                         column(12,plotOutput('plotbateria',height=750),
                                                wellPanel(h2('En esta gráfica se encuentran representados los porcentajes de asociación
                                                             por marca para cada atributo.',align="left"),align="center")
                                                )
                                         ),
                                tabPanel('resumen por dimensión',
                                         column(12,plotOutput('plotimagen',height=750),
                                                wellPanel(h2('En esta imagen se encuentran resumidos los items de la batería en 7
                                                             dimensiones principales. La línea punteada indica el score promedio de la población.'
                                                             ,align="left"),align="center")
                                                )
                                         )
                                
                                )
                              )
                            )
                   ),
                   tabPanel("Equity",
                            sidebarLayout(
                              sidebarPanel(
                                menu4(),
                                conditionalPanel(condition ="assert:<output.contenido>",
                                                 downloadButton('d4',
                                                                'Descarga la base filtrada')
                                ),
                                width = 3
                              ),    
                              mainPanel(
                                tabsetPanel(
                                  tabPanel('Score',
                                           column(9,plotOutput('equitybateria',height=800),
                                                  wellPanel(h2('bla bla equity',align="left"),align="center")
                                           ),
                                           column(3,plotOutput('equityscore',height=800),
                                                  wellPanel(h2('bla bla equity',align="left"),align="center")
                                           )
                                  ),
                                  #column(4,plotOutput("",height=100,width=1300))),
                                  tabPanel("Compra en el futuro",
                                           br(),
                                           br())
                                  #column(4,plotOutput("",height=100,width=1300)))
                                )
                              )
                            )
                   ),
                   tabPanel("Evaluación Publicitaria",
                            sidebarLayout(
                              sidebarPanel(
                                menu5(),
                                conditionalPanel(condition ="assert:<output.contenido>",
                                                 downloadButton('d5',
                                                                'Descarga la base filtrada')
                                ),
                                width = 3
                              ),    
                              mainPanel(
                                tabsetPanel(
                                  tabPanel("Lugares en donde vio publicidad",
                                           br(),
                                           br()),
                                           #column(4,plotOutput("",height=100,width=1300))),
                                  tabPanel("Qué recuerda de la publicidad de EKT",
                                           h3(textOutput("txt_p16_1"),align="center"),
                                           column(12,plotOutput("nube_P16_1"), align = "center")),
                                  tabPanel("Qué ideas le transmitió la publicidad de EKT",
                                           h3(textOutput("txt_p16a_1"),align="center"),
                                           column(12,plotOutput("nube_P16A_1"), align="center")),
                                  tabPanel("Cómo lo hizo sentir con respecto a EKT",
                                           h3(textOutput("txt_p17"),align="center"),
                                           column(4,plotOutput("plot_p17",height=100,width=1300))),
                                  tabPanel("Score comerciales EKT",
                                           br(),
                                           br()),
                                           #column(4,plotOutput("",height=100,width=1300))),
                                  tabPanel("Qué recuerda de la publicidad de Coppel",
                                           h3(textOutput("txt_p19_1"),align="center"),
                                           column(12,plotOutput("nube_P19_1"), align = "center")),
                                  tabPanel("Qué ideas le transmitió la publicidad de Coppel",
                                           h3(textOutput("txt_p19a_1"),align="center"),
                                           column(12,plotOutput("nube_P19A_1"), align = "center")),
                                  tabPanel("Cómo lo hizo sentir con respecto a Coppel",
                                           h3(textOutput("txt_p20"),align="center"),
                                           column(4,plotOutput("plot_p20",height=100,width=1300))),
                                  tabPanel("Score comerciales Coopel",
                                           br(),
                                           br())
                                           #column(4,plotOutput("",height=100,width=1300)))
                                )
                              )
                            )),
                   tabPanel("Conclusiones"),
                   tabPanel("Contacto",
                            column(12,imageOutput('contacto'),align="center"))

))