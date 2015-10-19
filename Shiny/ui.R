library(shiny)
library(shinythemes)

shinyUI(navbarPage("",theme = shinytheme("flatly"),
                   tabPanel("Objetivo"),
                   tabPanel("Tipo de producto compró U12",
                            sidebarLayout(
                              sidebarPanel(
                                menu1(),
                                conditionalPanel(condition ="assert:<output.contenido>",
                                                 downloadButton('d1',
                                                                'Descarga la base filtrada')
                                )
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
                                )
                              ),    
                              mainPanel(
                                tabsetPanel(
                                  tabPanel("Top of Mind y espontáneo de marca",
                                           h3(textOutput("txt_p3"),align="center"),
                                           br(),
                                           column(4,plotOutput("plot_p3",height=100,width=1300)),align="center"),
                                  tabPanel("Top of mind y espontáneo de publicidad",
                                           h3(textOutput("txt_p4"),align="center"),
                                           br(),
                                           column(4,plotOutput("plot_p4",height=100,width=1300)),align="center"),
                                  tabPanel("Conocimiento de logos",
                                           h3(textOutput("txt_p5"),align="center"),
                                           br(),
                                           column(4,plotOutput("plot_p5_Guiado",height=100,width=1300)),align="center"),
                                  tabPanel("Recordación de marca",
                                           h3(textOutput("txt_p6"),align="center"),
                                           br(),
                                           column(4,plotOutput("plot_p6_Guiado",height=100,width=1300)),align="center"),
                                  tabPanel("Consideración de marca",
                                           h3(textOutput("txt_p7"),align="center"),
                                           br(),
                                           column(4,plotOutput("plot_p7",height=100,width=1300)),align="center"),
                                  tabPanel("Compras recientes",
                                           h3(textOutput("txt_p8"),align="center"),
                                           br(),
                                           column(4,plotOutput("plot_p8",height=100,width=1300)),align="center"),
                                  tabPanel("Compras anteriores",
                                           h3(textOutput("txt_p9"),align="center"),
                                           br(),
                                           column(4,plotOutput("plot_p9",height=100,width=1300)),align="center"),
                                  tabPanel("Compras frecuentes",
                                           h3(textOutput("txt_p10"),align="center"),
                                           br(),
                                           column(4,plotOutput("plot_p10",height=100,width=1300)),align="center"),
                                  tabPanel("Segundas opciones de compra",
                                           h3(textOutput("txt_p11"),align="center"),
                                           br(),
                                           column(4,plotOutput("plot_p11",height=100,width=1300)),align="center"),
                                  tabPanel("Satisfacción",
                                           h3(textOutput("txt_p11a"),align="center"),
                                           br(),
                                           column(4,plotOutput("plot_p11a",height=100,width=1300)),align="center")
                                  
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
                               )
                             ),
                             mainPanel(
                               #column(4,
                              #        plotOutput("plot_p2",height=1000,width=1300)
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
                                )
                              ),    
                              mainPanel(
                                tabsetPanel(
                                  tabPanel("Score",
                                           br(),
                                           br()),
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
                                )
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