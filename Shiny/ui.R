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
                                           wellPanel(style = "background-color: white;",
                                                     h4("Se presenta la primer mención espontánea y total menciones espontáneas de 
                                                        conocimiento para cada marca del set competitivo.",
                                                     align="center"),align="center"),
                                           column(5,plotOutput("plot_p3",height=100,width=1300)),align="center"),
                                  tabPanel("Top y share of Mind de publicidad",
                                           h3(textOutput("txt_p4"),align="center"),
                                           br(),
                                           wellPanel(style = "background-color: white;",h4("Se presenta la primer mención
                                           espontánea y total menciones espontáneas de conocimiento publicitario para cada marca
                                           del set competitivo.",align="center"),align="center"),
                                           column(4,plotOutput("plot_p4",height=100,width=1300)),align="center"),
#                                   tabPanel("Guiado de marca",
#                                            h3(textOutput("txt_p5"),align="center"),
#                                            br(),
#                                            column(4,plotOutput("plot_p5_Guiado",height=100,width=1300)),align="center"),
                                  tabPanel("Total Conocimiento - Publicidad",
                                           h3(textOutput("txt_p6"),align="center"),
                                           br(),
                                           wellPanel(style = "background-color: white;",h4("Aquí se incluyen las menciones 
                                           espontáneas y ayudadas de conocimiento publicitario.",align="center"),align="center"),
                                           column(4,plotOutput("plot_p6_Guiado",height=100,width=1300)),align="center"),
#                                   tabPanel("Consideración de uso",
#                                            h3(textOutput("txt_p7"),align="center"),
#                                            br(),
#                                            column(4,plotOutput("plot_p7",height=100,width=1300)),align="center"),
                                  tabPanel("Uso alguna vez",
                                         h3(textOutput("txt_p10"),align="center"),
                                         br(),
                                         wellPanel(style = "background-color: white;",h4("Se muestran todas las tiendas
                                          ocupadas en alguna ocasión,sin importar cuánto tiempo tenga de esto.",align="center"),align="center"),
                                         column(4,plotOutput("plot_p10",height=100,width=1300)),align="center"),
                                  tabPanel("Uso U12",
                                           h3(textOutput("txt_p9"),align="center"),
                                           br(),                                           
                                           wellPanel(style = "background-color: white;",h4("Tiendas en las que los entrevistados compraron
                                           algún artículo de electrónica, línea blanca, telefonía,cómputo y/o muebles dentro de los últimos
                                           12 meses.",align="center"),align="center"),
                                           column(4,plotOutput("plot_p9",height=100,width=1300)),align="center"),
                                  tabPanel("Uso actual",
                                           h3(textOutput("txt_p8"),align="center"),
                                           br(),
                                           wellPanel(style = "background-color: white;",h4("Tiendas en las que los entrevistados compran 
                                           artículo de electrónica,línea blanca, telefonía, cómputo y/o muebles actualmente.",align="center"),align="center"),
                                           column(4,plotOutput("plot_p8",height=100,width=1300)),align="center"),
                                  tabPanel("Uso de mayor frecuencia",
                                           h3(textOutput("txt_p11"),align="center"),
                                           br(),                                           
                                           wellPanel(style = "background-color: white;",h4("Tiendas en las que los entrevistados compran artículo
                                           de electrónica,línea blanca, telefonía, cómputo y/o muebles con mayor frecuencia.",align="center"),align="center"),
                                           column(4,plotOutput("plot_p11",height=100,width=1300)),align="center"),
                                  tabPanel("Alternativa de compra",
                                           h3(textOutput("txt_p11a"),align="center"),
                                           br(),                                          
                                           wellPanel(style = "background-color: white;",h4("Se muestran las tiendas en las que los entrevistados elegirían 
                                           si tuvieran que decidir comprar un artículo en otra tienda que no fuera su tienda más frecuente.",align="center"),align="center"),
                                           column(4,plotOutput("plot_p11a",height=100,width=1300)),align="center"),
                                  tabPanel("Uso frecuente vs alternativa de compra",
                                           #h3(textOutput("txt_p11a"),align="center"),
                                           br(),
                                           wellPanel(style = "background-color: white;",h4("Se muestran las tiendas en las que los entrevistados compran más 
                                           frecuentemente (eje “X”),vs. La tienda en la que comprarían como segunda opción 
                                           (eje “Y”).",align="center"),align="center"),
                                           column(4,plotOutput("plot_p11_vs_p11a",height=100,width=1300)),align="center"),
                                  tabPanel("Funnel de Consideración a Total",
                                           wellPanel(style = "background-color: white;",h4("Se muestra la conversión del mercado desde el conocimiento
                                           de las marcas hasta su uso frecuente.",align="center"),align="center"),
                                           column(12,imageOutput('consideraciontotal'),align="center")),
                                  tabPanel("Funnel de Consideración Clientes EKT",
                                           wellPanel(style = "background-color: white;",h4("Se muestra la conversión de los clientes EKT desde el 
                                           conocimiento de las marcas hasta su uso frecuente.",align="center"),align="center"),
                                           column(10,imageOutput('consideracionekt'),align="center")),
                                  tabPanel("Funnel de Consideración Competencia",
                                           wellPanel(style = "background-color: white;",h4("Se muestra la conversión de los clientes de la competencia
                                           desde el conocimiento de las marcas hasta su uso frecuente. Considerar en su lectura que hay en esta muestra
                                           una mezcla de usuarios de tiendas de la competencia.",align="center"),align="center"),
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
                                width = 2
                              ),
                              mainPanel(tabsetPanel(
                                tabPanel('Resultados de la batería',
                                         wellPanel(style = "background-color: white;",h4("Se muestra la percepción de las compañías por parte de 
                                         los usuarios. El porcentaje representa el número de asignaciones de un atributo a la marca. ",align="center"),align="center"),
                                         column(12,plotOutput('plotbateria',height=750),
                                                wellPanel(style = "background-color: white;",h4('En esta gráfica se encuentran representados los porcentajes de asociación
                                                                                         por marca para cada atributo.',align="left"),align="center")
                                               
                                                )
                                         ),
                                tabPanel('Resumen por dimensión',
                                         wellPanel(style = "background-color: white;",h4("Se muestra la percepción de las marcas por parte de los usuarios. 
                                         En este caso en 7 dimensiones que representan atributos agrupados mediante factorización y que fueron nombrados de 
                                          acuerdo a lo que representan. La línea punteada representa el score promedio de los consultados.",
                                         align="left"),align="center"),
                                         column(12,plotOutput('plotimagen',height=750)

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
                                width = 2
                              ),    
                              mainPanel(
                                tabsetPanel(
                                  tabPanel('Score',
                                           wellPanel(style = "background-color: white;",h4("Se muestra el score de brand equity (valor de la marca)
                                           calculado a partir de los atributos principales que dan peso a una marca en la mente del consumidor.",
                                           align="center"),align="center"),
                                           column(9,plotOutput('equitybateria',height=800)
                                           ),
                                           column(3,plotOutput('equityscore',height=800)
                                           )
                                  )
                                  #column(4,plotOutput("",height=100,width=1300))),
#                                   tabPanel("Compra en el futuro",
#                                            br(),
#                                            br())
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
                                width = 2
                              ),    
                              mainPanel(
                                tabsetPanel(
                                  tabPanel("Lugares en donde vio publicidad",
                                           wellPanel(style = "background-color: white;",h4("Se presentan los medios donde los entrevistados recuerdan 
                                            haber visto, leído o escuchado publicidad de las marcas que conoce, el alcance y frecuencia declarado de 
                                            cada medio.",align="center"),align="center"),
                                           column(10,
                                                  plotOutput('graficapublicidad',height=750,width=1050)
                                                  ),
                                           column(2,
                                                  wellPanel(
                                                    selectInput(
                                                      'marca', 
                                                      label = h4('Marca'),
                                                      choices = list(
                                                        'Elektra' = 'Elektra',
                                                        'Coppel' = 'Coppel',
                                                        'Famsa' = "Famsa",
                                                        'Bodega' = 'Bodega',
                                                        "Walmart" = "Walmart",
                                                        "Liverpool" = "Liverpool"),
                                                      selected='Elektra'),
                                                    checkboxGroupInput(
                                                      'medios', 
                                                      label = h4('Medios'),
                                                      choices = list(
                                                        'Televisión' = 1,
                                                        'Radio' = 2,
                                                        'Revistas' = 3,
                                                        'Periódicos' = 4,
                                                        "Espectaculares" = 5,
                                                        "Paradas y posters" = 6,
                                                        "Vallas y bardas" =7,
                                                        "Cines" = 8,
                                                        "Autobuses y metro" = 9,
                                                        "Internet" = 10,
                                                        "Centros comerciales" = 11,
                                                        "Redes sociales" = 12,
                                                        "Otro" = 13),
                                                      selected = c(1,2,3,4,5))
                                                  ))
                                           ),
                                           #column(4,plotOutput("",height=100,width=1300))),
                                  tabPanel("Qué recuerda de la publicidad de EKT",
                                           wellPanel(style = "background-color: white;",h4("La nube representa la importancia de las palabras 
                                           mencionadas por cada uno de los entrevistados con respecto a lo que recuerdan
                                           o sienten respecto a la marca, entre mayor sea el tamaño es mayor el porcentaje que dicha palabra fue mencionada y asociada a EKT.",
                                           align="center"),align="center"),
                                           h3(textOutput("txt_p16_1"),align="center"),
                                           column(12,plotOutput("nube_P16_1"), align = "center")),
                                  tabPanel("Qué ideas le transmitió la publicidad de EKT",
                                           wellPanel(style = "background-color: white;",h4("La nube representa la importancia de las palabras
                                           mencionadas por cada uno de los entrevistados con respecto a lo que recuerdan o
                                           sienten respecto a la marca, entre mayor sea el tamaño es mayor el porcentaje que dicha palabra 
                                           fue mencionada y asociada a EKT.",align="center"),align="center"),
                                           h3(textOutput("txt_p16a_1"),align="center"),
                                           column(12,plotOutput("nube_P16A_1"), align="center")),
                                  tabPanel("Cómo lo hizo sentir con respecto a EKT",
                                           wellPanel(style = "background-color: white;",h4("De los entrevistados que recuerdan 
                                           publicidad de EKT, se muestra si cambió para bien o para mal su percepción de la marca. ",
                                           align="center"),align="center"),
                                           h3(textOutput("txt_p17"),align="center"),
                                           column(4,plotOutput("plot_p17",height=100,width=1300))),
                                  tabPanel("Score comerciales",
                                           plotOutput('graficascorepublicidad',height=600)),
                                           #column(4,plotOutput("",height=100,width=1300))),
                                  tabPanel("Qué recuerda de la publicidad de Coppel",
                                           wellPanel(style = "background-color: white;",h4("La nube representa la importancia de las palabras mencionadas por 
                                           cada uno de los entrevistados con respecto a lo que recuerdan o sienten respecto a la marca, 
                                           entre mayor sea el tamaño es mayor el porcentaje que dicha palabra fue mencionada y asociada a Coppel.",
                                           align="center"),align="center"),
                                           h3(textOutput("txt_p19_1"),align="center"),
                                           column(12,plotOutput("nube_P19_1"), align = "center")),
                                  tabPanel("Qué ideas le transmitió la publicidad de Coppel",
                                           wellPanel(style = "background-color: white;",h4("La nube representa la importancia de las
                                           palabras mencionadas por cada uno de los entrevistados con respecto a lo que recuerdan o sienten 
                                           respecto a la marca, entre mayor sea el tamaño es mayor el porcentaje que dicha palabra fue mencionada
                                           y asociada a Coppel.",align="center"),align="center"),
                                           h3(textOutput("txt_p19a_1"),align="center"),
                                           column(12,plotOutput("nube_P19A_1"), align = "center")),
                                  tabPanel("Cómo lo hizo sentir con respecto a Coppel",
                                           wellPanel(style = "background-color: white;",h4("De los entrevistados que recuerdan publicidad de 
                                           Coppel, se muestra si cambió para bien o para mal su percepción de la marca ",
                                           align="center"),align="center"),
                                           h3(textOutput("txt_p20"),align="center"),
                                           column(4,plotOutput("plot_p20",height=100,width=1300))),
                                  tabPanel("Recordación de Contenidos EKT",
                                           h3(textOutput("txt_open2"),align="center"), fluidPage(
                                                   column(12,
                                                          wellPanel(style = "background-color: white;",align="center",tags$ul(
                                                                  tags$li("Correctos y específicos: Recordación puntual de elementos contenidos.",
                                                                          style = "color:black;font-size:10pt",align="justify"),
                                                                  tags$li("Genéricos: Comunes a la marca, ej: Slogan",
                                                                          style = "color:black;font-size:10pt",align="justify"),
                                                                  tags$li("Comunes a la categoría: Podrían ser asignados a cualquier marca de la categoría.",
                                                                          style = "color:black;font-size:10pt",align="justify"),
                                                                  tags$li("Incorrectos: Elementos que no salen en la publicidad o corresponden a otra marca.",
                                                                          style = "color:black;font-size:10pt",align="justify"),
                                                                  tags$li("No identificados: No se encuentra material para afirmar o negar que estas menciones corresponden a la publicidad de la marca.",
                                                                          style = "color:black;font-size:10pt",align="justify"))),align="rigth")),
                                           wellPanel(style = "background-color: white;",h4("Menciones menores al 2% no se muestran en
                                           el siguiente listado",align="center"),align="center"),
                                           h2(textOutput("text2"),align="center",style = "background-color: #2c3e50;color:white"),
                                           column(12,tableOutput('table2'), align = "center",style = "background-color:white"),
                                            h2(textOutput("text3"),align="center",style = "background-color: #2c3e50;color:white"),
                                           column(12,tableOutput('table4'), align = "center",style = "background-color:white"),
                                           h2(textOutput("text4"),align="center",style = "background-color: #2c3e50;color:white"),
                                           column(12,tableOutput('table5'), align = "center",style = "background-color:white"),
                                            h2(tableOutput("text5"),align="center",style = "background-color: #2c3e50;color:white"),
                                           # column(12,tableOutput('table10'), align = "center",style = "background-color:white;color:red"),
                                          h2(textOutput("text11"),align="center",style = "background-color: #2c3e50;color:white"),
                                          h2(textOutput("text12"),align="center",style = "background-color: #2c3e50;color:white")),
                                  
                                  tabPanel("Recordación de Contenidos Coppel",
                                           h3(textOutput("txt_open1"),align="center"),
                                           fluidPage(
                                                   column(12,
                                                          wellPanel(style = "background-color: white;",align="center",tags$ul(
                                                                  tags$li("Correctos y específicos: Recordación puntual de elementos contenidos.",
                                                                          style = "color:black;font-size:10pt",align="justify"),
                                                                  tags$li("Genéricos: Comunes a la marca, ej: Slogan",
                                                                                  style = "color:black;font-size:10pt",align="justify"),
                                                                  tags$li("Comunes a la categoría: Podrían ser asignados a cualquier marca de la categoría.",
                                                                          style = "color:black;font-size:10pt",align="justify"),
                                                                  tags$li("Incorrectos: Elementos que no salen en la publicidad o corresponden a otra marca.",
                                                                          style = "color:black;font-size:10pt",align="justify"),
                                                                  tags$li("No identificados: No se encuentra material para afirmar o negar que estas menciones corresponden a la publicidad de la marca.",
                                                                          style = "color:black;font-size:10pt",align="justify"))),align="rigth")),
                                           wellPanel(style = "background-color: white;",h4("Menciones menores al 2% no se muestran en
                                           el siguiente listado",align="center"),align="center"),
                                           h2(textOutput("text6"),align="center",style = "background-color: #2c3e50;color:white"),
                                           column(12,tableOutput('table6'), align = "center",style = "background-color:white"),
                                           
                                           h2(textOutput("text7"),align="center",style = "background-color: #2c3e50;color:white"),
                                           column(12,tableOutput('table7'), align = "center",style = "background-color:white"),
                                           
                                           h2(textOutput("text8"),align="center",style = "background-color: #2c3e50;color:white"),
                                           column(12,tableOutput('table8'), align = "center",style = "background-color:white"),
                                           h2(textOutput("text9"),align="center",style = "background-color: #2c3e50;color:white"),
                                           h2(textOutput("text20"),align="center",style = "background-color: #2c3e50;color:white"))
                                           # column(12,tableOutput('table9'), align = "center",style = "background-color:white;color:red"))
                                           
                                  # column(12,tableOutput('table6'), align = "center")),
                                  
                                           #column(4,plotOutput("",height=100,width=1300)))
                                )
                              )
                            )),
                   tabPanel("Conclusiones",
                     fluidPage(
                        column(10,
                                wellPanel(style = "background-color: #2c3e50;",align="center",
                               h2(strong('Conclusiones Generales',style = "color:#C2D1E0")),
                                tags$ul(
                                tags$li("Los líderes son Elektra y Coppel a nivel conocimiento y uso, dentro de 
                                  una categoría muy grande donde existen muchos competidores.",style = "color:#C2D1E0;font-size:18pt",align="justify"),
                                tags$li("Elektra logra un ligero mejor desempeño en términos de recordación publicitaria dentro del Top of Mind, 
                                  sin embargo a Total Conocimiento Elektra y Coppel son las marcas con mejor recordación publicitaria del set competitivo. 
                                  Por lo anterior, son las marcas asociadas con un mayor número de atributos en imagen.",
                                  style = "color:#C2D1E0;font-size:18pt",align="justify"),
                                tags$li("Los clientes de Coppel consideran a EKT como una segunda opción, al igual que los clientes EKT 
                                  consideran a Coppel como su segunda opción, en menor proporción, pero también se encuentran en 
                                  su set de consideración Bodega Aurrera y Walmart.",style = "color:#C2D1E0;font-size:18pt",align="justify"),
                                tags$li("En el funnel de consideración a total,  observamos que casi una tercera parte de los clientes de EKT 
                                  también han comprado en Coppel en los últimos 12 meses .",style = "color:#C2D1E0;font-size:18pt",align="justify"),
                                tags$li("Dentro de nuestro Target, muy pocos compran en Liverpool, sin embargo los que lo han llegado a comprar alguna vez, 
                                  la mitad de estos han comprado en los últimos 12 meses y lo vuelve su tienda más frecuente 
                                  (una vez que prueban, se quedan)",style = "color:#C2D1E0;font-size:18pt",align="justify"),
                                tags$li("Observamos que Coppel se muestra con una imagen más sólida y dirigida a su target, su imagen es plenamente identificada
                                  y no es confundida por la competencia, salvo en los atributos de intereses bajos en compras a crédito y 
                                  buenas experiencias en sus compras, pues son atributos que
                                  comparten con EKT",style = "color:#C2D1E0;font-size:18pt",align="justify"),
                                tags$li("Por su parte EKT es una marca que es identificada por la cercanía y la disponibilidad de sus horario, sin embargo no
                                  es reconocida por ofrecer buenas promociones, atención rápida, tiendas amplias 
                                  y acomodo de productos adecuado.",style = "color:#C2D1E0;font-size:18pt",align="justify"),
                                tags$li("Liverpool es bien valorado por su instalaciones atractivas y  tienda de prestigio a total,
                                  sin embargo dentro de los clientes EKT no le reconocen estos dos atributos.",style = "color:#C2D1E0;font-size:18pt"
                                  ,align="justify"),
                                tags$li("Los clientes de competencia le reconocen más a Elektra la cercanía, variedad de productos, 
                                  variedad de marcas y la disponibilidad de horario; sin embargo no le atribuyen el ofrecer instalación 
                                  de productos /mercancía, buenas promociones, personal amable, instalaciones atractivas, 
                                  acomodo de los productos adecuado, precios accesibles de contado y 
                                  atención rápida",style = "color:#C2D1E0;font-size:18pt",align="justify"),
                                tags$li("El Brand Equity para Elektra con clientes de competencia es muy bajo 6.2%,  
                                  sin embargo dentro de los usuarios de EKT hay una clara mejor 
                                  percepción de Equity.",style = "color:#C2D1E0;font-size:18pt",align="justify"),
                                tags$li("La recordación espontánea se centra en elementos muy generales, tiene menciones específicas un tanto pulverizadas, 
                                  no se logran verbalizaciones tan detalladas.",style = "color:#C2D1E0;font-size:18pt",align="justify"),
                                tags$li("Uno de los elementos más recordados son los abonos chiquitos 
                                  y la tienda Elektra",style = "color:#C2D1E0;font-size:18pt",align="justify"),
                                tags$li("Históricamente se ha percibido una saturación de mensajes similares ,
                                  no solamente de la categoría retail sino también de otros productos,
                                  incluyendo financieros.",style = "color:#C2D1E0;font-size:18pt",align="justify"),
                                tags$li("Los atributos más asociados a la marca son Interesante e Identificación",style = "color:#C2D1E0;font-size:18pt",
                                  align="justify"))
),align="center"))),
                   tabPanel("Contacto",
                            column(12,imageOutput('contacto'),align="center"))

))
