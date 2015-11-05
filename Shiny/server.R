# server.R

library(shiny)


shinyServer(function(input, output,session){
  
  #Tipo de producto U12 meses (P2)-----------
  
  #Información de contacto-----------
  output$objetivo<-renderImage({
    list(src='images/objetivo.png',
         filetype='image/png',
         alt='wiiii')
  }, 
  deleteFile = F)
  
  
  output$d1 <- downloadHandler(
    filename = "Base_Filtrada.csv",
    content = function(file){
      tabla2<-filtro(p2,
                     input$filtroEdad1,
                     input$filtroGen1,
                     input$filtroNiv1,
                     input$filtroTipoCliente1,
                     unique(df$P2_1),
                     input$facet1)
      tabla3<-tabla2[,c('Genero','Edad','NIVEL','Tipo_Cliente')]
      write.csv(tabla3,file,row.names=F)
    }
  )
  
  
  data_p2 <- reactive({
    a <- filtro(p2,
                input$filtroEdad1,
                input$filtroGen1,
                input$filtroNiv1,
                input$filtroTipoCliente1,
                unique(df$P2_1),
                input$facet1)
    if(class(a) != "list") {
      if(nrow(a) == 0) b = NULL
      else b <- cuenta(a[,c("P2_1","P2_2","P2_3","P2_4","P2_5", "F_3")],2)
    }
    else{
      if(length(a) == 0) b = NULL
      else {
        b <- lapply(a, function(x)  cuenta(x[,c("P2_1","P2_2","P2_3","P2_4","P2_5", "F_3")],2))
        d <- lapply(1:length(b), function(i) {
          d <- data.frame(Nivel = names(b)[i], b[[i]])
        })
        b <- rbind_all(d)
      }
      return(b)
    }
  })
  
  output$plot_p2 <- renderPlot({
    graphdata <- data_p2()
    verifica_checkbox(graphdata, 1, 
                      input$filtroEdad1,
                      input$filtroGen1,
                      input$filtroNiv1,
                      input$filtroTipoCliente1,
                      unique(df$P2_1))
    grafica_cuenta(graphdata, input$facet1)
  }, 
  height = 900, 
  width = 1000)
  
  tb_p2<-reactive({
    b <- tam_base(p2,
                  input$filtroEdad1,
                  input$filtroGen1,
                  input$filtroNiv1,
                  input$filtroTipoCliente1,
                  unique(df$P2_1))
    return(b)
  })
  
  output$txt_p2 <- renderText(paste("Tamaño de base: ", tb_p2()[1],
                                    ". Esto es un tamaño de ",tb_p2()[3],".",sep=""))
  
  ###### Pregunta 3
  
  data_p3_top <- reactive({
    genera_data_p(p3_top, 
                  c("P3_Top", "F_3"), 
                  1,
                  input$filtroEdad2,
                  input$filtroGen2,
                  input$filtroNiv2,
                  input$filtroTipoCliente2,
                  input$filtroTipoProducto2,
                  input$facet2)
  })
  
  data_p3_share <- reactive({
    genera_data_p(p3_share, c(11:22), 2,
                  input$filtroEdad2,
                  input$filtroGen2,
                  input$filtroNiv2,
                  input$filtroTipoCliente2,
                  input$filtroTipoProducto2,
                  input$facet2)
  })
  
  tb_p3 <- reactive({
    b <- tam_base(p3_top,
                  input$filtroEdad2,
                  input$filtroGen2,
                  input$filtroNiv2,
                  input$filtroTipoCliente2,
                  input$filtroTipoProducto2)
    return(b)
  })
  
  output$txt_p3 <- renderText(paste("Tamaño de base: ", tb_p3()[1],
                                    ". Esto es un tamaño de ",tb_p3()[3],".",sep=""))
  
  output$plot_p3 <- renderPlot({
    df_top <- data_p3_top()
    df_share <- data_p3_share()
    verifica_checkbox(df_top, 0,
                      input$filtroEdad2,
                      input$filtroGen2,
                      input$filtroNiv2,
                      input$filtroTipoCliente2,
                      input$filtroTipoProducto2)
    grafica_top_share(df_top, df_share, input$facet2)
  }, 
  height = 700, 
  width = 1600)
  
  ###### Pregunta 4
  
  data_p4_top <- reactive({
    genera_data_p(p4_Top, 
                  c("P4_Top", "F_3"), 
                  1,
                  input$filtroEdad2,
                  input$filtroGen2,
                  input$filtroNiv2,
                  input$filtroTipoCliente2,
                  input$filtroTipoProducto2,
                  input$facet2)
  })
  
  data_p4_share <- reactive({
    genera_data_p(p4_share, c(11:22), 2,
                  input$filtroEdad2,
                  input$filtroGen2,
                  input$filtroNiv2,
                  input$filtroTipoCliente2,
                  input$filtroTipoProducto2,
                  input$facet2)
  })
  
  tb_p4<-reactive({
    b <- tam_base(p4_Top,
                  input$filtroEdad2,
                  input$filtroGen2,
                  input$filtroNiv2,
                  input$filtroTipoCliente2,
                  input$filtroTipoProducto2)
    return(b)
  })
  
  output$txt_p4 <- renderText(paste("Tamaño de base: ", tb_p4()[1],
                                    ". Esto es un tamaño de ",tb_p4()[3],".",sep=""))
  
  output$plot_p4 <- renderPlot({
    df_top <- data_p4_top()
    df_share <- data_p4_share()
    verifica_checkbox(df_top, 0,
                      input$filtroEdad2,
                      input$filtroGen2,
                      input$filtroNiv2,
                      input$filtroTipoCliente2,
                      input$filtroTipoProducto2)
    grafica_top_share(df_top, df_share, input$facet2)
  }, 
  height = 700, 
  width = 1600)
  
  #Logos (P5)----
  data_p5_Guiado <- reactive({
    genera_data_p(p5_guiado, c(11:21), 2,
                  input$filtroEdad2,
                  input$filtroGen2,
                  input$filtroNiv2,
                  input$filtroTipoCliente2,
                  input$filtroTipoProducto2,
                  input$facet2)
  })
  
  tb_p5<-reactive({
    b <- tam_base(p5_guiado,
                  input$filtroEdad2,
                  input$filtroGen2,
                  input$filtroNiv2,
                  input$filtroTipoCliente2,
                  input$filtroTipoProducto2)
    return(b)
  })
  
  output$txt_p5 <- renderText(paste("Tamaño de base: ", tb_p5()[1],
                                    ". Esto es un tamaño de ",tb_p5()[3],".",sep=""))
  
#   output$plot_p5_Guiado <- renderPlot({
#     graphdata <- data_p5_Guiado()
#     verifica_checkbox(graphdata, 0,
#                       input$filtroEdad2,
#                       input$filtroGen2,
#                       input$filtroNiv2,
#                       input$filtroTipoCliente2,
#                       input$filtroTipoProducto2)
#     grafica_cuenta(graphdata, input$facet2)
#   }, 
#   height = 900, 
#   width = 1000)
  
  #Recuerda haber visto publicidad (P6)----
  
  data_p6_Guiado <- reactive({
    genera_data_p(p6_guiado, c(11:21), 2,
                  input$filtroEdad2,
                  input$filtroGen2,
                  input$filtroNiv2,
                  input$filtroTipoCliente2,
                  input$filtroTipoProducto2,
                  input$facet2)
  })
  
  tb_p6<-reactive({
    b <- tam_base(p6_guiado,
                  input$filtroEdad2,
                  input$filtroGen2,
                  input$filtroNiv2,
                  input$filtroTipoCliente2,
                  input$filtroTipoProducto2)
    return(b)
  })
  
  output$txt_p6 <- renderText(paste("Tamaño de base: ", tb_p6()[1],
                                    ". Esto es un tamaño de ",tb_p6()[3],".",sep=""))
  
  output$plot_p6_Guiado <- renderPlot({
    graphdata <- data_p6_Guiado()
    verifica_checkbox(graphdata, 0,
                      input$filtroEdad2,
                      input$filtroGen2,
                      input$filtroNiv2,
                      input$filtroTipoCliente2,
                      input$filtroTipoProducto2)
    grafica_cuenta(graphdata, input$facet2)
  }, 
  height = 900, 
  width = 1000)
  
  #Marcas que considera par siguiente compra (P7)------
  
  data_p7 <- reactive({
    genera_data_p(p7, c(11:21), 2,
                  input$filtroEdad2,
                  input$filtroGen2,
                  input$filtroNiv2,
                  input$filtroTipoCliente2,
                  input$filtroTipoProducto2,
                  input$facet2)
  })
  
  tb_p7<-reactive({
    b <- tam_base(p7,
                  input$filtroEdad2,
                  input$filtroGen2,
                  input$filtroNiv2,
                  input$filtroTipoCliente2,
                  input$filtroTipoProducto2)
    return(b)
  })
  
  output$txt_p7 <- renderText(paste("Tamaño de base: ", tb_p7()[1],
                                    ". Esto es un tamaño de ",tb_p7()[3],".",sep=""))
  
#   output$plot_p7 <- renderPlot({
#     graphdata <- data_p7()
#     verifica_checkbox(graphdata, 0,
#                       input$filtroEdad2,
#                       input$filtroGen2,
#                       input$filtroNiv2,
#                       input$filtroTipoCliente2,
#                       input$filtroTipoProducto2)
#     grafica_cuenta(graphdata, input$facet2)
#   }, 
#   height = 900, 
#   width = 1000)
  
  #En que tiendas ha comprado últimamente (P8)-----
  
  data_p8<- reactive({
    genera_data_p(p8, c(11:21), 2,
                  input$filtroEdad2,
                  input$filtroGen2,
                  input$filtroNiv2,
                  input$filtroTipoCliente2,
                  input$filtroTipoProducto2,
                  input$facet2)
  })
  
  tb_p8<-reactive({
    b <- tam_base(p8,
                  input$filtroEdad2,
                  input$filtroGen2,
                  input$filtroNiv2,
                  input$filtroTipoCliente2,
                  input$filtroTipoProducto2)
    return(b)
  })
  
  output$txt_p8 <- renderText(paste("Tamaño de base: ", tb_p8()[1],
                                    ". Esto es un tamaño de ",tb_p8()[3],".",sep=""))
  
  output$plot_p8 <- renderPlot({
    graphdata <- data_p8()
    verifica_checkbox(graphdata, 0,
                      input$filtroEdad2,
                      input$filtroGen2,
                      input$filtroNiv2,
                      input$filtroTipoCliente2,
                      input$filtroTipoProducto2)
    grafica_cuenta(graphdata, input$facet2)
  }, 
  height = 900, 
  width = 1000)
  
  #En qué tiendas ha comprado U12
  
  data_p9 <- reactive({
    genera_data_p(p7, c(11:21), 2,
                  input$filtroEdad2,
                  input$filtroGen2,
                  input$filtroNiv2,
                  input$filtroTipoCliente2,
                  input$filtroTipoProducto2,
                  input$facet2)
  })
  
  tb_p9<-reactive({
    b <- tam_base(p9,
                  input$filtroEdad2,
                  input$filtroGen2,
                  input$filtroNiv2,
                  input$filtroTipoCliente2,
                  input$filtroTipoProducto2)
    return(b)
  })
  
  output$txt_p9 <- renderText(paste("Tamaño de base: ", tb_p9()[1],
                                    ". Esto es un tamaño de ",tb_p9()[3],".",sep=""))
  
  output$plot_p9 <- renderPlot({
    graphdata <- data_p9()
    verifica_checkbox(graphdata, 0,
                      input$filtroEdad2,
                      input$filtroGen2,
                      input$filtroNiv2,
                      input$filtroTipoCliente2,
                      input$filtroTipoProducto2)
    grafica_cuenta(graphdata, input$facet2)
  }, 
  height = 900, 
  width = 1000)
  
  #En cuales ha comprado alguna vez----
  
  data_p10 <- reactive({
    genera_data_p(p10, c(11:21), 2,
                  input$filtroEdad2,
                  input$filtroGen2,
                  input$filtroNiv2,
                  input$filtroTipoCliente2,
                  input$filtroTipoProducto2,
                  input$facet2)
  })
  
  tb_p10<-reactive({
    b <- tam_base(p10,
                  input$filtroEdad2,
                  input$filtroGen2,
                  input$filtroNiv2,
                  input$filtroTipoCliente2,
                  input$filtroTipoProducto2)
    return(b)
  })
  
  output$txt_p10 <- renderText(paste("Tamaño de base: ", tb_p10()[1],
                                     ". Esto es un tamaño de ",tb_p10()[3],".",sep=""))
  
  output$plot_p10 <- renderPlot({
    graphdata <- data_p10()
    verifica_checkbox(graphdata, 0,
                      input$filtroEdad2,
                      input$filtroGen2,
                      input$filtroNiv2,
                      input$filtroTipoCliente2,
                      input$filtroTipoProducto2)
    grafica_cuenta(graphdata, input$facet2)
  }, 
  height = 900, 
  width = 1000)
  
  #En cuales compra con mayor frecuencia (P11)----
  data_p11 <- reactive({
    genera_data_p(p11, c("P11", "F_3"), 1,
                  input$filtroEdad2,
                  input$filtroGen2,
                  input$filtroNiv2,
                  input$filtroTipoCliente2,
                  input$filtroTipoProducto2,
                  input$facet2)
  })
  
  tb_p11<-reactive({
    b <- tam_base(p11,
                  input$filtroEdad2,
                  input$filtroGen2,
                  input$filtroNiv2,
                  input$filtroTipoCliente2,
                  input$filtroTipoProducto2)
    return(b)
  })
  
  output$txt_p11 <- renderText(paste("Tamaño de base: ", tb_p11()[1],
                                     ". Esto es un tamaño de ",tb_p11()[3],".",sep=""))
  
  output$plot_p11 <- renderPlot({
    graphdata <- data_p11()
    verifica_checkbox(graphdata, 0,
                      input$filtroEdad2,
                      input$filtroGen2,
                      input$filtroNiv2,
                      input$filtroTipoCliente2,
                      input$filtroTipoProducto2)
    grafica_cuenta(graphdata, input$facet2)
  }, 
  height = 900, 
  width = 1000)
  
  #Si no comprara en su tienda más frecuente en cuál lo haría (P11a)------
  
  data_p11a<- reactive({
    genera_data_p(p11a, c("P11a", "F_3"), 1,
                  input$filtroEdad2,
                  input$filtroGen2,
                  input$filtroNiv2,
                  input$filtroTipoCliente2,
                  input$filtroTipoProducto2,
                  input$facet2)
  })
  
  tb_p11a<-reactive({
    b <- tam_base(p11a,
                  input$filtroEdad2,
                  input$filtroGen2,
                  input$filtroNiv2,
                  input$filtroTipoCliente2,
                  input$filtroTipoProducto2)
    return(b)
  })
  
  output$txt_p11a <- renderText(paste("Tamaño de base: ", tb_p11a()[1],
                                      ". Esto es un tamaño de ",tb_p11a()[3],".",sep=""))
  
  output$plot_p11a <- renderPlot({
    graphdata <- data_p11a()
    verifica_checkbox(graphdata, 0,
                      input$filtroEdad2,
                      input$filtroGen2,
                      input$filtroNiv2,
                      input$filtroTipoCliente2,
                      input$filtroTipoProducto2)
    grafica_cuenta(graphdata, input$facet2)
  }, 
  height = 900, 
  width = 1000)
  
  #   tb_p16_1 <- reactive({
  #     b <- tam_base(base_evap,
  #                   input$filtroEdad5,
  #                   input$filtroGen5,
  #                   input$filtroNiv5,
  #                   input$filtroTipoCliente5,
  #                   input$filtroTipoProducto5)
  #     return(b)
  #   })
  
  data_p11_vs_p11a <- reactive({
    p11_f <- filtro(p11,
                input$filtroEdad2,
                input$filtroGen2,
                input$filtroNiv2,
                input$filtroTipoCliente2,
                unique(df$P2_1),
                input$facet2)
    
    p11a_f <- filtro(p11a,
                    input$filtroEdad2,
                    input$filtroGen2,
                    input$filtroNiv2,
                    input$filtroTipoCliente2,
                    unique(df$P2_1),
                    input$facet2)
    list(p11_f, p11a_f)
  })
  
  output$plot_p11_vs_p11a <- renderPlot({
    datos_p11 <- data_p11_vs_p11a()[[1]]
    datos_p11a <- data_p11_vs_p11a()[[2]]
#     verifica_checkbox(datos_p11, 0,
#                       input$filtroEdad2,
#                       input$filtroGen2,
#                       input$filtroNiv2,
#                       input$filtroTipoCliente2,
#                       input$filtroTipoProducto2)
    facet_p11_vs_p11a <- function() NULL
    if(class(datos_p11) == "list" && class(datos_p11a) == "list"){
      if(length(datos_p11) == 0) b = NULL
      else {
        d <- lapply(1:length(datos_p11), function(i) {
          data.frame(Nivel = names(datos_p11)[i], datos_p11[[i]])
        })
        datos_p11 <- rbind_all(d)
        d <- lapply(1:length(datos_p11a), function(i) {
          data.frame(Nivel = names(datos_p11a)[i], datos_p11a[[i]])
        })
        datos_p11a <- rbind_all(d)
        facet_p11_vs_p11a <- function() facet_wrap(~Nivel)
        
        graphdata <- inner_join(datos_p11, datos_p11a) %>% 
          group_by(P11, P11a, Nivel) %>% 
          summarise(n = round(sum(F_3))) %>%
          rename(Uso_actual = P11, Segunda_opcion = P11a) %>%
          ungroup() %>%
          group_by(Uso_actual) %>%
          mutate(n2 = sum(n)) %>%
          ungroup() %>%
          arrange(desc(n2)) %>%
          mutate(Uso_actual = factor(Uso_actual, levels = unique(Uso_actual)))
      }
    }
    else{
      graphdata <- inner_join(datos_p11, datos_p11a) %>% 
        group_by(P11, P11a) %>% 
        summarise(n = round(sum(F_3))) %>%
        rename(Uso_actual = P11, Segunda_opcion = P11a) %>%
        ungroup() %>%
        group_by(Uso_actual) %>%
        mutate(n2 = sum(n)) %>%
        ungroup() %>%
        arrange(desc(n2)) %>%
        mutate(Uso_actual = factor(Uso_actual, levels = unique(Uso_actual)))
    }
    ggplot(graphdata) + 
      geom_bar(aes(x = Uso_actual, y = n, fill = Segunda_opcion), color = "black", stat = 'identity') +
      facet_p11_vs_p11a() +
      scale_fill_manual(values = 
                          c("Bodega Aurrerá" = "forestgreen",
                            "Chedraui" = "darkorange2",
                            "Coppel" = "goldenrod1",
                            "Elektra" = "red",
                            "Famsa" = "royalblue4",
                            "Liverpool" = "deeppink",
                            "Otros" = "black",
                            "Salinas y Rocha" = "grey",
                            "Soriana" = "chartreuse4",
                            "Tiendas de Electrónica" = "grey20",
                            "Viana" = "chartreuse2",
                            "Walmart" = "dodgerblue2")) +
      theme(axis.text.x=element_text(angle=90,size=22),
            axis.text.y=element_text(size=22),
            panel.background=element_rect(fill='#C2D1E0'),
            strip.background=element_rect(fill="#2c3e50"),
            panel.border = element_rect(colour = "#2c3e50", fill=NA, size=1),
            strip.text.x = element_text(colour = 'white', size = 22),
            legend.text=element_text(size=24),
            legend.title=element_blank()) +
      ylab("") + xlab("")
  }, 
  height = 700, 
  width = 1600)
  
  
  tb_tam_base_16a18 <- reactive({
    a <- base_evap %>% filter(P17 != " ")
    b <- tam_base(a,
                  input$filtroEdad5,
                  input$filtroGen5,
                  input$filtroNiv5,
                  input$filtroTipoCliente5,
                  input$filtroTipoProducto5)
    df %>% filter(P17 != " ") %>% nrow()
    return(b)
  })
  
  tb_tam_base_19a21 <- reactive({
    a <- base_evap %>% filter(P20 != " ")
    b <- tam_base(a,
                  input$filtroEdad5,
                  input$filtroGen5,
                  input$filtroNiv5,
                  input$filtroTipoCliente5,
                  input$filtroTipoProducto5)
    df %>% filter(P17 != " ") %>% nrow()
    return(b)
  })  
  
  
  
  
  
  tb_tam_base_open <- reactive({
          a <- p19 %>% filter(p19$P19_1_COD_1 != "")
          b <- tam_base(a,
                        input$filtroEdad5,
                        input$filtroGen5,
                        input$filtroNiv5,
                        input$filtroTipoCliente5,
                        input$filtroTipoProducto5)
          return(b)
  }) 
  
  
  tb_tam_base_open2 <- reactive({
          a <- p19 %>% filter(p16$P16_1_COD_1 != "")
          b <- tam_base(a,
                        input$filtroEdad5,
                        input$filtroGen5,
                        input$filtroNiv5,
                        input$filtroTipoCliente5,
                        input$filtroTipoProducto5)
          return(b)
  }) 
  
  
  
  
  
  output$txt_p16_1 <- renderText(paste("Tamaño de base: ", tb_tam_base_16a18()[1],
                                       ". Esto es un tamaño de ",tb_tam_base_16a18()[3],".",sep=""))
  
  
  output$txt_p16a_1 <- renderText(paste("Tamaño de base: ", tb_tam_base_16a18()[1],
                                        ". Esto es un tamaño de ",tb_tam_base_16a18()[3],".",sep=""))
  
  output$txt_p17 <- renderText(paste("Tamaño de base: ", tb_tam_base_16a18()[1],
                                     ". Esto es un tamaño de ",tb_tam_base_16a18()[3],".",sep=""))
  
  output$txt_p19_1 <- renderText(paste("Tamaño de base: ", tb_tam_base_19a21()[1],
                                       ". Esto es un tamaño de ",tb_tam_base_19a21()[3],".",sep=""))
  
  output$txt_p19a_1 <- renderText(paste("Tamaño de base: ", tb_tam_base_19a21()[1],
                                        ". Esto es un tamaño de ",tb_tam_base_19a21()[3],".",sep=""))
  
  output$txt_p20 <- renderText(paste("Tamaño de base: ", tb_tam_base_19a21()[1],
                                     ". Esto es un tamaño de ",tb_tam_base_19a21()[3],".",sep=""))
  
  
  
  output$txt_open1 <- renderText(paste("Tamaño de base: ", tb_tam_base_open()[1],
                                     ". Esto es un tamaño de ",tb_tam_base_open()[3],".",sep=""))
  
  output$txt_open2 <- renderText(paste("Tamaño de base: ", tb_tam_base_open2()[1],
                                       ". Esto es un tamaño de ",tb_tam_base_open2()[3],".",sep=""))
  
  data_p16_1 <- reactive({
    a <- filtro(base_evap,
                input$filtroEdad5,
                input$filtroGen5,
                input$filtroNiv5,
                input$filtroTipoCliente5,
                input$filtroTipoProducto5,
                "Total",
                TRUE)
    return(P16_1[a])
  })
  
  output$nube_P16_1 <- renderPlot({
    graphdata <- data_p16_1()
    if(length(graphdata) == 0) graphdata <- NULL
    verifica_checkbox(graphdata, 0, 
                      input$filtroEdad5,
                      input$filtroGen5,
                      input$filtroNiv5,
                      input$filtroTipoCliente5,
                      input$filtroTipoProducto5)
    nubes(palabras_prohibidas, graphdata)
  },height = 800, 
  width = 1000)
  
  
  
  data_p16a_1 <- reactive({
    a <- filtro(base_evap,
                input$filtroEdad5,
                input$filtroGen5,
                input$filtroNiv5,
                input$filtroTipoCliente5,
                input$filtroTipoProducto5,
                "Total",
                TRUE)
    return(P16A_1[a])
  })
  
  output$nube_P16A_1 <- renderPlot({
    graphdata <- data_p16a_1()
    if(length(graphdata) == 0) graphdata <- NULL
    verifica_checkbox(graphdata, 0, 
                      input$filtroEdad5,
                      input$filtroGen5,
                      input$filtroNiv5,
                      input$filtroTipoCliente5,
                      input$filtroTipoProducto5)
    nubes(palabras_prohibidas, graphdata)
  },height = 800, 
  width = 1000)
  
  
  data_p17 <- reactive({
    genera_data_p(p17, c("P17", "F_3"), 1,
                  input$filtroEdad5,
                  input$filtroGen5,
                  input$filtroNiv5,
                  input$filtroTipoCliente5,
                  input$filtroTipoProducto5)
  })
  
  output$plot_p17 <- renderPlot({
    graphdata <- data_p17()
    graphdata$linea <- factor(c("Mucho mejor y más interesado",
                                "Un poco mejor",
                                "No cambió para nada",
                                "Cambió para peor"), 
                              levels = c("Mucho mejor y más interesado",
                                         "Un poco mejor",
                                         "No cambió para nada",
                                         "Cambió para peor"))
    verifica_checkbox(graphdata, 0,
                      input$filtroEdad5,
                      input$filtroGen5,
                      input$filtroNiv5,
                      input$filtroTipoCliente5,
                      input$filtroTipoProducto5)
    grafica_cuenta(graphdata)
  }, 
  height = 900, 
  width = 1000)
  
  data_p19_1 <- reactive({
    a <- filtro(base_evap,
                input$filtroEdad5,
                input$filtroGen5,
                input$filtroNiv5,
                input$filtroTipoCliente5,
                input$filtroTipoProducto5,
                "Total",
                TRUE)
    return(P19_1[a])
  })
  
  output$nube_P19_1 <-  renderPlot({
    graphdata <- data_p19_1()
    if(length(graphdata) == 0) graphdata <- NULL
    verifica_checkbox(graphdata, 0, 
                      input$filtroEdad5,
                      input$filtroGen5,
                      input$filtroNiv5,
                      input$filtroTipoCliente5,
                      unique(df$P2_1))
    nubes(palabras_prohibidas, graphdata)
  },height = 800, 
  width = 1000)
  
  data_p19a_1 <- reactive({
    a <- filtro(base_evap,
                input$filtroEdad5,
                input$filtroGen5,
                input$filtroNiv5,
                input$filtroTipoCliente5,
                input$filtroTipoProducto5,
                "Total",
                TRUE)
    return(P19A_1[a])
  })
  
  output$nube_P19A_1 <-  renderPlot({
    graphdata <- data_p19a_1()
    if(length(graphdata) == 0) graphdata <- NULL
    verifica_checkbox(graphdata, 0, 
                      input$filtroEdad5,
                      input$filtroGen5,
                      input$filtroNiv5,
                      input$filtroTipoCliente5,
                      input$filtroTipoProducto5)
    nubes(palabras_prohibidas, graphdata)
  },height = 800, 
  width = 1000)
  
  data_p20 <- reactive({
    genera_data_p(p20, c("P20", "F_3"), 1,
                  input$filtroEdad5,
                  input$filtroGen5,
                  input$filtroNiv5,
                  input$filtroTipoCliente5,
                  input$filtroTipoProducto5)
  })
  
  output$plot_p20 <- renderPlot({
    graphdata <- data_p20()
    graphdata$linea <- factor(c("Mucho mejor y más interesado",
                                "Un poco mejor",
                                "No cambió para nada",
                                "Cambió para peor"), 
                              levels = c("Mucho mejor y más interesado",
                                         "Un poco mejor",
                                         "No cambió para nada",
                                         "Cambió para peor"))
    verifica_checkbox(graphdata, 0,
                      input$filtroEdad5,
                      input$filtroGen5,
                      input$filtroNiv5,
                      input$filtroTipoCliente5,
                      input$filtroTipoProducto5)
    grafica_cuenta(graphdata)
  }, 
  height = 900, 
  width = 1000)
  
  
  output$d2 <- downloadHandler(
    filename = "Base_Filtrada.csv",
    content = function(file){
      tabla2<-filtro(df,
                     input$filtroEdad2,
                     input$filtroGen2,
                     input$filtroNiv2,
                     input$filtroTipoCliente2,
                     input$filtroTipoProducto2,
                     input$facet2)
      tabla3<-tabla2[,c('Genero','Edad','NIVEL','Tipo_Cliente')]
      write.csv(tabla3,file,row.names=F)
    }
  )
  
  output$d3 <- downloadHandler(
    filename = "Base_Filtrada.csv",
    content = function(file){
      tabla2<-filtro(df,
                     input$filtroEdad3,
                     input$filtroGen3,
                     input$filtroNiv3,
                     input$filtroTipoCliente3,
                     input$filtroTipoProducto3,
                     input$facet3)
      tabla3<-tabla2[,c('Genero','Edad','NIVEL','Tipo_Cliente')]
      write.csv(tabla3,file,row.names=F)
    }
  )
  
  output$d4 <- downloadHandler(
    filename = "Base_Filtrada.csv",
    content = function(file){
      tabla2<-filtro(df,
                     input$filtroEdad4,
                     input$filtroGen4,
                     input$filtroNiv4,
                     input$filtroTipoCliente4,
                     input$filtroTipoProducto4,
                     input$facet4)
      tabla3<-tabla2[,c('Genero','Edad','NIVEL','Tipo_Cliente')]
      write.csv(tabla3,file,row.names=F)
    }
  )
  
  output$d5 <- downloadHandler(
    filename = "Base_Filtrada.csv",
    content = function(file){
      tabla2<-filtro(df,
                     input$filtroEdad5,
                     input$filtroGen5,
                     input$filtroNiv5,
                     input$filtroTipoCliente5,
                     input$filtroTipoProducto5)
      tabla3<-tabla2[,c('Genero','Edad','NIVEL','Tipo_Cliente')]
      write.csv(tabla3,file,row.names=F)
    }
  )
  
  
  #Información de contacto-----------
  output$contacto<-renderImage({
    list(src='images/contacto2.png',
         filetype='image/png',
         alt='wiiii',
         width = 1200,
         height = 900)
  }, 
  deleteFile = F)
  
  output$plotimagen<-renderPlot({
    datos<-data_plotimagen()
    grafica_dimensiones(datos)
  })
  
  data_plotimagen <- reactive({
    bateriab<-filtro(bateria,
                     input$filtroEdad3,
                     input$filtroGen3,
                     input$filtroNiv3,
                     input$filtroTipoCliente3,
                     input$filtroTipoProducto3)
    return(bateriab)
  })
  
  output$plotbateria<-renderPlot({
    datos<-data_plotimagen()
    grafica_bateria(datos)
  })
  
  data_plotequity <- reactive({
    bateriab<-filtro(dfequity,
                     input$filtroEdad4,
                     input$filtroGen4,
                     input$filtroNiv4,
                     input$filtroTipoCliente4,
                     input$filtroTipoProducto4)
    return(bateriab)
  })
  
  output$equitybateria<-renderPlot({
    datos<-data_plotequity()
    grafica_bateria_equity(datos)
  })
  
  output$equityscore<-renderPlot({
    datos<-data_plotequity()
    grafica_bateria_equity_score(datos)
  })
  
  #Funnel de Consideración a total
  
  output$consideraciontotal<-renderImage({
    list(src='images/consideraciontotal.png',
         filetype='image/png',
         alt='wiiii',
         height = 800, 
         width = 1500)
  }, 
  deleteFile = F)
  
  
  #Funnel de Consideración Clientes EKT
  
  output$consideracionekt<-renderImage({
    list(src='images/consideracionekt.png',
         filetype='image/png',
         alt='wiiii',
         height = 800, 
         width = 1500)
  }, 
  deleteFile = F)
  
  
  #Funnel de Consideración Competencia
  
  output$consideracioncomp<-renderImage({
    list(src='images/consideracioncomp.png',
         filetype='image/png',
         alt='wiiii',
         height = 800, 
         width = 1500)
  }, 
  deleteFile = F)
  
   data_plotpublicidad <- reactive({
    bateriab<-filtro(df,
                     input$filtroEdad5,
                     input$filtroGen5,
                     input$filtroNiv5,
                     input$filtroTipoCliente5)
    return(bateriab)
  })
  
  output$graficapublicidad<-renderPlot({
    datos<-data_plotpublicidad()
    fgrafpub(datos,input$marca,input$medios)
  })
  
  
  observe({
    if(length(input$medios) > my_max)
    {
      updateCheckboxGroupInput(session, "medios", selected= tail(input$medios,my_max))
    }
    if(length(input$medios) < my_min)
    {
      updateCheckboxGroupInput(session, "medios", selected = '1')
    }
  })
  
  
  
  
  data_gp <- reactive({
    bateriab<-filtro(df,
                     input$filtroEdad5,
                     input$filtroGen5,
                     input$filtroNiv5,
                     input$filtroTipoCliente5)
    return(bateriab)
  })
  
  output$graficascorepublicidad<-renderPlot({
    datos<-data_gp()
    grafica_calificacion_comerciales(datos)
  })
  
  
  
  
  
  
  
  
  #preguntas abiertas p16
  
  
  data_p16 <- reactive({
          bat <- filtro(p16,
                        input$filtroEdad5,
                        input$filtroGen5,
                        input$filtroNiv5,
                        input$filtroTipoCliente5,
                        input$filtroTipoProducto5)
          return(bat)
  })
  
  
output$text2<-renderText({
        graphdata <- data_p16()
        if(length(graphdata) == 0) graphdata <- NULL
        verifica_checkbox(graphdata, 0, 
                          input$filtroEdad5,
                          input$filtroGen5,
                          input$filtroNiv5,
                          input$filtroTipoCliente5,
                          input$filtroTipoProducto5)
        g <- abiertas(graphdata[,11:27],1)
        g <- paste("COMUNES A LA CATEGORÍA: ",
                   as.character(round(g[g$linea=="COMUNES A LA CATEGORÍA",2],
                                      digits=0)),
                   "%",
                   sep="")
})
#         g <- as.integer(g["COMUNES A LA CATEGORÍA",2])
#         paste(g)

output$text3<-renderText({
        graphdata <- data_p16()
        if(length(graphdata) == 0) graphdata <- NULL
        verifica_checkbox(graphdata, 0, 
                          input$filtroEdad5,
                          input$filtroGen5,
                          input$filtroNiv5,
                          input$filtroTipoCliente5,
                          input$filtroTipoProducto5)
        g <- abiertas(graphdata[,11:27],1)
        g <- paste("CORRECTO ESPECÍFICO: ",
                   as.character(round(g[g$linea=="CORRECTO ESPECÍFICO",2],
                                      digits=0)),
                   "%",
                   sep="")
        
})
        
output$text4<-renderText({
        graphdata <- data_p16()
        if(length(graphdata) == 0) graphdata <- NULL
        verifica_checkbox(graphdata, 0, 
                          input$filtroEdad5,
                          input$filtroGen5,
                          input$filtroNiv5,
                          input$filtroTipoCliente5,
                          input$filtroTipoProducto5)
        g <- abiertas(graphdata[,11:27],1)
        g <- paste("CORRECTO GENÉRICO: ",
                   as.character(round(g[g$linea=="CORRECTO GENÉRICO",2],
                                      digits=0)),
                   "%",
                   sep="")
})



output$text11<-renderText({
        graphdata <- data_p16()
        if(length(graphdata) == 0) graphdata <- NULL
        verifica_checkbox(graphdata, 0, 
                          input$filtroEdad5,
                          input$filtroGen5,
                          input$filtroNiv5,
                          input$filtroTipoCliente5,
                          input$filtroTipoProducto5)
        g <- abiertas(graphdata[,11:27],1)
        g <- paste("INCORRECTO: ",
                   as.character(round(g[g$linea=="INCORRECTO",2],
                                      digits=0)),
                   "%",
                   sep="")
        
})
        
        
output$text12<-renderText({
                graphdata <- data_p16()
                if(length(graphdata) == 0) graphdata <- NULL
                verifica_checkbox(graphdata, 0, 
                                  input$filtroEdad5,
                                  input$filtroGen5,
                                  input$filtroNiv5,
                                  input$filtroTipoCliente5,
                                  input$filtroTipoProducto5)
                g <- abiertas(graphdata[,11:27],1)
                g <- paste("NO IDENTIFICADO: ",
                           as.character(round(g[g$linea=="NO IDENTIFICADO",2],
                                              digits=0)),
                           "%",
                           sep="")
})
        
        
  output$table2 <- renderTable({
          graphdata <- data_p16()
          if(length(graphdata) == 0) graphdata <- NULL
          verifica_checkbox(graphdata, 0, 
                            input$filtroEdad5,
                            input$filtroGen5,
                            input$filtroNiv5,
                            input$filtroTipoCliente5,
                            input$filtroTipoProducto5)
           g <- abiertas(graphdata[,11:27],1)
           g <- g %>%
                   filter(linea %in% c(list("LÍNEA BLANCA", "TELEFONÍA", "PANTALLAS","ELECTRÓNICA","CÓMPUTO",
                                            "MUEBLES", "MOTOS", "ELECTRODOMÉSTICOS", "ACCESORIOS", "VIDEOJUEGOS", "LLANTAS", "OFERTAS Y PROMOCIONES",
                                            "PRECIOS BAJOS", "FORMAS DE PAGO", "VARIEDAD DE MARCAS", "VARIEDAD DE PRODUCTOS", "FACILIDADES DE PAGO",
                                            "CRÉDITO FÁCIL Y RÁPIDO","SERVICIOS BANCARIOS","CARACTERÍSTICAS DEL PRODUCTO","PRÉSTAMOS","INTERESES BAJOS",
                                            "DESCUENTOS POR EL DÍA DE LAS MADRES","AL PERSONAL DE LA TIENDA", "DESCUENTOS POR EL DÍA DEL PADRE")))%>%
                   arrange(linea)
  },include.rownames=FALSE,include.colnames=F)

  
  
  output$table4 <- renderTable({
          graphdata <- data_p16()
          if(length(graphdata) == 0) graphdata <- NULL
          verifica_checkbox(graphdata, 0, 
                            input$filtroEdad5,
                            input$filtroGen5,
                            input$filtroNiv5,
                            input$filtroTipoCliente5,
                            input$filtroTipoProducto5)
          g <- abiertas(graphdata[,11:27],1)
          g <- g %>%
                  filter(linea %in% c(list("FAMILIA EN LA TIENDA VIENDO PRODUCTOS","PERSONAL PROMOCIONANDO ARTÍCULOS DENTRO DE LA TIENDA ELEKTRA",
                                           "FIESTA CON ESTÉREO QUE TERMINABAN COMPRANDO EN ELEKTRA",
                                           "MUJER EN ELEKTRA PIDIENDO INFORMES DE UN ESTÉREO",
                                           "VENDEDOR ATENDIENDO CLIENTES CON AMABILIDAD Y RESPETO DENTRO DE LA TIENDA ELEKTRA",
                                           "ANUNCIOS DE LAS PANTALLAS EN FILA DE CAJAS",
                                           "CUAUHTÉMOC BLANCO EN LA TIENDA",
                                           "EMPLEADOS ACOMODANDO PRODUCTOS",
                                           "NIÑOS CON TABLET HACIENDO TAREA",
                                           "DESCUENTOS DE REGRESO A CLASES EN ELEKTRA",
                                           "FAMILIAS FELICES EN SU CASA",
                                           "PAREJA VIENDO PANTALLA QUE DICE 30% DE DESCUENTO",
                                           "SANTA CLAUS Y LOS REYES MAGOS SE SURTEN EN ELEKTRA",
                                           "SEÑORA LIMPIANDO SU CASA Y LE DA TIEMPO DE VER LA NOVELA",
                                           "UNA PAREJA BESÁNDOSE Y EL CHAVO CON EL CASCO EN LA MANO",
                                           "HIJOS COMPRANDO LICUADORA",
                                           "PAREJA DANDO REGALOS",
                                           "UNA FAMILIA PIDIENDO UN PRÉSTAMO",
                                           "VIEJITA FELIZ CON ESTUFA NUEVA",
                                           "CLIENTE COMPRA ESTÉREO Y SE LO LLEVAN GRATIS A SU CASA",
                                           "UN CHAVO EN LA CALLE CON SU MOTO ITALIKA")))%>%
                  arrange(linea)
          },include.rownames=FALSE,include.colnames=F)
   
  
  
  
  
   
  output$table5 <- renderTable({
          graphdata <- data_p16()
          if(length(graphdata) == 0) graphdata <- NULL
          verifica_checkbox(graphdata, 0, 
                            input$filtroEdad5,
                            input$filtroGen5,
                            input$filtroNiv5,
                            input$filtroTipoCliente5,
                            input$filtroTipoProducto5)
          g <- abiertas(graphdata[,11:27],1)
          g <- g %>%
                  filter(linea %in% c(list("ABONOS CHIQUITOS PARA PAGAR POQUITO",
                                           "TIENDA ELEKTRA",
                                           "GUARDADITO",
                                           "PAGO PUNTUAL",
                                           "BANCO AZTECA",
                                           "ITALIKA",
                                           "ABIERTO DE 9 A 9, 365 DÍAS",
                                           "COLORES AMARILLO Y ROJO",
                                           "ENVÍO DE DINERO",
                                           "CON ELEKTRA VIVES MEJOR",
                                           "CONFIABLE",
                                           "ENTREGA INMEDIATA",
                                           "A CRÉDITO Y DE CONTADO ELEKTRA ES MÁS BARATO",
                                           "EN LA COMPRA DE TU ITALIKA TE REGALAMOS EL CASCO")))%>%
                  arrange(linea)
          
  },include.rownames=FALSE,include.colnames=F)

  output$table10 <- renderTable({
          graphdata <- data_p16()
          if(length(graphdata) == 0) graphdata <- NULL
          verifica_checkbox(graphdata, 0, 
                            input$filtroEdad5,
                            input$filtroGen5,
                            input$filtroNiv5,
                            input$filtroTipoCliente5,
                            input$filtroTipoProducto5)
          g <- abiertas(graphdata[,11:27],1)
          g <- g %>%
                  filter(linea %in% c(list( "INCORRECTO",
                                            "COPPEL",
                                            "EXPERIENCIA EN LA TIENDA",
                                            "ROPA",
                                            "DESDE HACE AÑOS ELEKTRA MEJORA TU VIDA",
                                            "CALZADO",
                                            "CHAVOS COMO SI ESTUVIERAN EN EL CINE",
                                            "COMPARABAN 2 ESTÉREOS UNO DE ELEKTRA Y OTRO DE LA COMPETENCIA",
                                            "JÓVENES CANTANDO Y BAILANDO",
                                            "UNA CHAVA CON SU MOTO YAMAHA",
                                            "NO IDENTIFICADO",
                                            "ARMA LA FIESTA EN TU CUERPO",
                                            "CARROS LLENOS, MÁS BARATO QUE EL SÚPER",
                                            "CHAVO CON AUDIFONOS BEAT",
                                            "ELEKTRA TE HACE MÁS FÁCIL LA VIDA",
                                            "MUDANZA LLEGANDO A UNA CASA",
                                            "SEÑORA QUE CAMBIA DE COMPAÑÍA TELEFÓNICA")))%>%
                  arrange(linea)
  },include.rownames=FALSE,include.colnames=F)
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  # preguntas abiertas p19
  
  data_p19 <- reactive({
          bat <- filtro(p19,
                        input$filtroEdad5,
                        input$filtroGen5,
                        input$filtroNiv5,
                        input$filtroTipoCliente5,
                        input$filtroTipoProducto5)
          return(bat)
  })
  
  
  output$text6<-renderText({
          graphdata <- data_p19()
          if(length(graphdata) == 0) graphdata <- NULL
          verifica_checkbox(graphdata, 0, 
                            input$filtroEdad5,
                            input$filtroGen5,
                            input$filtroNiv5,
                            input$filtroTipoCliente5,
                            input$filtroTipoProducto5)
          g <- abiertas(graphdata[,11:27],2)
          g <- paste("COMUNES A LA CATEGORÍA: ",
                     as.character(round(g[g$linea=="COMUNES A LA CATEGORÍA",2],
                                        digits=0)),
                     "%",
                     sep="")
          })
  
  #         g <- as.integer(g["COMUNES A LA CATEGORÍA",2])
  #         paste(g)
  
  output$text7<-renderText({
          graphdata <- data_p19()
          if(length(graphdata) == 0) graphdata <- NULL
          verifica_checkbox(graphdata, 0, 
                            input$filtroEdad5,
                            input$filtroGen5,
                            input$filtroNiv5,
                            input$filtroTipoCliente5,
                            input$filtroTipoProducto5)
          g <- abiertas(graphdata[,11:27],2)
          g <- paste("CORRECTO ESPECÍFICO: ",
                     as.character(round(g[g$linea=="CORRECTO ESPECÍFICO",2],
                                        digits=0)),
                     "%",
                     sep="")
  })
  
  output$text8<-renderText({
          graphdata <- data_p19()
          if(length(graphdata) == 0) graphdata <- NULL
          verifica_checkbox(graphdata, 0, 
                            input$filtroEdad5,
                            input$filtroGen5,
                            input$filtroNiv5,
                            input$filtroTipoCliente5,
                            input$filtroTipoProducto5)
          g <- abiertas(graphdata[,11:27],2)
          g <- paste("CORRECTO GENÉRICO: ",
                     as.character(round(g[g$linea=="CORRECTO GENÉRICO",2],
                                        digits=0)),
                     "%",
                     sep="")
  })
  
  
  
  
  output$text9<-renderText({
          graphdata <- data_p19()
          if(length(graphdata) == 0) graphdata <- NULL
          verifica_checkbox(graphdata, 0, 
                            input$filtroEdad5,
                            input$filtroGen5,
                            input$filtroNiv5,
                            input$filtroTipoCliente5,
                            input$filtroTipoProducto5)
          g <- abiertas(graphdata[,11:27],2)
          g <- paste("INCORRECTO: ",
                     as.character(round(g[g$linea=="INCORRECTO",2],
                                        digits=0)),
                     "%",
                     sep="")
  })
  
  
  
  
  output$text20<-renderText({
          graphdata <- data_p19()
          if(length(graphdata) == 0) graphdata <- NULL
          verifica_checkbox(graphdata, 0, 
                            input$filtroEdad5,
                            input$filtroGen5,
                            input$filtroNiv5,
                            input$filtroTipoCliente5,
                            input$filtroTipoProducto5)
          g <- abiertas(graphdata[,11:27],2)
          g <- paste("NO IDENTIFICADO: ",
                     as.character(round(g[g$linea=="NO IDENTIFICADO",2],
                                        digits=0)),
                     "%",
                     sep="")
  })
  
  
  
  
  output$table6 <- renderTable({
          graphdata <- data_p19()
          if(length(graphdata) == 0) graphdata <- NULL
          verifica_checkbox(graphdata, 0, 
                            input$filtroEdad5,
                            input$filtroGen5,
                            input$filtroNiv5,
                            input$filtroTipoCliente5,
                            input$filtroTipoProducto5)
          g <- abiertas(graphdata[,11:27],2)
          g <- g %>%
                  filter(linea %in% c(list("TELEFONÍA",
                                           "LÍNEA BLANCA",
                                           "PANTALLAS",
                                           "CÓMPUTO",
                                           "ELECTRÓNICA",
                                           "MUEBLES",
                                           "ELECTRODOMÉSTICOS",
                                           "MOTOS",
                                           "PERFUMERÍA",
                                           "JUGUETERÍA",
                                           "LLANTAS",
                                           "OFERTAS Y PROMOCIONES",
                                           "PRECIOS BAJOS",
                                           "FORMAS DE PAGO",
                                           "VARIEDAD DE MARCAS",
                                           "CRÉDITO FÁCIL Y RÁPIDO",
                                           "VARIEDAD DE PRODUCTOS",
                                           "FAMILIA EN LA TIENDA",
                                           "FACILIDADES DE PAGO",
                                           "SERVICIOS BANCARIOS",
                                           "CARACTERÍSTICAS DEL PRODUCTO",
                                           "BUENA ATENCIÓN Y SERVICIO",
                                           "PRÉSTAMOS",
                                           "CERCANÍA DE TIENDAS",
                                           "INTERESES BAJOS",
                                           "DESCUENTOS POR EL DÍA DE LAS MADRES"))) %>%
                  arrange(linea)
  },include.rownames=FALSE,include.colnames=F)
  
  
  
  output$table7 <- renderTable({
          graphdata <- data_p19()
          if(length(graphdata) == 0) graphdata <- NULL
          verifica_checkbox(graphdata, 0, 
                            input$filtroEdad5,
                            input$filtroGen5,
                            input$filtroNiv5,
                            input$filtroTipoCliente5,
                            input$filtroTipoProducto5)
          g <- abiertas(graphdata[,11:27],2)
          g <- g %>%
                  filter(linea %in% c(list( "NIÑOS CON TABLETS PROMOCIONANDO EL REGRSO A CLASES CON COPPEL",
                                            "JÓVENES CANTANDO Y BAILANDO",
                                            "ARTISTAS PROMOCIONANDO LOS PRODUCTOS Y DESCUENTOS DE COPPEL",
                                            "MAITE PERRONI",
                                            "JÓVENES DENTRO DE LA TIENDA VIENDO ROPA",
                                            "CHAVA OFRECIENDO EL CRÉDITO DE COPPEL",
                                            "JENNIFER LOPEZ DISEÑANDO",
                                            "FAMILIA DÁNDOSE REGALOS DE NAVIDAD",
                                            "VÁZQUEZ SOUNDS",
                                            "PERSONAS ANUNCIANDO LOS PRODUCTOS DE TELEFONÍA Y LOS DESCUENTOS",
                                            "COMPARTE LA FELICIDAD CON TU FAMILIA Y CON COPPEL",
                                            "NIÑOS USANDO AMORÓMETRO",
                                            "EN MÉXICO LAS FAMILIAS SALEN A TRABAJAR, EN COPPEL RECONOCEN TU ESFUERZO",
                                            "NIÑOS JUGANDO DENTRO DE LA TIENDA Y NO SE ROMPÍAN LOS PRODUCTOS",
                                            "FAMILIA COMPRANDO XBOX PARA DÍA DEL NIÑO")))%>%
                  arrange(linea)
  },include.rownames=FALSE,include.colnames=F)
  
  
  
  
  
  
  output$table8 <- renderTable({
          graphdata <- data_p19()
          if(length(graphdata) == 0) graphdata <- NULL
          verifica_checkbox(graphdata, 0, 
                            input$filtroEdad5,
                            input$filtroGen5,
                            input$filtroNiv5,
                            input$filtroTipoCliente5,
                            input$filtroTipoProducto5)
          g <- abiertas(graphdata[,11:27],2)
          g <- g %>%
                  filter(linea %in% c(list("ROPA",
                                           "CALZADO",
                                           "TARJETA COPPEL",
                                           "TIENDA COPPEL",
                                           "COPPEL MEJORA TU VIDA",
                                           "PROMOCIONES DE VERANO",
                                           "TIENDA COPPEL CANADA",
                                           "ROPA DE TEMPORADA",
                                           "VENDEDOR ATENDIENDO CLIENTES",
                                           "CONFIABLE",
                                           "COPPEL RENUEVA TU HOGAR",
                                           "AMARILLO Y AZÚL",
                                           "TIPS",
                                           "VISTE A LA MODA CON COPPEL",
                                           "ILUMINA TU NAVIDAD CON COPPEL")))%>%
                  arrange(linea)
          
  },include.rownames=FALSE,include.colnames=F)
  
  output$table9 <- renderTable({
          graphdata <- data_p19()
          if(length(graphdata) == 0) graphdata <- NULL
          verifica_checkbox(graphdata, 0, 
                            input$filtroEdad5,
                            input$filtroGen5,
                            input$filtroNiv5,
                            input$filtroTipoCliente5,
                            input$filtroTipoProducto5)
          g <- abiertas(graphdata[,11:27],2)
          g <- g %>%
                  filter(linea %in% c(list( "INCORRECTO",
                          "PAGOS CHIQUITOS",
                          "ENTREGA INMEDIATA",
                          "TRABAJDORES EN SUS OFICINAS",
                          "PAGOS PUNTUALES",
                          "NO IDENTIFICADO",
                          "PARA ESOS MOMENTOS INOLVIDABLES CON TU FAMILIA",
                          "UN CUARTO VACÍO Y DE REPENTE APARECEN APARATOS",
                          "SEÑORA LAVANDO A MANO")))%>%
                  arrange(linea)
  },include.rownames=FALSE,include.colnames=F)
 
  
})
