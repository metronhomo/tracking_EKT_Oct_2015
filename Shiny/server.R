# server.R
library(shiny)


shinyServer(function(input, output,session){
 
  #Tipo de producto U12 meses (P2)-----------
  
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
  height = 900, 
  width = 1000)
  
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
    genera_data_p(p4_share, c(11:21), 2,
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
  height = 900, 
  width = 1000)
  
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
  
  output$plot_p5_Guiado <- renderPlot({
    graphdata <- data_p5_Guiado()
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
  
  output$plot_p7 <- renderPlot({
    graphdata <- data_p7()
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
  
  output$d1 <- downloadHandler(
    filename = "Base_Filtrada.csv",
    content = function(file){
      tabla2<-filtro(df,
                     input$filtroEdad1,
                     input$filtroGen1,
                     input$filtroNiv1,
                     input$filtroTipoCliente1,
                     input$filtroTipoProducto1,
                     input$facet1)
      tabla3<-tabla2[,c('Genero','Edad','NIVEL','Tipo_Cliente')]
      write.csv(tabla3,file,row.names=F)
    }
  )
  
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
                     input$filtroTipoProducto5,
                     input$facet5)
      tabla3<-tabla2[,c('Genero','Edad','NIVEL','Tipo_Cliente')]
      write.csv(tabla3,file,row.names=F)
    }
  )

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
    verifica_checkbox(graphdata, 1, 
                      input$filtroEdad5,
                      input$filtroGen5,
                      input$filtroNiv5,
                      input$filtroTipoCliente5,
                      unique(df$P2_1))
    nubes(palabras_prohibidas, graphdata)
  },height = 1200, 
  width = 1200)
  
#   output$nube_P16_1 <- renderPlot({
#     nubes(palabras_prohibidas,P16A_1)
#   },height = 1200, 
#   width = 1200)
  
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
    verifica_checkbox(graphdata, 1, 
                      input$filtroEdad5,
                      input$filtroGen5,
                      input$filtroNiv5,
                      input$filtroTipoCliente5,
                      unique(df$P2_1))
    nubes(palabras_prohibidas, graphdata)
  },height = 1200, 
  width = 1200)
  
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
    verifica_checkbox(graphdata, 1, 
                      input$filtroEdad5,
                      input$filtroGen5,
                      input$filtroNiv5,
                      input$filtroTipoCliente5,
                      unique(df$P2_1))
    nubes(palabras_prohibidas, graphdata)
  },height = 1200, 
  width = 1200)
  
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
    verifica_checkbox(graphdata, 1, 
                      input$filtroEdad5,
                      input$filtroGen5,
                      input$filtroNiv5,
                      input$filtroTipoCliente5,
                      unique(df$P2_1))
    nubes(palabras_prohibidas, graphdata)
  },height = 1200, 
  width = 1200)
  
                  
  #Información de contacto-----------
  output$contacto<-renderImage({
    list(src='images/contacto.png',
         filetype='image/png',
         alt='wiiii',
         width = 1200,
         height = 900)
  }, 
  deleteFile = F)
  
})