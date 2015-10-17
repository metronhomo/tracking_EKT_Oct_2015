# server.R
library(shiny)

verifica_checkbox <- function(graphdata, input, panel_1 = FALSE) {
  if(panel_1) {
    string_de_filtros <- c("Edad", "Género", "Nivel", "Tipo de cliente")
    vec <- c(
      length(input$filtroEdad1),
      length(input$filtroGen1),
      length(input$filtroNiv1),
      length(input$filtroTipoCliente1)
    )
  }
  else{
    string_de_filtros <- c("Edad", "Género", "Nivel", "Tipo de cliente", "Tipo de producto")
    vec <- c(
      length(input$filtroEdad2),
      length(input$filtroGen2),
      length(input$filtroNiv2),
      length(input$filtroTipoCliente2),
      length(input$filtroTipoProducto2)
    )
  }
  vec <- string_de_filtros[vec == 0]
  validate(need(!is.null(graphdata), paste("Escoge al menos una variable del panel \"", vec, "\".")))
  # validate(need(nrow(graphdata) <= 30, "Son demasiados filtros, por lo que la base es muy pequeña."))
}

shinyServer(function(input, output,session){
  #  
  #   #Para actualizar RadioButtons de la variable de cruce
  #   observe({
  #     if (input$filtrovar2 != 'Total'){
  #       datos1 <- df %>%
  #         dplyr::select(one_of(input$filtrovar2))
  #       choices <- eval(parse(text = 
  #                               paste('list(', paste('"', levels(datos1[ , 1]), '"="', levels(datos1[,1]),'"',
  #                                                    collapse=',',sep=''),')'
  #                               )
  #       ))
  #     } else{
  #       choices <- 'Total'
  #     }
  #     updateRadioButtons(session, 'filtrocat2', choices = choices)
  #   })
  #   
  #   observe({
  #     if (input$filtrovar1 != 'Total'){
  #       datos1 <- df %>%
  #         dplyr::select(one_of(input$filtrovar1))
  #       choices <- eval(parse(text = 
  #                               paste('list(', paste('"', levels(datos1[ , 1]), '"="', levels(datos1[,1]),'"',
  #                                                    collapse=',',sep=''),')'
  #                               )
  #       ))
  #     } else{
  #       choices <- 'Total'
  #     }
  #     updateRadioButtons(session, 'filtrocat1', choices = choices)
  #   })
  #   
  
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
    verifica_checkbox(graphdata, input, 1)
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
  

  genera_data_p <- function(pregunta, vector_string, ru) {
    a <- filtro(pregunta,
                input$filtroEdad2,
                input$filtroGen2,
                input$filtroNiv2,
                input$filtroTipoCliente2,
                input$filtroTipoProducto2,
                input$facet2)
    if(class(a) != "list") {
      if(nrow(a) == 0) b = NULL
      else b <- cuenta(a[, vector_string], ru)
    }
    else{
      if(length(a) == 0) b = NULL
      else {
        b <- lapply(a, function(x)  cuenta(x[, vector_string], ru))
        d <- lapply(1:length(b), function(i) {
          d <- data.frame(Nivel = names(b)[i], b[[i]])
        })
        b <- rbind_all(d)
      }
    }
    return(b)
  }
  
  ###### Pregunta 3
  
  data_p3_top <- reactive({
    genera_data_p(p3_top, c("P3_Top", "F_3"), 1)
  })
  
  data_p3_share <- reactive({
    genera_data_p(p3_share, c(11:22), 2)
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
    verifica_checkbox(df_top, input)
    grafica_top_share(df_top, df_share, input$facet2)
  }, 
  height = 900, 
  width = 1000)
  
  ###### Pregunta 4
  
  data_p4_top <- reactive({
    genera_data_p(p4_Top, c("P4_Top", "F_3"), 1)
  })
  
  data_p4_share <- reactive({
    genera_data_p(p4_share, c(11:21), 2)
  })
   
#   output$plot_p4 <- renderPlot({
#     df_top <- data_p4_top()
#     df_share <- data_p4_share()
#     verifica_checkbox(df_top, input)
#     grafica_top_share(df_top, df_share, input$facet2)
#   }, 
#   height = 900, 
#   width = 1000)
#   
#   
#   data_p4_Share <- reactive({
#     genera_data_p(p4_share, 11:20, 2)
#   })
#   
#   output$plot_p4_Share <- renderPlot({
#     graphdata <- data_p4_Share()
#     verifica_checkbox(graphdata, input)
#     grafica_cuenta(graphdata, input$facet2)
#   }, 
#   height = 900, 
#   width = 1000)
#   
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
    verifica_checkbox(df_top, input)
    grafica_top_share(df_top, df_share, input$facet2)
  }, 
  height = 900, 
  width = 1000)
  
  #Logos (P5)----
  data_p5_Guiado <- reactive({
    genera_data_p(p5_guiado, c(11:21), 2)
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
    verifica_checkbox(graphdata, input)
    grafica_cuenta(graphdata, input$facet2)
  }, 
  height = 900, 
  width = 1000)
  
  #Recuerda haber visto publicidad (P6)----
  
  data_p6_Guiado <- reactive({
    genera_data_p(p6_guiado, c(11:21), 2)
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
    verifica_checkbox(graphdata, input)
    grafica_cuenta(graphdata, input$facet2)
  }, 
  height = 900, 
  width = 1000)
  
  #Marcas que considera par siguiente compra (P7)------
  
  data_p7 <- reactive({
    genera_data_p(p7, c(11:21), 2)
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
    verifica_checkbox(graphdata, input)
    grafica_cuenta(graphdata, input$facet2)
  }, 
  height = 900, 
  width = 1000)
  
  #En que tiendas ha comprado últimamente (P8)-----
  
  data_p8<- reactive({
    genera_data_p(p8, c(11:21), 2)
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
    verifica_checkbox(graphdata, input)
    grafica_cuenta(graphdata, input$facet2)
  }, 
  height = 900, 
  width = 1000)
  
  #En qué tiendas ha comprado U12
  
  data_p9 <- reactive({
    genera_data_p(p7, c(11:21), 2)
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
    verifica_checkbox(graphdata, input)
    grafica_cuenta(graphdata, input$facet2)
  }, 
  height = 900, 
  width = 1000)
  
  #En cuales ha comprado alguna vez----
  
  data_p10 <- reactive({
    genera_data_p(p10, c(11:21), 2)
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
    verifica_checkbox(graphdata, input)
    grafica_cuenta(graphdata, input$facet2)
  }, 
  height = 900, 
  width = 1000)
  
  #En cuales compra con mayor frecuencia (P11)----
  data_p11 <- reactive({
    genera_data_p(p11, c("P11", "F_3"), 1)
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
    verifica_checkbox(graphdata, input)
    grafica_cuenta(graphdata, input$facet2)
  }, 
  height = 900, 
  width = 1000)
  
  #Si no comprara en su tienda más frecuente en cuál lo haría (P11a)------
  
  data_p11a<- reactive({
    genera_data_p(p11a, c("P11a", "F_3"), 1)
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
    verifica_checkbox(graphdata, input)
    grafica_cuenta(graphdata, input$facet2)
  }, 
  height = 900, 
  width = 1000)
  
  data_p17 <- reactive({
    genera_data_p(p17, c("P17", "F_3"), 1)
  })
  
  output$plot_p17 <- renderPlot({
    graphdata <- data_p17()
    verifica_checkbox(graphdata, input)
    grafica_cuenta(graphdata, input$facet2)
  }, 
  height = 900, 
  width = 1000)
  
  data_p20 <- reactive({
    genera_data_p(p20, c("P_20", "F_3"), 1)
  })
  
  output$plot_p20 <- renderPlot({
    graphdata <- data_p20()
    verifica_checkbox(graphdata, input)
    grafica_cuenta(graphdata, input$facet2)
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

  output$nube_P16_1 <- renderPlot({
    nubes(palabras_prohibidas,P16_1)
  },height = 1200, 
  width = 1200)
  
  output$nube_P16_1 <- renderPlot({
    nubes(palabras_prohibidas,P16A_1)
  },height = 1200, 
  width = 1200)
               
  output$nube_P19_1 <- renderPlot({
    nubes(palabras_prohibidas,P19_1)
  },height = 1200, 
  width = 1200) 
  
  output$nube_P19A_1 <- renderPlot({
    nubes(palabras_prohibidas,P19A_1)
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