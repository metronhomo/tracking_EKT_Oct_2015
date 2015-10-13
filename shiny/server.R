# server.R
library(shiny)

shinyServer(function(input, output,session){
  
  #Para actualizar RadioButtons de la variable de cruce
  observe({
    if(input$filtrovar!="Total"){
      datos1 <- df %>%
        dplyr::select(one_of(input$filtrovar))
      
      r_options<-eval(parse(text=paste('list(',paste('"',levels(datos1[,1]),'"="',levels(datos1[,1]),'"',
                                                     collapse=',',sep=''),')')))
    }else{
      r_options <- "Total"
    }
    updateRadioButtons(session,'filtrocat',choices=r_options)
  })
  
  #Imagen en sección de contacto
  output$contacto<-renderImage({
    list(src='images/contacto.png',
         filetype='image/png',
         alt='wiiii',
         width = 1200,
         height = 900)
  }, 
  deleteFile = F)
  
  
  
  data_p2 <- reactive({
    a <- filtro(p2,
                input$edad,
                input$genero,
                input$plaza,
                input$nivel,
                input$tipo_cliente,
                input$tipo_producto)
    b <- cuenta(a,2)
    return(b)
  }) 
  
  data_p3 <- reactive({
    a <- filtro(p3_top,
                input$edad,
                input$genero,
                input$plaza,
                input$nivel,
                input$tipo_cliente,
                input$tipo_producto)
    b <- cuenta(a,1)
    return(b)
  })
  
  data_p4_Top <- reactive({
    a <- filtro(p4_Top,
                input$edad,
                input$genero,
                input$plaza,
                input$nivel,
                input$tipo_cliente,
                input$tipo_producto)
    b <- cuenta(a,1)
    return(b)
  })
  
  data_p4_Share <- reactive({
    a <- filtro(p4_share,
                input$edad,
                input$genero,
                input$plaza,
                input$nivel,
                input$tipo_cliente,
                input$tipo_producto)
    b <- cuenta(a,2)
    return(b)
  })
  
  data_p5_Guiado <- reactive({
    a <- filtro(p5_guiado,
                input$edad,
                input$genero,
                input$plaza,
                input$nivel,
                input$tipo_cliente,
                input$tipo_producto)
    b <- cuenta(a,1)
    return(b)
  })
  
  data_p6_Guiado <- reactive({
    a <- filtro(p6_guiado,
                input$edad,
                input$genero,
                input$plaza,
                input$nivel,
                input$tipo_cliente,
                input$tipo_producto)
    b <- cuenta(a,1)
    return(b)
  })
  
  data_p7 <- reactive({
    a <- filtro(p7,
                input$edad,
                input$genero,
                input$plaza,
                input$nivel,
                input$tipo_cliente,
                input$tipo_producto)
    b <- cuenta(a,2)
    return(b)
  })
  
  data_p8<- reactive({
    a <- filtro(p8,
                input$edad,
                input$genero,
                input$plaza,
                input$nivel,
                input$tipo_cliente,
                input$tipo_producto)
    b <- cuenta(a,2)
    return(b)
  })
  
  data_p9 <- reactive({
    a <- filtro(p9,
                input$edad,
                input$genero,
                input$plaza,
                input$nivel,
                input$tipo_cliente,
                input$tipo_producto)
    b <- cuenta(a,2)
    return(b)
  })
  
  data_p10 <- reactive({
    a <- filtro(p10,
                input$edad,
                input$genero,
                input$plaza,
                input$nivel,
                input$tipo_cliente,
                input$tipo_producto)
    b <- cuenta(a,2)
    return(b)
  })
  
  data_p11 <- reactive({
    a <- filtro(p11,
                input$edad,
                input$genero,
                input$plaza,
                input$nivel,
                input$tipo_cliente,
                input$tipo_producto)
    b <- cuenta(a,1)
    return(b)
  })
  
  data_p11a<- reactive({
    a <- filtro(p11a,
                input$edad,
                input$genero,
                input$plaza,
                input$nivel,
                input$tipo_cliente,
                input$tipo_producto)
    b <- cuenta(a,1)
    return(b)
  })
  
  data_p17 <- reactive({
    a <- filtro(p17,
                input$edad,
                input$genero,
                input$plaza,
                input$nivel,
                input$tipo_cliente,
                input$tipo_producto)
    b <- cuenta(a,1)
    return(b)
  })
  
  data_p20 <- reactive({
    a <- filtro(p20,
                input$edad,
                input$genero,
                input$plaza,
                input$nivel,
                input$tipo_cliente,
                input$tipo_producto)
    b <- cuenta(a,1)
    return(b)
  })
  
  output$plot_p2 <- renderPlot({
    graphdata <- data_p2()
    if (nrow(graphdata)==0) return()
    
    grafica_cuenta(graphdata)
  }, 
  height = 900, 
  width = 1000)
  
  output$plot_p3 <- renderPlot({
    graphdata <- data_p3()
    if (nrow(graphdata)==0) return()
    
    grafica_cuenta(graphdata)
  }, 
  height = 900, 
  width = 1000)
  
  output$plot_p4_Top <- renderPlot({
    graphdata <- data_p4_Top()
    if (nrow(graphdata)==0) return()
    
    grafica_cuenta(graphdata)
  }, 
  height = 900, 
  width = 1000)
  
  output$plot_p4_Share <- renderPlot({
    graphdata <- data_p4_Share()
    if (nrow(graphdata)==0) return()
    
    grafica_cuenta(graphdata)
  }, 
  height = 900, 
  width = 1000)
  
  output$plot_p5_Guiado <- renderPlot({
    graphdata <- data_p5_Guiado()
    if (nrow(graphdata)==0) return()
    
    grafica_cuenta(graphdata)
  }, 
  height = 900, 
  width = 1000)
  
  output$plot_p6_Guiado <- renderPlot({
    graphdata <- data_p6_Guiado()
    if (nrow(graphdata)==0) return()
    
    grafica_cuenta(graphdata)
  }, 
  height = 900, 
  width = 1000)
  
  output$plot_p7 <- renderPlot({
    graphdata <- data_p7()
    if (nrow(graphdata)==0) return()
    
    grafica_cuenta(graphdata)
  }, 
  height = 900, 
  width = 1000)
  
  output$plot_p8 <- renderPlot({
    graphdata <- data_p8()
    if (nrow(graphdata)==0) return()
    
    grafica_cuenta(graphdata)
  }, 
  height = 900, 
  width = 1000)
  
  output$plot_p9 <- renderPlot({
    graphdata <- data_p9()
    if (nrow(graphdata)==0) return()
    
    grafica_cuenta(graphdata)
  }, 
  height = 900, 
  width = 1000)
  
  output$plot_p10 <- renderPlot({
    graphdata <- data_p10()
    if (nrow(graphdata)==0) return()
    
    grafica_cuenta(graphdata)
  }, 
  height = 900, 
  width = 1000)
  
  output$plot_p11 <- renderPlot({
    graphdata <- data_p11()
    if (nrow(graphdata)==0) return()
    
    grafica_cuenta(graphdata)
  }, 
  height = 900, 
  width = 1000)
  
  output$plot_p11a <- renderPlot({
    graphdata <- data_p11a()
    if (nrow(graphdata)==0) return()
    
    grafica_cuenta(graphdata)
  }, 
  height = 900, 
  width = 1000)
  
  output$plot_p17 <- renderPlot({
    graphdata <- data_p17()
    if (nrow(graphdata)==0) return()
    
    grafica_cuenta(graphdata)
  }, 
  height = 900, 
  width = 1000)
  
  output$plot_p20 <- renderPlot({
    graphdata <- data_p20()
    if (nrow(graphdata)==0) return()
    
    grafica_cuenta(graphdata)
  }, 
  height = 900, 
  width = 1000)
  
  
})