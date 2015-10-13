 # server.R
library(shiny)

shinyServer(function(input, output,session){

  #Para actualizar RadioButtons de la variable de cruce
  observe({
    if(input$filtrovar!="Total"){
      datos1<-df %>%
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
  
  
})