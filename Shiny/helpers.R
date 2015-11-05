  #Helpers-------

  
verifica_checkbox <- function(graphdata, panel_1 = FALSE, edad, genero, nivel, tipo_cliente, tipo_producto) {
  if(panel_1) {
    string_de_filtros <- c("Edad", "Género", "Nivel", "Tipo de cliente")
    vec <- c(
      length(edad),
      length(genero),
      length(nivel),
      length(tipo_cliente)
    )
  }
  else{
    string_de_filtros <- c("Edad", "Género", "Nivel", "Tipo de cliente", "Tipo de producto")
    vec <- c(
      length(edad),
      length(genero),
      length(nivel),
      length(tipo_cliente),
      length(tipo_producto)
    )
  }
  vec <- string_de_filtros[vec == 0]
  validate(need(!is.null(graphdata), paste("Escoge al menos una categoría de la variable \"", vec, "\".")))
}  
  
genera_data_p <- function(pregunta, 
                          vector_string, 
                          ru, 
                          edad, 
                          genero, 
                          nivel, 
                          tipo_cliente, 
                          tipo_producto, 
                          facet = "Total") {
  a <- filtro(pregunta,
              edad,
              genero,
              nivel,
              tipo_cliente,
              tipo_producto,
              facet)
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

#Con esta función obtenemos histogramas para las preguntas:

# ru = 1 cuando es una sola variable
# ru = 2 cuando sea respuesta multiple

cuenta <- function(datos,ru){
  
  if(ru==1){
    datos <- data.frame(datos)
    Top <-  apply(select(datos, -F_3),2,function(c){
      xtabs(datos$F_3~c,data=select(datos, -F_3))
    })
    
    Top<-data.frame(Top) 
    colnames(Top) <- c("Conteo")
    Top <- cbind(linea= rownames(Top),Top) %>%
      mutate(Porcentaje=Conteo/sum(Conteo)) %>%
      arrange(desc(Porcentaje)) %>%
      filter(linea != "") %>%
      filter(linea != " ")
    
    Top$linea <- factor(Top$linea,levels=unique(as.character(Top$linea)))
    
    return(Top)
  }
  
  if(ru ==2){
    
    conteos<- apply(select(datos, -F_3), 2, function(c){
      xtabs(datos$F_3 ~c,data=select(datos, -F_3))
    })
    
    aux<-data.frame()
    aux<-lapply(conteos,function(l){
      n<-length(l)
      if(n!=0){
        rbind(aux,data.frame(linea=as.character(names(l)),
                             valor=as.numeric(l[1:n]),
                             stringsAsFactors = F)) 
      }
    })
    
    aux3<-rbind_all(aux) %>%
      filter(linea !="") %>%
      filter(linea !=" ") %>%
      group_by(linea) %>%
      summarise(valor=sum(valor)) %>%
      mutate(Porcentaje=valor/sum(datos$F_3)) %>%
      data.frame() %>%
      arrange(desc(Porcentaje))
    
    aux3$linea <- factor(aux3$linea,levels=unique(as.character(aux3$linea)))
    
    return(aux3)
  }
}

#Ejemplo de la función-----
#cuenta(base_con[,95:104],2)

# imagen de Marca
gd <- function (mr){
  conteos<- apply(mr,2,function(c){
    xtabs(base_im$F_3~c,data=mr)
  })
  
  aux<-data.frame()
  aux2<-lapply(conteos,function(l){
    n<-length(l)
    rbind(aux,data.frame(linea=names(l),valor=as.numeric(l[1:n])))     
  })
  
  aux3<-Reduce('rbind',aux2) %>%
    filter(linea !=" ") %>%
    group_by(linea) %>%
    summarise(valor=sum(valor)) %>%
    mutate(Porcentaje=valor/nrow(base_im)) %>%
    data.frame() %>%
    arrange(desc(Porcentaje))
  
  return(aux3)
}

#gy <- lapply(frases,gd)

#Equity----
hj <- function(mr){
  prob <- (prop.table((table(mr)))*100)
}

p14 <- function(data){
  conteos<- data.frame(apply(mr,2,function(c){
    xtabs(base_im$F_3~c,data=mr)/nrow(base_im)
  }))
  conteos <- cbind(atributo= rownames(conteos),conteos)
  colnames(conteos) <- c( "Elektra","Coppel","Famsa","Bodega  Aurrera", "Walmart","Liverpool")
  aux<-gather(conteos,"Tienda","Valor",-atributo)
  
  aux$Tienda <- apply(as.data.frame(aux$Tienda),2,function(r){gsub("P14_","",r)})
  
  return(aux)
}

# Ejemplo de como correr la función
# preg <-base_eq[,c(42:47)]
# p14(preg)

#evaluación de publicidad----
p15 <- function(don){
  conteos<- lapply(don,function(c){
    xtabs(base_evap$F_3~c,data=don)
  })
  
  aux<-data.frame()
  aux2<-lapply(conteos,function(l){
    n<-length(l)
    rbind(aux,data.frame(linea=names(l),valor=as.numeric(l[1:n])))     
  })
  
  aux3<-Reduce('rbind',aux2) %>%
    filter(linea !=" ") %>%
    group_by(linea) %>%
    summarise(valor=sum(valor)) %>%
    mutate(Porcentaje=valor/nrow(base_im)) %>%
    data.frame() %>%
    arrange(desc(Porcentaje))
  return(aux3)
}

funp15<-function(){
  gy <- data.frame(do.call(cbind,lapply(marcas,p15)))
  gh <- gy[,c(1,3,6,9,12,15,18)]
  colnames(gh) <-c("Medio","Elektra","Coppel","Famsa","Bodega  Aurrera","Walmart","Liverpool")
  aux<-gather(gh,"Tienda","Porcentaje",-Medio)
  return(aux)
}

#atributos de spots----
attri <- function(df){
  conteos<- t(sapply(df,function(c){
    xtabs(base_evap$F_3~c,data=df)
  }))
  
  conteos <- data.frame(conteos)
  conteos$total <- apply(conteos,1,sum)
  conteos$Concuerda_completamente <- conteos[,1]/conteos[,4]
  conteos$Concuerda_en_parte <- conteos[,2]/conteos[,4]
  conteos$Concuerda_en_nada <- conteos[,3]/conteos[,4]
  conteos <- cbind(atributo = rownames(conteos), conteos)
  aux<-gather(conteos,"Concuerda","Valor",-atributo)
  
  return(aux)
}

# Ejemplo de como correr la función
# attri(base_evap[,c(115:128)])       

# evaluacion de comerciales----
evaluacion<-function(datos,ru){
  
  if(ru==1){
    datos <- data.frame(datos)
    Top <-  apply(datos,2,function(c){
      xtabs(base_evap$F_3~c,data=datos)
    })
    
    Top<-data.frame(Top)
    colnames(Top) <- c("Conteo")
    Top <- cbind(linea= rownames(Top),Top) %>%
      mutate(Porcentaje=Conteo/sum(Conteo)) %>%
      arrange(desc(Porcentaje))
    
    Top$linea <- factor(Top$linea,levels=unique(as.character(Top$linea)))
    
    return(Top)
  }
  
  if(ru ==2){
    
    conteos<- apply(datos,2,function(c){
      xtabs(base_evap$F_3~c,data=datos)
    })
    
    aux<-data.frame()
    aux2<-lapply(conteos,function(l){
      n<-length(l)
      rbind(aux,data.frame(linea=names(l),valor=as.numeric(l[1:n])))     
    })
    
    aux3<-Reduce('rbind',aux2) %>%
      filter(linea != "") %>%
      filter(linea != " ") %>%
      group_by(linea) %>%
      summarise(valor=sum(valor)) %>%
      mutate(Porcentaje=valor/nrow(base_con)) %>%
      data.frame() %>%
      arrange(desc(Porcentaje))
    return(aux3)
  }    
}

# Regresa un df, una lista con df's o un vector.
# Si la variable de facet es "Total" regresa df,
# en otro caso regresa una lista en la que cada elemento es un df con 
# la información de cada nivel de la variable de facet.
# Si además, el parámetro word_cloud es cierto, entonces regresa un vector con
# índices. Esto porque los wordclouds solo necesitan los índices.
filtro <- function(df,
                 edad=unique(df$Edad),
                 genero=unique(df$Genero),
                 nivel=unique(df$NIVEL),
                 tipo_cliente=unique(df$Tipo_Cliente),
                 tipo_producto=unique(df$P2_1),
                 facet = "Total",
                 word_cloud = FALSE){
  a <- df %>% mutate(idx = 1:nrow(.)) %>%
    filter(Edad %in% edad) %>%
    filter(Genero %in% genero) %>%
    filter(NIVEL %in% nivel)  %>%
    filter(Tipo_Cliente %in% tipo_cliente) %>%
    filter(P2_1 %in% tipo_producto)
  if(facet != "Total") {
    datos1 <- a %>%
      dplyr::select(x = one_of(facet))
    niveles <- unique(datos1$x)
    a <- lapply(niveles, function(i) 
      eval(substitute(filter(a, facet %in% i), 
                      list(facet = as.name(facet)))))
    names(a) <- niveles
  }
  if(word_cloud) a <- a$idx
  return(a) 
}

grafica_cuenta <- function(df, facet = "Total"){
  
  if(facet == "Total") {
    ggplot(df, aes(x=linea,y=Porcentaje)) + 
      geom_bar(stat="identity",fill="#2c3e50",colour="black") + 
      geom_text(aes(y=Porcentaje + .03 ,label=paste0(round(Porcentaje*100),"%")),
                colour='black',size=6) +
      theme(axis.text.x=element_text(angle=90,size=22),
            axis.text.y=element_text(size=22),
            panel.background=element_rect(fill='#C2D1E0'),
            strip.background=element_rect(fill="#2c3e50"),
            panel.border = element_rect(colour = "#2c3e50", fill=NA, size=1),
            strip.text.x = element_text(colour = 'white', size = 22),
            legend.text=element_text(size=24),
            legend.title=element_blank()) +
      scale_y_continuous(labels=percent) +
      ylab("") + xlab("")
  }
  else {
    ggplot(df, aes(x=linea, y=Porcentaje)) + 
      geom_bar(stat="identity",fill="#2c3e50",colour="black") + 
      facet_wrap(~ Nivel) +
      geom_text(aes(y=Porcentaje + .03 ,label=paste0(round(Porcentaje*100),"%")),
                colour='black',size=6) +
      theme(axis.text.x=element_text(angle=90,size=22),
            axis.text.y=element_text(size=22),
            panel.background=element_rect(fill='#C2D1E0'),
            strip.background=element_rect(fill="#2c3e50"),
            panel.border = element_rect(colour = "#2c3e50", fill=NA, size=1),
            strip.text.x = element_text(colour = 'white', size = 22),
            legend.text=element_text(size=24),
            legend.title=element_blank()) +
      scale_y_continuous(labels = percent) +
      ylab("") + xlab("")
  }
}

grafica_top_share<-function(df_top, df_share, facet = "Total"){
  if(facet == "Total")   {
    names(df_top)[2] <- "valor"
    df_top$Tipo <- "Top"
    df_share$Tipo <- "Share"
    l1<-unique(as.character(df_top$linea))
    l2<-unique(as.character(df_share$linea))
    l3<-data.frame(linea=unique(c(l1,l2)),stringsAsFactors = F)

    aux2 <- l3 %>%
      left_join(df_share,by="linea") %>% arrange(desc(Porcentaje))
    
    aux2$linea<-factor(aux2$linea,levels=as.character(aux2$linea))
    
    aux1 <- l3 %>%
      left_join(df_top,by="linea")
    
    aux1$linea<-factor(aux1$linea,levels=as.character(aux2$linea))
    
    ggplot() +
      geom_bar(data=aux2,aes(x=linea,y=Porcentaje,fill="Share"),stat="identity",colour="black") +
      geom_text(data=aux2,aes(x=linea,y=Porcentaje + .03 ,label=paste0(round(Porcentaje*100),"%")),
                colour='black',size=6) +
      geom_bar(data=aux1,aes(x=linea,y=Porcentaje,fill="Top"), alpha=0.4,stat="identity",colour="black") +
      geom_text(data=aux1,aes(x=linea,y=Porcentaje - .05 ,label=paste0(round(Porcentaje*100),"%")),
                colour="#2c3e50",size=6) +
      theme(axis.text.x=element_text(angle=90,size=22),
            axis.text.y=element_text(size=22),
            panel.background=element_rect(fill='#C2D1E0'),
            strip.background=element_rect(fill="#2c3e50"),
            panel.border = element_rect(colour = "#2c3e50", fill=NA, size=1),
            strip.text.x = element_text(colour = 'white', size = 22),
            legend.text=element_text(size=24),
            legend.title=element_blank()) +
      ylim(0,1) + 
      scale_y_continuous(labels=percent) +
      scale_fill_manual(name="Tipo",
                          values=c(Share="#2c3e50", Top="#C2D1E0")) +
      ylab("") + xlab("")
  }
  else {
    
    df_top$Tipo <- "Top"
    df_share$Tipo <- "Share"
    
    l1<-unique(as.character(df_top$linea))
    l2<-unique(as.character(df_share$linea))
    l3<-data.frame(linea=unique(c(l1,l2)),stringsAsFactors = F)
    
    aux2 <- l3 %>%
      left_join(df_share,by="linea") %>% arrange(desc(Porcentaje))
    
    aux2$linea<-factor(aux2$linea,levels=as.character(aux2$linea))
    
    aux1 <- l3 %>%
      left_join(df_top,by="linea")
    
    aux1$linea<-factor(aux1$linea,levels=as.character(aux2$linea))
    
    Niveles <- sort(unique(aux1$Nivel)[complete.cases(unique(aux1$Nivel))])
    
    plotlist <- lapply(1:length(Niveles), function(x) {
      #legend_plot <- ifelse(x == length(Niveles), "bottom", "none")
      legend_plot <- "bottom"
      #       if(length(Niveles)%%2 == 0){
      #         legend_plot = "bottom"
      #       }
      #       else {
      #         legend_plot <- ifelse(x == ceiling(length(Niveles)/2), "bottom", "none")
      #       }
      aux1 <- filter(aux1, Nivel == Niveles[x])
      aux2 <- filter(aux2, Nivel == Niveles[x]) %>% arrange(desc(Porcentaje))
      aux2$linea <- factor(aux2$linea, levels = aux2$linea)
      aux1$linea <- factor(aux1$linea, levels = aux2$linea)
      ggplot() +
        geom_bar(data = aux2,aes(x=linea,y=Porcentaje,fill="Share"),stat="identity",colour="black") +
        geom_text(data=aux2,aes(x=linea,y=Porcentaje + .03 ,label=paste0(round(Porcentaje*100),"%")),
                  colour='black',size=6) +
        geom_bar(data=aux1,aes(x=linea,y=Porcentaje,fill="Top"), alpha=0.4,stat="identity",colour="black") +
        geom_text(data=aux1,aes(x=linea,y=Porcentaje - .05 ,label=paste0(round(Porcentaje*100),"%")),
                  colour="#2c3e50",size=6) +
        theme(axis.text.x=element_text(angle=90,size=22),
              axis.text.y=element_text(size=22),
              panel.background=element_rect(fill='#C2D1E0'),
              strip.background=element_rect(fill="#2c3e50"),
              panel.border = element_rect(colour = "#2c3e50", fill=NA, size=1),
              strip.text.x = element_text(colour = 'white', size = 22),
              legend.text=element_text(size=24),
              legend.title=element_blank()) +
        ylab("") + xlab("") + 
        ylim(0,1) +
        scale_y_continuous(labels=percent) +
        scale_fill_manual(name="Tipo",
                          values=c(Share="#2c3e50", Top="#C2D1E0")) +
        ggtitle(Niveles[x]) +
        theme(legend.position = legend_plot)
    })
    
    multiplot(plotlist = plotlist, cols=length(plotlist))
    
#     ggplot() +
#       geom_bar(data=aux2,aes(x=linea,y=Porcentaje,fill="Share"),stat="identity",colour="black") +
#       geom_text(data=aux2,aes(x=linea,y=Porcentaje + .03 ,label=paste0(round(Porcentaje*100),"%")),
#                 colour='black',size=6) +
#       geom_bar(data=aux1,aes(x=linea,y=Porcentaje,fill="Top"), alpha=0.4,stat="identity",colour="black") +
#       geom_text(data=aux1,aes(x=linea,y=Porcentaje - .05 ,label=paste0(round(Porcentaje*100),"%")),
#                 colour="#2c3e50",size=6) +
#       facet_wrap(~Nivel) +
#       #facet_wrap(data = aux1, ~Nivel) +
#       theme(axis.text.x=element_text(angle=90,size=22),
#             axis.text.y=element_text(size=22),
#             panel.background=element_rect(fill='#C2D1E0'),
#             strip.background=element_rect(fill="#2c3e50"),
#             panel.border = element_rect(colour = "#2c3e50", fill=NA, size=1),
#             strip.text.x = element_text(colour = 'white', size = 22),
#             legend.text=element_text(size=24),
#             legend.title=element_blank()) +
#       scale_y_continuous(labels=percent) +
#       scale_fill_manual(name="Tipo",
#                         values=c(Share="#2c3e50", Top="#C2D1E0")) +
#       ylab("") + xlab("")
    
#     ggplot() +
#       geom_bar(data=aux2,aes(x=linea,y=Porcentaje,fill="Share"),stat="identity",colour="black") +
#       geom_text(data=aux2,aes(x=linea,y=Porcentaje + .03 ,label=paste0(round(Porcentaje*100),"%")),
#                 colour='black',size=6) +
#       geom_bar(data=aux1,aes(x=linea,y=Porcentaje,fill="Top"), alpha=0.4,stat="identity",colour="black") +
#       geom_text(data=aux1,aes(x=linea,y=Porcentaje - .05 ,label=paste0(round(Porcentaje*100),"%")),
#                 colour="#2c3e50",size=6) +
#       facet_wrap(~Nivel) +
#       #facet_wrap(data = aux1, ~Nivel) +
#       theme(axis.text.x=element_text(angle=90,size=22),
#             axis.text.y=element_text(size=22),
#             panel.background=element_rect(fill='#C2D1E0'),
#             strip.background=element_rect(fill="#2c3e50"),
#             panel.border = element_rect(colour = "#2c3e50", fill=NA, size=1),
#             strip.text.x = element_text(colour = 'white', size = 22),
#             legend.text=element_text(size=24),
#             legend.title=element_blank()) +
#       scale_y_continuous(labels=percent) +
#       scale_fill_manual(name="Tipo",
#                         values=c(Share="#2c3e50", Top="#C2D1E0")) +
#       ylab("") + xlab("")
    
#     aux1 <- l3 %>%
#       left_join(df_top,by="linea") 
#     
#     aux1$linea<-factor(aux1$linea,levels=as.character(aux1$linea))
#     
#     aux2 <- l3 %>%
#       left_join(df_share,by="linea")
#     
#     aux2$linea<-factor(aux2$linea,levels=as.character(aux2$linea))
#     
#     names(aux1) <- c("linea", "Nivel", "Conteo", "Porcentaje", "Tipo")
#     names(aux2) <- c("linea", "Nivel", "Conteo", "Porcentaje", "Tipo")
#     
#     aux_r <- rbind(aux1, aux2) %>% filter(complete.cases(.))
#     aux_r$Tipo <- factor(aux_r$Tipo, levels = c("Share", "Top"))
#     aux_r$Tipo <- factor(aux_r$Tipo, levels = c("Top", "Share"))
#     
#     ggplot(aux_r) +
#       geom_bar(aes(x = linea, y = Porcentaje, fill = Tipo, alpha = Tipo), 
#                position = "identity", stat="identity", colour="black") +
#       facet_wrap(~Nivel) +
#       geom_text(aes(x = linea, y = Porcentaje + .03 , label = paste0(round(Porcentaje*100),"%")),
#                 colour='black',size=6) +
#       theme(axis.text.x=element_text(angle=90,size=22),
#             axis.text.y=element_text(size=22),
#             panel.background=element_rect(fill='#C2D1E0'),
#             strip.background=element_rect(fill="#2c3e50"),
#             panel.border = element_rect(colour = "#2c3e50", fill=NA, size=1),
#             strip.text.x = element_text(colour = 'white', size = 22),
#             legend.text=element_text(size=24),
#             legend.title=element_blank()) +
#       scale_y_continuous(labels=percent) +
#       scale_fill_manual(name="Tipo",
#                         values=c(Top="#2c3e50", Share="#C2D1E0")) +
#                         #values=c(Share="red", Top="blue")) +
#       scale_alpha_manual(values=c(Share = 0.5, Top = 1)) +
#       ylab("") + xlab("")
    
  }
}

tam_base<-function(datos, edad, genero, nivel, tc, tp){
  renglones <- nrow(datos)
  a <- filtro(datos,
              edad,
              genero,
              nivel,
              tc,
              tp,
              "Total")
  n <- nrow(a)
  ifelse(n<=30,color<-"red",color<-"black") 
  ifelse(n<=30,txt<-"base inadecuado para hacer inferencias",txt<-"base adecuado")
  return(list(round(n),color,txt))
}

nubes<-function(palabras_prohibidas, base){
  #genera vector de la variable
  p<-VCorpus(VectorSource(na.omit(base)))
  
  #Elimina espacios
  p<-tm_map(p,stripWhitespace)
  #numeros
  p<-tm_map(p,removeNumbers)
  #remueve signos de puntuaci?n
  p<-tm_map(p,removePunctuation)
  #min?sculas
  p<-tm_map(p,content_transformer(tolower))
  #elimina palabras no requeridas
  p<-tm_map(p,removeWords,palabras_prohibidas)
  
  
  #genera matriz de palabras
  a<-TermDocumentMatrix(p)
  b<-rowSums(as.matrix(a))
  names(b)<-rownames(as.matrix(a))
  b<-sort(b,decreasing=T)
  
  nube<-wordcloud(names(b),b,min.freq=1,scale=c(16,.1),max.word=Inf,
                  random.order=FALSE, rot.per=0.15, 
                  colors=brewer.pal(8, "Dark2"))

  
  return(nube)
  
}

menu1 <- function(){
  fluidPage(
     wellPanel(
        helpText(h4('Selecciona las variables sobre las que quieres filtrar.')),
        checkboxGroupInput(
          'filtroEdad1', 
          label = h4('Edad'),
          choices = list(
            '25 - 35' = '25-35',
            '36 - 55' = '36-55'),
          inline = T,
          selected = c('25-35', '36-55')),
        checkboxGroupInput(
          'filtroGen1', 
          label = h4('Género'),
          choices = list(
            'Masculino' = 'Hombre',
            'Femenino' = 'Mujer'),
          inline = T,
          selected = c('Hombre', 'Mujer')),
        checkboxGroupInput(
          'filtroNiv1', 
          label = h4('Nivel'),
          choices = list(
            'C-' = 'C-',
            'D+' = 'D+',
            'D' = 'D'),
          inline = T,
          selected = c(
            'C-',
            'D+',
            'D')),
        checkboxGroupInput(
          'filtroTipoCliente1', 
          label = h4('Tipo de cliente'),
          choices = list(
            'Frecuente de Elektra' = 'Cliente frecuente Elektra',
            'Frecuente de la competencia' = 'Cliente frecuente de la competencia'),
          selected = c('Cliente frecuente Elektra', 'Cliente frecuente de la competencia')),
        #submitButton(text = "Graficar"),
        helpText(h4("Selecciona la variable con la que quieres cruzar."),align="left"),
        selectInput(
          "facet1",
          label = "",
          choices = list(
            "Edad" = "Edad",
            "Género" = "Genero",
            "Nivel" = "NIVEL",
            "Tipo de cliente" = "Tipo_Cliente",
            "Total"
          ), selected = "Total")
      )
  )}

menu2<-function(){
  fluidPage(
    wellPanel(
      helpText(h4('Selecciona las variables sobre las que quieres filtrar.')),
      checkboxGroupInput(
        'filtroEdad2', 
        label = h4('Edad'),
        choices = list(
          '25 - 35' = '25-35',
          '36 - 55' = '36-55'),
        inline = T,
        selected = c('25-35', '36-55')),
      checkboxGroupInput(
        'filtroGen2', 
        label = h4('Género'),
        choices = list(
          'Masculino' = 'Hombre',
          'Femenino' = 'Mujer'),
        inline = T,
        selected = c('Hombre', 'Mujer')),
      checkboxGroupInput(
        'filtroNiv2', 
        label = h4('Nivel'),
        choices = list(
          'C-' = 'C-',
          'D+' = 'D+',
          'D' = 'D'),
        inline = T,
        selected = c(
          'C-',
          'D+',
          'D')),
      checkboxGroupInput(
        'filtroTipoCliente2', 
        label = h4('Tipo de cliente'),
        choices = list(
          'Frecuente de Elektra' = 'Cliente frecuente Elektra',
          'Frecuente de la competencia' = 'Cliente frecuente de la competencia'),
        selected = c('Cliente frecuente Elektra', 'Cliente frecuente de la competencia')),
      checkboxGroupInput(
        'filtroTipoProducto2', 
        label = h4('Tipo de producto'),
        choices = list(
          'Electrónica' = 'Electrónica',
          'Línea Blanca' = 'Linea Blanca',
          'Telefonía' = "Telefonia",
          'Muebles' = 'Muebles',
          "Cómputo" = "Cómputo"),
        selected = c('Electrónica', 'Linea Blanca',
                     'Telefonia', 'Muebles',
                     'Cómputo')),
      #submitButton(text = "Graficar"),
      helpText(h4("Selecciona la variable con la que quieres cruzar."),align="left"),
      selectInput(
        "facet2",
        label = "",
        choices = list(
          "Edad" = "Edad",
          "Género" = "Genero",
          "Nivel" = "NIVEL",
          "Tipo de cliente" = "Tipo_Cliente",
          "Total"
        ), selected = "Total"),
#       radioButtons("filtrocat2", 
#                    label = h4(""),
#                    choices=list('')
#       )
#       #downloadButton('downloadFile', 'Descargar archivo')
        div(h4("Este panel no funciona en ninguna de las pestañas con nombre Funnel."),
                 style = "color:red",align="left")
    )
  )}

menu3<-function(){
  fluidPage(
    wellPanel(
      helpText(h4('Selecciona las variables sobre las que quieres filtrar.')),
      checkboxGroupInput(
        'filtroEdad3', 
        label = h4('Edad'),
        choices = list(
          '25 - 35' = '25-35',
          '36 - 55' = '36-55'),
        inline = T,
        selected = c('25-35', '36-55')),
      checkboxGroupInput(
        'filtroGen3', 
        label = h4('Género'),
        choices = list(
          'Masculino' = 'Hombre',
          'Femenino' = 'Mujer'),
        inline = T,
        selected = c('Hombre', 'Mujer')),
      checkboxGroupInput(
        'filtroNiv3', 
        label = h4('Nivel'),
        choices = list(
          'C-' = 'C-',
          'D+' = 'D+',
          'D' = 'D'),
        inline = T,
        selected = c(
          'C-',
          'D+',
          'D')),
      checkboxGroupInput(
        'filtroTipoCliente3', 
        label = h4('Tipo de cliente'),
        choices = list(
          'Frecuente de Elektra' = 'Cliente frecuente Elektra',
          'Frecuente de la competencia' = 'Cliente frecuente de la competencia'),
        selected = c('Cliente frecuente Elektra', 'Cliente frecuente de la competencia')),
      checkboxGroupInput(
        'filtroTipoProducto3', 
        label = h4('Tipo de producto'),
        choices = list(
          'Electronica' = 'Electrónica',
          'Línea Blanca' = 'Linea Blanca',
          'Telefonía' = "Telefonia",
          'Muebles' = 'Muebles',
          "Cómputo" = "Cómputo"),
        selected = c('Electrónica', 'Linea Blanca',
                     'Telefonia', 'Muebles',
                     'Cómputo'))
    )
  )}

menu4<-function(){
  fluidPage(
    wellPanel(
      helpText(h4('Selecciona las variables sobre las que quieres filtrar.')),
      checkboxGroupInput(
        'filtroEdad4', 
        label = h4('Edad'),
        choices = list(
          '25 - 35' = '25-35',
          '36 - 55' = '36-55'),
        inline = T,
        selected = c('25-35', '36-55')),
      checkboxGroupInput(
        'filtroGen4', 
        label = h4('Género'),
        choices = list(
          'Masculino' = 'Hombre',
          'Femenino' = 'Mujer'),
        inline = T,
        selected = c('Hombre', 'Mujer')),
      checkboxGroupInput(
        'filtroNiv4', 
        label = h4('Nivel'),
        choices = list(
          'C-' = 'C-',
          'D+' = 'D+',
          'D' = 'D'),
        inline = T,
        selected = c(
          'C-',
          'D+',
          'D')),
      checkboxGroupInput(
        'filtroTipoCliente4', 
        label = h4('Tipo de cliente'),
        choices = list(
          'Frecuente de Elektra' = 'Cliente frecuente Elektra',
          'Frecuente de la competencia' = 'Cliente frecuente de la competencia'),
        selected = c('Cliente frecuente Elektra', 'Cliente frecuente de la competencia')),
      checkboxGroupInput(
        'filtroTipoProducto4', 
        label = h4('Tipo de producto'),
        choices = list(
          'Electrónica' = 'Electrónica',
          'Línea Blanca' = 'Linea Blanca',
          'Telefonía' = "Telefonia",
          'Muebles' = 'Muebles',
          "Cómputo" = "Cómputo"),
        selected = c('Electrónica', 'Linea Blanca',
                     'Telefonia', 'Muebles',
                     'Cómputo'))
    )
  )}

menu5<-function(){
  fluidPage(
    wellPanel(
      helpText(h4('Selecciona las variables sobre las que quieres filtrar.')),
      checkboxGroupInput(
        'filtroEdad5', 
        label = h4('Edad'),
        choices = list(
          '25 - 35' = '25-35',
          '36 - 55' = '36-55'),
        inline = T,
        selected = c('25-35', '36-55')),
      checkboxGroupInput(
        'filtroGen5', 
        label = h4('Género'),
        choices = list(
          'Masculino' = 'Hombre',
          'Femenino' = 'Mujer'),
        inline = T,
        selected = c('Hombre', 'Mujer')),
      checkboxGroupInput(
        'filtroNiv5', 
        label = h4('Nivel'),
        choices = list(
          'C-' = 'C-',
          'D+' = 'D+',
          'D' = 'D'),
        inline = T,
        selected = c(
          'C-',
          'D+',
          'D')),
      checkboxGroupInput(
        'filtroTipoCliente5', 
        label = h4('Tipo de cliente'),
        choices = list(
          'Frecuente de Elektra' = 'Cliente frecuente Elektra',
          'Frecuente de la competencia' = 'Cliente frecuente de la competencia'),
        selected = c('Cliente frecuente Elektra', 'Cliente frecuente de la competencia')),
      checkboxGroupInput(
        'filtroTipoProducto5', 
        label = h4('Tipo de producto'),
        choices = list(
          'Electrónica' = 'Electrónica',
          'Línea Blanca' = 'Linea Blanca',
          'Telefonía' = "Telefonia",
          'Muebles' = 'Muebles',
          "Cómputo" = "Cómputo"),
        selected = c('Electrónica', 'Linea Blanca',
                     'Telefonia', 'Muebles',
                     'Cómputo'))
    )
  )}

media.ponderada<-function(varprom,bateria,varseg,medias=NULL){
  require(dplyr)
  bateriat<-bateria %>%
    select(one_of(varprom,varseg,'ponderador'))
  for(i in 1:length(levels(bateriat[,2]))){
    bateriatt<-bateriat[bateriat[,2]==levels(bateriat[,2])[i],]
    medias<-c(medias,weighted.mean(bateriatt[,1],bateriatt[,3]))
  }
  medias
}

grafica_dimensiones<-function(bateria_subconjunto){
  r1<-media.ponderada('status',bateria_subconjunto,'tienda')
  r2<-media.ponderada('post_venta',bateria_subconjunto,'tienda')
  r3<-media.ponderada('basicos',bateria_subconjunto,'tienda')
  r4<-media.ponderada('experiencia_en_tienda',bateria_subconjunto,'tienda')
  r5<-media.ponderada('economia',bateria_subconjunto,'tienda')
  r6<-media.ponderada('encontro_lo_que_buscaba',bateria_subconjunto,'tienda')
  r7<-media.ponderada('atencionr',bateria_subconjunto,'tienda')
  
  r<-rbind(r1,r2,r3,r4,r5,r6,r7)
  r<-data.frame(r)
  r$dimension<-c('Status','Post-Venta','Básicos','Experiencia en tienda','Economía',
                 'Encontró lo que buscaba','Atención rápida')
  
  names(r)<-c('Bodega ','Coppel','Elektra','Famsa','Liverpool','Walmart','dimension')
  
  r2<-r %>%
    tidyr::gather('concepto','valor',-dimension)
  
  
  names(r2)<-c('dimensión','concepto','valor')
  
  
  grafica_comparativa<-ggplot(data=r2,aes(x=dimensión,y=valor)) + 
    geom_point(aes(group=concepto),size=11) +
    geom_hline(aes(yintercept=0),linetype='dotted',size=2) +
    geom_point(aes(colour=concepto,group=concepto),size=10) +
    scale_colour_manual(values = c('forestgreen','goldenrod1', 
                                   'red','royalblue4','deeppink','dodgerblue2')) +
    theme(axis.text.x=element_text(angle=90,size=20, hjust = 1),
          axis.text.y=element_text(size=22),
          panel.background=element_rect(fill='#C2D1E0'),
          strip.background=element_rect(fill="#2c3e50"),
          panel.border = element_rect(colour = "#2c3e50", fill=NA, size=1),
          strip.text.x = element_text(colour = 'white', size = 22),
          legend.title=element_blank(),
          legend.text = element_text(size = 22),
          axis.title=element_text(size=22),
          plot.title = element_text(size=22)) +
    xlab('Dimensión') +
    ylab('Diferencias') +
    ggtitle('Score de conceptos por dimensión')
 
  return(grafica_comparativa)
  
}

grafica_bateria<-function(bateria_subconjunto){
  
  basebis<- bateria_subconjunto %>%
    select(one_of(c(names(bateria_subconjunto)[1:22]),'ponderador','tienda'))
  
  for(i in 1:22){
    basebis[,i]<-as.numeric(basebis[,i])
  }
  
  
  nombres<-as.list(names(basebis)[1:22])
  
  r<-lapply(nombres,media.ponderada,basebis,'tienda')
  
  r<-do.call(rbind,r)
  r<-as.data.frame(r)
  r$dimension<-names(basebis)[1:22]
  names(r)<-c('Bodega ','Coppel','Elektra','Famsa','Liverpool','Walmart','dimension')
  
  
  r<-r[c(22,
         1,5,
         2,6,17,
         8,9,10,
         12,4,20,19,18,11,
         16,7,13,3,
         15,14,21),]
  
  r$dimension<-factor(r$dimension,levels=r$dimension)
  
  
  r2<-r %>%
    tidyr::gather('concepto','valor',-dimension)
  
  
  names(r2)<-c('dimensión','concepto','valor')
  
  
  
  r2[,3]<-(r2[,3]-1)*100
  
  
  
  grafica_comparativa<-ggplot(data=r2,aes(x=dimensión,y=valor)) + 
    geom_point(aes(group=concepto),size=11) +
    geom_rect(aes(xmin=1.5, xmax=3.5, ymin=-Inf,ymax=Inf), alpha=0.01,fill='lightskyblue')+
    geom_rect(aes(xmin=6.5, xmax=9.5, ymin=-Inf,ymax=Inf), alpha=0.01,fill='lightskyblue')+
    geom_rect(aes(xmin=15.5, xmax=19.5, ymin=-Inf,ymax=Inf), alpha=0.01,fill='lightskyblue')+
    geom_point(aes(colour=concepto,group=concepto),size=10) +
    scale_colour_manual(values = c('forestgreen','goldenrod1', 
                                   'red','royalblue4','deeppink','dodgerblue2')) +
    theme(axis.text.x=element_text(size=18, hjust = 1),
          axis.text.y=element_text(size=22),
          panel.background=element_rect(fill='#C2D1E0'),
          strip.background=element_rect(fill="#2c3e50"),
          panel.border = element_rect(colour = "#2c3e50", fill=NA, size=1),
          strip.text.x = element_text(colour = 'white', size = 22),
          legend.title=element_blank(),
          legend.text = element_text(size = 22),
          axis.title=element_text(size=22),
          plot.title = element_text(size=22),
          panel.grid.major = element_line(colour = "gray50"),
          panel.grid.minor = element_line(colour = "gray50")) +
    xlab('Concepto') +
    ylab('Porcentaje de menciones') +
    ggtitle('Porcentaje de menciones por concepto')+
    theme(axis.text.x = element_text(angle = 90, hjust = 1))

  return(grafica_comparativa)
  
}

grafica_bateria_equity<-function(bateria_subconjunto){
  
  basebis<- bateria_subconjunto %>%
    select(one_of(c(names(bateria_subconjunto)[1:6]),'ponderador','tienda'))
  
  for(i in 1:7){
    basebis[,i]<-as.numeric(basebis[,i])
  }
  
  
  nombres<-as.list(names(basebis)[1:6])
  
  r<-lapply(nombres,media.ponderada,basebis,'tienda')
  
  r<-do.call(rbind,r)
  r<-as.data.frame(r)
  r$dimension<-names(basebis)[1:6]
  names(r)<-c('Bodega ','Coppel','Elektra','Famsa','Liverpool','Walmart','dimension')
  
  
  r$dimension<-factor(r$dimension,levels=r$dimension)
  
  
  r2<-r %>%
    tidyr::gather('concepto','valor',-dimension)
  
  
  names(r2)<-c('dimensión','concepto','valor')

  grafica_comparativa<-ggplot(data=r2,aes(x=dimensión,y=valor)) + 
    geom_point(aes(group=concepto),size=11) +
    #geom_rect(aes(xmin=6.5, xmax=7.5, ymin=-Inf,ymax=Inf), alpha=0.005,fill='blue')+
    
    theme(
      axis.text=element_text(size=16))+
    
    
    geom_point(aes(colour=concepto,group=concepto),size=10) +
    scale_colour_manual(values = c('forestgreen','goldenrod1', 
                                   'red','royalblue4','deeppink','dodgerblue2')) +
    ylim(4, 10) +
    theme(axis.text.x=element_text(angle=90,size=20, hjust = 1),
          axis.text.y=element_text(size=22),
          panel.background=element_rect(fill='#C2D1E0'),
          strip.background=element_rect(fill="#2c3e50"),
          panel.border = element_rect(colour = "#2c3e50", fill=NA, size=1),
          strip.text.x = element_text(colour = 'white', size = 22),
          legend.title=element_blank(),
          legend.text = element_text(size = 22),
          axis.title=element_text(size=22),
          plot.title = element_text(size=22)) +
    xlab('Variable') +
    ylab('Promedio') +
    ggtitle('Promedio de calificación por variable')
  
  
  
  
  
  return(grafica_comparativa)
  
}

grafica_bateria_equity_score<-function(bateria_subconjunto){
  
  basebis<- bateria_subconjunto %>%
    select(one_of(c(names(bateria_subconjunto)[7]),'ponderador','tienda'))

  basebis[,1]<-as.numeric(basebis[,1])

  nombres<-as.list(names(basebis)[1])
  
  r<-lapply(nombres,media.ponderada,basebis,'tienda')
  
  r<-do.call(rbind,r)
  r<-as.data.frame(r)
  r$dimension<-names(basebis)[1]
  names(r)<-c('Bodega ','Coppel','Elektra','Famsa','Liverpool','Walmart','dimension')
  
  
  r$dimension<-factor(r$dimension,levels=r$dimension)
  
  
  r2<-r %>%
    tidyr::gather('concepto','valor',-dimension)
  
  
  names(r2)<-c('dimensión','concepto','valor')
 
  grafica_comparativa<-ggplot(data=r2,aes(x=dimensión,y=valor)) + 
    geom_point(aes(group=concepto),size=11) +
    theme(axis.text.x=element_blank(),
          axis.text.y=element_text(size=22),
          panel.background=element_rect(fill='#C2D1E0'),
          strip.background=element_rect(fill="#2c3e50"),
          panel.border = element_rect(colour = "#2c3e50", fill=NA, size=1),
          strip.text.x = element_text(colour = 'white', size = 22),
          legend.title=element_blank(),
          legend.text = element_text(size = 22),
          axis.title=element_text(size=22),
          plot.title = element_text(size=22)) +
    ylim(4, 10) +
    geom_point(aes(colour=concepto,group=concepto),size=10) +
    scale_colour_manual(values = c('forestgreen','goldenrod1', 
                                   'red','royalblue4','deeppink','dodgerblue2')) +
    xlab('Score Equity') +
    ylab('') +
    ggtitle('Equity score por tienda')
 
  return(grafica_comparativa)
  
}



fgrafpub<-function(df,marca,medios){
  
  
  
  filtro<-switch(marca,
         'Elektra'=0,
         'Coppel'=1,
         'Famsa'=2,
         'Bodega'=3,
         'Walmart'=4,
         'Liverpool'=5)
  
  
  
  df[df==' ']<-NA
  variables<-(316:328)+(filtro*13)
  conteos<-df[0,variables]
  for(i in 1:length(conteos)){
    conteos[,i]<-as.numeric(conteos[,i])
  }
  nombres<-c('televisión',
             'radio',
             'revistas',
             'periódicos',
             'espectaculares',
             'paradas y posters',
             'vallas y bardas',
             'cines',
             'autobuses y metro',
             'internet',
             'centros comerciales',
             'redes sociales',
             'otro')
  names(conteos)<-nombres
  conteos<-as.list(conteos)
  j=1
  for(i in variables){
    conteos[[j]]<-c(which(!is.na(df[,i])))
    j<-j+1
  }
  library(VennDiagram)
  
  tot<-length(unique(unlist((conteos[as.numeric(unlist(medios))]))))
  
  
  tabla<-as.numeric(table(unlist((conteos[as.numeric(unlist(medios))]))))
  
  tabla<-summary(as.factor(tabla))
  tabla<-tabla/tot*100
  
  
  
  
  tabla<-paste(round(tabla,2),'%',sep='')
  
  tabla2<-c('personas se alcanzan 1 vez',
            'personas se alcanzan 2 veces',
            'personas se alcanzan 3 veces',
            'personas se alcanzan 4 veces',
            'personas se alcanzan 5 veces')
  
  
  titulo<-paste(tabla,tabla2)
  
  

  
  d1<-venn.diagram(x=conteos[as.numeric(unlist(medios))],
                   col=c('blue','green','red','yellow','gray')[1:length(unlist(medios))],
                   fill=c('blue','green','red','yellow','gray')[1:length(unlist(medios))],
                   lwd=1,
                   lty=3,
                   margin=0.05,
                   filename=NULL,
                   main.cex=1.7,
                   cat.cex=1.5,
                   main.pos=c(.8,1),
                   main=paste('En total se alcanzan ', tot,'personas\n',paste(titulo[1:min(length(unlist(medios)),3)],collapse='\n')))
  return(grid.draw(d1))
  
  
  
  
  
  
}




grafica_calificacion_comerciales<-function(df){
  
  
  names(df)
  
  bateria<-425:438
  
  head(df[,bateria])
  levels(df[,bateria[1]])
  df[,bateria][df[,bateria]==' ']<-NA
  df[,bateria]<-droplevels(df[,bateria])
  
  df2<-df[,bateria]
  
  for(i in 1:length(bateria)){
    df2[,i]<-as.numeric(df2[,i])
  }
  
  df2<-4-df2
  
  elektra<-apply(df2,2,weighted.mean,df$F_3,na.rm=T)
  
  
  
  
  bateria2<-442:455
  
  
  head(df[,bateria2])
  levels(df[,bateria2[1]])
  df[,bateria2][df[,bateria2]==' ']<-NA
  df[,bateria2]<-droplevels(df[,bateria2])
  
  df2<-df[,bateria2]
  
  for(i in 1:length(bateria2)){
    df2[,i]<-as.numeric(df2[,i])
  }
  
  df2<-4-df2
  
  coppel<-apply(df2,2,weighted.mean,df$F_3,na.rm=T)
  
  dt<-as.data.frame(cbind(elektra,coppel))
  
  dt$variable<-c('interesante',
                 'para gente como yo',
                 'me dice algo importante',
                 'creíble',
                 'se adecúa a la manera en\nque percibo esta marca',
                 'con humor',
                 'entretenido',
                 'único',
                 'confuso',
                 'irrtante',
                 'ofensivo',
                 'me hace pernsar\nque es diferente',
                 'dice algo nuevo',
                 'medice algo que me\n interesa particularmente')
  
  
  
  r2<-dt %>%
    tidyr::gather('concepto','valor',-variable)
  
  
  names(r2)<-c('variable','concepto','valor')
  
  
  g<-ggplot(data=r2,aes(x=variable,y=valor)) + 
    geom_point(aes(group=concepto),size=11) +
    #geom_rect(aes(xmin=6.5, xmax=7.5, ymin=-Inf,ymax=Inf), alpha=0.005,fill='blue')+
    
    theme(
      axis.text=element_text(size=16))+
    
    
    geom_point(aes(colour=concepto,group=concepto),size=10) +
    scale_colour_manual(values = c('red','goldenrod1')) +
    ylim(1,3) +
    theme(axis.text.x=element_text(angle=90,size=20, hjust = 1),
          axis.text.y=element_text(size=22),
          panel.background=element_rect(fill='#C2D1E0'),
          strip.background=element_rect(fill="#2c3e50"),
          panel.border = element_rect(colour = "#2c3e50", fill=NA, size=1),
          strip.text.x = element_text(colour = 'white', size = 22),
          legend.title=element_blank(),
          legend.text = element_text(size = 22),
          axis.title=element_text(size=22),
          plot.title = element_text(size=22)) +
    xlab('Variable') +
    ylab('Promedio') +
    ggtitle('Promedio de calificación por variable')
  
  
  return(g)
  
}




abiertas <- function(datos,ru){
        if(ru==1){
                conteos<- apply(select(datos, -F_3), 2, function(c){
                        xtabs(datos$F_3 ~c,data=select(datos, -F_3))
                })
                
                aux<-data.frame()
                aux<-lapply(conteos,function(l){
                        n<-length(l)
                        if(n!=0){
                                rbind(aux,data.frame(linea=as.character(names(l)),
                                                     valor=as.numeric(l[1:n]),
                                                     stringsAsFactors = F)) 
                        }
                })
                
                aux3<-rbind_all(aux) %>%
                        filter(linea !="") %>%
                        filter(linea !=" ") %>%
                        group_by(linea) %>%
                        summarise(valor=sum(valor)) %>%
                        mutate(Porcentaje=valor/sum(datos$F_3)) %>%
                        data.frame() %>%
                        arrange(desc(Porcentaje))
                
                aux3$linea <- factor(aux3$linea,levels=unique(as.character(aux3$linea)))
                aux4<-aux3[!(aux3$Porcentaje<"0.02"),]
                aux5 <- aux4[match(target,aux4$linea),]
                aux5 <- aux5[complete.cases(aux5),]
                aux5$Porcentaje <- aux5$Porcentaje*100
                aux5 <- aux5[,c(1,3)]
                
                return(aux5)
        }
        
        if(ru==2){
                conteos<- apply(select(datos, -F_3), 2, function(c){
                        xtabs(datos$F_3 ~c,data=select(datos, -F_3))
                })
                
                aux<-data.frame()
                aux<-lapply(conteos,function(l){
                        n<-length(l)
                        if(n!=0){
                                rbind(aux,data.frame(linea=as.character(names(l)),
                                                     valor=as.numeric(l[1:n]),
                                                     stringsAsFactors = F)) 
                        }
                })
                
                aux3<-rbind_all(aux) %>%
                        filter(linea !="") %>%
                        filter(linea !=" ") %>%
                        group_by(linea) %>%
                        summarise(valor=sum(valor)) %>%
                        mutate(Porcentaje=valor/sum(datos$F_3)) %>%
                        data.frame() %>%
                        arrange(desc(Porcentaje))
                
                aux3$linea <- factor(aux3$linea,levels=unique(as.character(aux3$linea)))
                aux4<-aux3[!(aux3$Porcentaje<"0.02"),]
                aux5 <- aux4[match(target2,aux4$linea),]
                aux5 <- aux5[complete.cases(aux5),]
                aux5$Porcentaje <- aux5$Porcentaje*100
                aux5 <- aux5[,c(1,3)]
                
                return(aux5)
                
        }
        
        
        
}
















