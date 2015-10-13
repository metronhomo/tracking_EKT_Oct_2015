#Helpers-------

menu <- function(){
  fluidPage(
    column(
      3, wellPanel(
        helpText('Selecciona las variables sobre las que quieres filtrar.'),
        checkboxGroupInput(
          'filtroEdad', 
          label = h4('Edad'),
          choices = list(
            '25 - 35' = '25-35',
            '36 - 55' = '36-55'),
          selected = c('25-35', '36-55')),
        checkboxGroupInput(
          'filtroGen', 
          label = h4('Género'),
          choices = list(
            'Masculino' = 'M',
            'Femenino' = 'F'),
          selected = c('M', 'F')),
        checkboxGroupInput(
          'filtroNiv', 
          label = h4('Nivel'),
          choices = list(
            'C-' = 'C-',
            'D+' = 'D+',
            'D' = 'D'),
          selected = c(
            'C-',
            'D+',
            'D')),
        checkboxGroupInput(
          'filtroTipoCliente', 
          label = h4('Tipo de cliente'),
          choices = list(
            'Frecuente de Elektra' = 'El',
            'Frecuente de la competencia' = 'Comp'),
          selected = c('El', 'Comp')),
        submitButton(text = "Graficar"),
        wellPanel(
          helpText("Selecciona la variable con la que quieres cruzar."),
          selectInput(
            "filtrovar",
            label = "",
            choices = list(
              "Total de cuartos en el hogar",
              "Número de baños",
              "Regadera funcional",
              "Número de focos en la vivienda",
              "Tipo de piso",
              "Número de automóviles propios",
              "Estufa de gas o eléctrica",
              "Último año de estudios de jefe del hogar",
              "Total"
            )
          )
        )
      )
    )
  )}

#Con esta función obtenemos histogramas para las preguntas:

# ru = 1 cuando es una sola variable
# ru = 2 cuando sea respuesta multiple

cuenta<-function(datos,ru){
  
  if(ru==1){
    Top <- cbind(prop.table(table(datos))) %>%
      data.frame()
    colnames(Top) <- c("Porcentaje")
    Top <- cbind(linea= rownames(Top),Top) %>%
      arrange(desc(Porcentaje))
    
    Top$linea <- factor(Top$linea,levels=unique(as.character(Top$linea)))
    
    return(Top)
  }
  
  if(ru ==2){
    
    conteos<- apply(datos,2,function(c){
      xtabs(~c,data=datos)
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
      filter(linea !=" ") %>%
      group_by(linea) %>%
      summarise(valor=sum(valor)) %>%
      mutate(Porcentaje=valor/nrow(datos)) %>%
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
    xtabs(~c,data=mr)
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
  gr <- data.frame(do.call(cbind,lapply(data,hj)))
  gr <- cbind(atributo= rownames(gr),gr)
  aux<-gather(gr,"Tienda","Valor",-atributo)
  
  aux$Tienda <- apply(as.data.frame(aux$Tienda),2,function(r){gsub("P14_","",r)})
  
  return(aux)
}

# Ejemplo de como correr la función
# preg <-base_eq[,c(42:47)]
# p14(preg)

#evaluación de publicidad----
p15 <- function(don){
  conteos<- lapply(don,function(c){
    xtabs(~c,data=don)
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
  colnames(gh) <-c("Medio","Elektra","Coppel","Famsa","Bodega Aurrera","Walmart","Liverpool")
  aux<-gather(gh,"Tienda","Porcentaje",-Medio)
  return(aux)
}

#atributos de spots----
attri <- function(df){
  conteos<- t(sapply(df,function(c){
    xtabs(~c,data=df)
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
    Top <- cbind(prop.table((table(datos)))*100) %>%
      data.frame()
    colnames(Top) <- c("Porcentaje")
    Top <- cbind(linea= rownames(Top),Top)
    return(Top)
  }
  
  if(ru ==2){
    
    conteos<- apply(datos,2,function(c){
      xtabs(~c,data=datos)
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
      mutate(Porcentaje=valor/nrow(base_con)) %>%
      data.frame() %>%
      arrange(desc(Porcentaje))
    return(aux3)
  }    
}

#Ejemplo de como correr la función
#eval(base_evap[,131],1)

filtro<-function(df,variable,facet){
  if(facet=="Total"){
    df2 <- df %>%
      select(one_of(variable)) 
    names(df2)<-c("var_x")
    df2 <- df2 %>%
      group_by(var_x) %>%
      summarise(n=n()) %>%
      mutate(valor=(n/sum(n))*100) %>%
      filter(complete.cases(var_x)) %>%
      select(1,3)
  }else{
    df2 <- df %>%
      select(one_of(facet,variable)) 
    names(df2)<-c("var_y","var_x")
    df2 <- df2 %>%
      group_by(var_y,var_x) %>%
      summarise(n=n()) %>%
      mutate(valor=(n/sum(n))*100) %>%
      filter(complete.cases(var_y)) %>%
      select(1,2,4)
  }
  return(df2)
}

graf<-function(df,variable,facet){
  if(facet == "Total"){
    df2<-filtro(df,variable,facet)
    ggplot(df2,aes(x=var_x,y=valor)) + 
      geom_bar(stat="identity",fill="#2c3e50",colour="black") + 
      geom_text(aes(y=valor + 3,label=paste0(round(valor),'%')),
                colour='black',size=6) +
      theme(axis.text.x=element_text(angle=90,size=22),
            axis.text.y=element_text(size=22),
            panel.background=element_rect(fill='#C2D1E0'),
            panel.border = element_rect(colour = "#2c3e50", fill=NA, size=1),
            strip.background=element_rect(fill="#2c3e50")) +
      ylab("") + xlab("")
  }else{
    df2<-filtro(df,variable,facet)
    
    ggplot(df2,aes(x=var_x,y=valor)) + 
      geom_bar(stat="identity",fill="#2c3e50",colour="black") + 
      geom_text(aes(y=valor + 3,label=paste0(round(valor),"%")),
                colour='black',size=6) +
      facet_wrap(~var_y) +
      theme(axis.text.x=element_text(angle=90,size=22),
            axis.text.y=element_text(size=22),
            panel.background=element_rect(fill='#C2D1E0'),
            strip.background=element_rect(fill="#2c3e50"),
            panel.border = element_rect(colour = "#2c3e50", fill=NA, size=1),
            strip.text.x = element_text(colour = 'white', size = 22)) +
      ylab("") + xlab("")
  }
}

filtro<-function(df,edad,genero,plaza,nivel,tipo_cliente,tipo_producto){
  if(edad!="Todos"){
    a<-df %>%
      filter(Edad %in% edad)
  }else{
    a<-datos
  }
  
  if(genero!="Todos"){
    b <- a %>%
      filter(Genero %in% genero)
  }else{
    b<-a
  }
  
  if(plaza!="Todos"){
    c<- b %>%
      filter(Plaza %in% plaza)
  }else{
    c<-b
  }
  
  if(nivel!="Todos"){
    d<- c %>%
      filter(NIVEL %in% nivel)
  }else{
    d<-c
  }
  
  if(tipo_cliente!="Todos"){
    e<- d %>%
      filter(Tipo_Cliente %in% tipo_cliente)
  }else{
    e<-d
  }
  
  if(tipo_producto!="Todos"){
    f<-e %>%
      filter(P2_1 %in% tipo_producto ||
               P2_2 %in% tipo_producto ||
               P2_3 %in% tipo_producto ||
               P2_4 %in% tipo_producto ||
               P2_5 %in% tipo_producto)
  }else{
    f<-e
  }
  
  return(f)  
}

grafica_cuenta<-function(df){
  
  ggplot(df,aes(x=linea,y=Porcentaje)) + 
    geom_bar(stat="identity",fill="#2c3e50",colour="black") + 
    geom_text(aes(y=Porcentaje + .03 ,label=paste0(round(Porcentaje*100),"%")),
              colour='black',size=6) +
    theme(axis.text.x=element_text(angle=90,size=22),
          axis.text.y=element_text(size=22),
          panel.background=element_rect(fill='#C2D1E0'),
          strip.background=element_rect(fill="#2c3e50"),
          panel.border = element_rect(colour = "#2c3e50", fill=NA, size=1),
          strip.text.x = element_text(colour = 'white', size = 22)) +
    scale_y_continuous(labels=percent) +
    ylab("") + xlab("")
  
}