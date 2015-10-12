library(dplyr)
library(readxl)

#Lectura de datos-----

data <- read_excel("Base_Final.xlsx",sheet=1)
data <- data[-1,]

base_con <- data %>%
        select(1,8,12:14,27:135)

saveRDS(base_con,"data/base_con.rds")

base_im <- data %>%
        select(1,8,12:14,136:273)

saveRDS(base_im,"data/base_im.rds")

base_eq <- data %>%
        select(1,8,12:14,274:315)

saveRDS(base_eq,"data/base_eq.rds")

base_evap <- data %>%
        select(1,8,12:14,316:455)

saveRDS(base_evap,"data/base_evap.rds")

#Datos----
base_con<-readRDS("data/base_con.rds")
base_im<-readRDS("data/base_im.rds")
base_eq <- readRDS("data/base_eq.rds")
base_evap <- readRDS("data/base_evap.rds")

# ru = 1 cuando es una sola variable
# ru = 2 cuando sea respuesta multiple
cuenta<-function(datos,ru){
        
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
                mutate(p_valor=valor/nrow(base_con)) %>%
                data.frame() %>%
                arrange(desc(p_valor))
        return(aux3)
        }    
}
#Ejemplo de la función-----
cuenta(base_con[,95:104],2)

# imagen de Marca
gd <- function (mr){
  conteos<- apply(mr,2,function(c){
    xtabs(~c,data=mr)
  })
  
  aux<-data.frame()
  aux2<-lapply(conteos,function(l){
    n<-length(l)
    if(n!=0)
    rbind(aux,data.frame(linea=names(l),valor=as.numeric(l[1:n])))     
  })
  
  aux3<-Reduce('rbind',aux2) %>%
    filter(linea !=" ") %>%
    group_by(linea) %>%
    summarise(valor=sum(valor)) %>%
    mutate(p_valor=valor/nrow(base_im)) %>%
    data.frame() %>%
    arrange(desc(p_valor))

return(aux3)
}

frases <- list(a1 = base_im[,c(6:9)],
                b2 = base_im[,c(12:17)],
                c3 = base_im[,c(18:23)],
                d4 = base_im[,c(24:29)],
                e5 = base_im[,c(30:35)],
                f6 = base_im[,c(36:40)],
                g7 = base_im[,c(42:46)],
                h8 = base_im[,c(48:53)],
                i9 = base_im[,c(54:59)],
                j10 = base_im[,c(60:65)],
                k11 = base_im[,c(66:71)],
                l12 = base_im[,c(72:77)],
                m13 = base_im[,c(78:83)],
                n14 = base_im[,c(84:89)],
                nn15 = base_im[,c(90:95)],
                o16 = base_im[,c(96:100)],
                p17 = base_im[,c(102:106)],
                q18 = base_im[,c(108:113)],
                r19 = base_im[,c(114:119)],
                s20 = base_im[,c(120:125)],
                s21 = base_im[,c(126:131)],
                s22 = base_im[,c(132:137)],
                s23 = base_im[,c(138:143)])

gy <- lapply(frases,gd)

#Equity P14

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

#Ejemplo de como correr p14
#preg <-base_eq[,c(42:47)]
#p14(preg)

#evaluación de publicidad

#Evaluacion publicitaria lugar 

p15 <- function(don){
  conteos<- lapply(don,function(c){
    xtabs(~c,data=elektra)
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
    mutate(p_valor=valor/nrow(base_im)) %>%
    data.frame() %>%
    arrange(desc(p_valor))
  
  gy <- data.frame(do.call(cbind,lapply(don,p15)))
  gh <- gy[,c(1,3,6,9,12,15,18)]
  colnames(gh) <-c("Medio","Elektra","Coppel","Famsa","Bodega Aurrera","Walmart","Liverpool")
  
  aux <- gather(gh,"Tienda","Valor",-Medio) 
  
  return(aux)
}

p15(marcas)
#atributos de spots

atri <- (base_evap[,c(115:128)])

attri <- function(base){

conteos<- t(sapply(base,function(c){
        xtabs(~c,data=base)
}))

conteos <- data.frame(conteos[,2:4])
conteos$total <- apply(conteos,1,sum)
conteos$Concuerda_completamente <- conteos[,1]/conteos[,4]
conteos$Concuerda_en_parte <- conteos[,2]/conteos[,4]
conteos$Concuerda_en_nada <- conteos[,3]/conteos[,4]
conteos <- cbind(atributo = rownames(conteos), conteos)

atributos <- conteos[,c(1,6)]

return(atributos)

}

attri(base_evap[,c(115:128)])       

# evaluacion de comerciales

eval<-function(datos,ru){
        
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
                        mutate(p_valor=valor/nrow(base_con)) %>%
                        data.frame() %>%
                        arrange(desc(p_valor))
                return(aux3)
        }    
}

eval(base_evap[,131],1)
