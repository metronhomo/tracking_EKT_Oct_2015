options("encoding" = "UTF-8")

library(shiny)
library(knitr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(scales)
library(readxl)
library (wordcloud)
library(SnowballC)
library(tm)
library(grid)

df <- read.csv("./data/Base_Final.csv")

options(digits=3)

base_con <- readRDS("data/base_con.rds")
base_im <- readRDS("data/base_im.rds")
base_eq <- readRDS("data/base_eq.rds")
base_evap <- readRDS("data/base_evap.rds")

source("helpers.R")

p2 <-base_con[,c(3:12, 117)]                         # Que tipo de producto compro dentro de los ultimos 12 meses 
p3_top <- base_con[,c(3:12,13, 117)]         # top of mind Marca
p3_share <- base_con[, c(3:23, 117)]               # ru=2     share of mind" Marca
p4_Top <-  base_con[,c(3:12, 24, 117)]         # top de publicidad
p4_share <-  base_con[,c(3:12, 25:34, 117)]    #share de publicidad
p5_guiado <-  base_con[,c(3:12, 35:44, 117)]   # share of mind" Marca
p6_guiado <-  base_con[,c(3:12, 45:54, 117)]   # guiado de publicidad
p7 <-  base_con[,c(3:12, 55:64, 117)]          # cual de estas marcas considera la proxima vez que compre un producto
p8 <-  base_con[,c(3:12, 65:74, 117)]          # en cuales tiendas ha comprado ultimamente
p9 <-  base_con[,c(3:12, 75:84, 117)]          # en cuales tiendas ha comprado usted un producto de electronica,linea blanca,computo
p10 <-  base_con[,c(3:12, 85:94, 117)]         # en cuales tiendas a comprado alguna vez un producto de elctronica
p11 <-  base_con[,c(3:12, 95, 117)]            # en cuales tiendas es en la que compra usted con mayor frecuancia este tipo de productos
p11a <-  base_con[,c(3:12, 96, 117)]           # si algun momento decidiera dejar de comprar en su tienda más frecuente, en dondé compraría este tipo de productos?
p14 <- base_eq[,c(3:7, 49:55)]           # que probabilidad hay que usted compre productos de electronica,linea blanca, muebles en estas tiendas en el futuro
p15 <- base_evap[,c(3:7, 13:118, 153)]          #  anterior me dijo que recordaba haber visto publicidad, digame para cada una de ellas los lugares en los que recuerda haber visto, leido o escuchado.
p17 <- base_evap[,c(3:12, 121, 153)]           # Basandose en lo que recuerda de este comercial ¿Como lo hace sentir con respecto a Elektra
p18 <- base_evap[,c(3:7, 122:135, 153)]       # Ahora voy a leer algunas afirmaciones sobre este comercial. que tan de acuerdo o en desacuerdo está con cada una de ellas.
p20 <- base_evap[,c(3:12, 138, 153)]           # Basandose en lo que recuerda de este comercial ¿Como lo hace sentir con respecto a Coppel.
p21 <- base_evap[,c(3:7, 139:152, 153)]       # Ahora voy a leer algunas afirmaciones sobre este comercial. que tan de acuerdo o en desacuerdo está con cada una de ellas.

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

marcas <- list(
  elektra <- base_evap[,c(6:17)],
  Coppel <- base_evap[,c(19:30)],
  Famsa <- base_evap[,c(32:43)],
  Bodega <- base_evap[,c(45:56)],
  Walmart <- base_evap[,c(58:69)],
  Liver <- base_evap[,c(71:82)])

atri <- (base_evap[,c(115:128)])

P16_1 <- readRDS("data/P16_1.RDS")
P16A_1 <- readRDS("data/P16A_1.RDS")
P19_1 <- readRDS("data/P19_1.RDS")
P19A_1 <- readRDS("data/P19A_1.RDS")

palabras_prohibidas=c('que','y','na','casi','lleva','llevas','elektraz','traia','quehacer','elktra','asta','paratoda','fua','ello','ningun','trae','tenias','hay','en','vi','vio','obtener','s','compra','con','la','un','una','unos','uno','etc','era','eran','desde','cada','mismo','hacia','hasta','para','por','cual','asi','así',
                      'algo','tu','tus','sale','van','daban','esta','estos','esa','esas','llega','recuerdo','veia','compren','paa','sobra','dejes',
                      'comprar','ahí','ahi','días','pudes','otras','días','muy','dos','dio','ninguna','nada','las','como','decía','ellos','no','dejemos','de','rosa','rojo','mejoraba','grande','grandes','toda','todo','esto','tenia','solo','podia','podía','puedo','pueden','puedes','aquí','dar','solamente','compres','tenga','compraras',
                      'comprando','atendías','entrando', 'tenía','ofrecen','sacar','viendo','ademas','acuerdo','bonita','escucho','ver','escuchando','hace','hacen','sobre','a','hay','desde','tienen','todos','varias','bien','vende','dando','dandose','darles','darlo','lla','terminas',
                      'usar','vender','probar','mas','más','siempre','que','los','las','ofrecen','sin','ir','más','estar','tanto','es','esa','via','cuentan','estan','saliendo','unas','estaban','tipos','ademas','pasaban','decia','despues','después','podemos','estaba','pasando','tinen',
                      'alli','hacerte','poner','quieras','haya','dicho','vayas','quede','pensar','pidamos','tal','venden','nadie','misma','demas','dicho','poner','formando','compras','diciendo','hacer','pedir','verce','hacerquen','cuestion','agarra','plansirve','sacan','amarillodistintos',
                      'decian','una','sus','del','para','son','muchas','muchos','mucha','mucho','dice','dicen','decian','decían','dijeron','salen','decir','dan','tanta','entra','ven','habia','pasan','llevar','abre','salia','salian','azul','azules','color','colores','sonaba','mitad','temporada',
                      'venta','menos','gran','elelektra','elektra','mi','está','obetener','obtener','haciendo','vio','aqui','otro','cuento','todas','cuanto','tiene','dentro','entre','llevas','donde','usando','uso','final','mostras','opción','encontrar','encuentras','terminas','convencer','convenser',
                      'coppel','buena','bueno','buenos','buenas','union','blancas','ofrecen','busques','podria','podriamos','salimos','deberian','cambiarse','ofreciendote','aunque','mantenerme','enganchan','tamaña','estoy','usted','aserte','tengan','manejan','sucia','llame','hacernos',
                      'les','coopel','tenian','entrar','visto','pasaron','ella','dia','buen','viste','tipo','igual','veo','mano','según','veian','pues','ahora','tambien','algunos','este','vacio','tienes','llevarte',
                      'estes','llama','pasar','decirle','mis','darnos','tan','vistas','querer','ser','saber','obtien','sepan','junto','lugar','tengas','seres','vean','darme','tener','seria','pase','otras','atraer','durante','hagan','hablan','comun','vea',
                      'ellas','llegar','vayan','rato','cambiar','trato','tengamos','tratar','tran','invitan','quieren','nos','vaya','hacer','irlas','cas','creo','acerte','puede','mostrarle','lados','nuestras','nuestro','ofrecer','copel','quienes','atraes',
                      'porque','formas','veas','resiste','tomar','vez','debes', 'afor','supon','andes','podias','estas','saque','nosotros','alguna','cualquier','compre','dichos','llamar','busco','hacerle','adquirir','conozcan','cambia','cambien','armar',
                      'quiere','compras','obtienen','quiza','podria','comer','infla','daa','mejro','saquemos','eso','darle','tratan')

load("./data/ws dimensiones.RData")
load("./data/ws equity.RData")
# names(dfequity)[6] <- "valor por su dinero"
names(dfequity)[1:6] <- c("Familiarizado con esta marca",
  "La marca tiene algo especial",
  "Va con mi personalidad y me entiende",
  "Mucha gente la usa",
  "Tiene alta calidad",
  "Valor por su dinero")
#   "Score equity",
#   "Tienda",
#   "Edad",
#   "Tipo de Cliente",
#   "Tipo de producto",
#   "Género",
#   "Nivel",
#   "Ponderador")

# rm(base_con,base_im,base_evap,base_eq)

# http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
 # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}