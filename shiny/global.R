library(shiny)
library(knitr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(scales)
library(readxl)

source("helpers.R")
options(digits=0)

df <- read.csv("./data/Base_Final.csv")

base_con<-readRDS("data/base_con.rds")
base_im<-readRDS("data/base_im.rds")
base_eq <- readRDS("data/base_eq.rds")
base_evap <- readRDS("data/base_evap.rds")

p2 <-base_con[,c(6:10,12:15,27:31)]            # Que tipo de producto compro dentro de los ultimos 12 meses 
acp3_top <- base_con[,c(6,12:15,27:31)]         # top of mind Marca
p5_guiado <-  base_con[,c(33:42,12:15,27:31)]   # share of mind" Marca
p4_Top <-  base_con[,c(22,12:15,27:31)]         # top de publicidad
p4_share <-  base_con[,c(22:32,12:15,27:31)]    #share de publicidad
p6_guiado <-  base_con[,c(43:52,12:15,27:31)]   # guiado de publicidad
p7 <-  base_con[,c(53:62,12:15,27:31)]          # cual de estas marcas considera la proxima vez que compre un producto
p8 <-  base_con[,c(63:72,12:15,27:31)]          # en cuales tiendas ha comprado ultimamente
p9 <-  base_con[,c(73:82,12:15,27:31)]          # en cuales tiendas ha comprado usted un producto de electronica,linea blanca,computo
p10 <-  base_con[,c(83:92,12:15,27:31)]         # en cuales tiendas a comprado alguna vez un producto de elctronica
p11 <-  base_con[,c(93,12:15,27:31)]            # en cuales tiendas es en la que compra usted con mayor frecuancia este tipo de productos
p11a <-  base_con[,c(94,12:15,27:31)]           # si algun momento decidiera dejar de comprar en su tienda más frecuente, en dondé compraría este tipo de productos?
p14 <- base_eq[,c(42:47,12:15,27:31)]           # que probabilidad hay que usted compre productos de electronica,linea blanca, muebles en estas tiendas en el futuro
p15 <- base_evap[,c(6:82,12:15,27:31)]          #  anterior me dijo que recordaba haber visto publicidad, digame para cada una de ellas los lugares en los que recuerda haber visto, leido o escuchado.
p17 <- base_evap[,c(114,12:15,27:31)]           # Basandose en lo que recuerda de este comercial ¿Como lo hace sentir con respecto a Elektra
p18 <- base_evap[,c(115:128,12:15,27:31)]       # Ahora voy a leer algunas afirmaciones sobre este comercial. que tan de acuerdo o en desacuerdo está con cada una de ellas.
p20 <- base_evap[,c(131,12:15,27:31)]           # Basandose en lo que recuerda de este comercial ¿Como lo hace sentir con respecto a Coppel.
p21 <- base_evap[,c(132:145,12:15,27:31)]       # Ahora voy a leer algunas afirmaciones sobre este comercial. que tan de acuerdo o en desacuerdo está con cada una de ellas.

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

rm(base_con,base_im,base_evap,base_eq)