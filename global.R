library(shiny)
library(knitr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(scales)
library(readxl)

source("helpers.R")
options(digits=0)

base_con<-readRDS("data/base_con.rds")
base_im<-readRDS("data/base_im.rds")
base_eq <- readRDS("data/base_eq.rds")
base_evap <- readRDS("data/base_evap.rds")

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
