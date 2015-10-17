


# function "cuenta" es para preguntas de p2 hasta p11a
# function "p14" es para la pregunta p14      # no hay que introducir ningun dato llamandola automaticamente hace la tabla
# function "funp15" es para la pregunta p15   # no hay que introducir ningun dato llamandola automaticamente hace la tabla
# function "eval" es para preguntas p17 y p20
# function "attri" es para preguntas p18 y p21

ru <- 1 # respuesta unica
ru <- 2 # respuesta multiple


p2 <-  base_con[,6: 10]                    # ru=2     Que tipo de producto compro dentro de los ultimos 12 meses 
p3_top <- cuenta(base_con[,11],1)          # ru=1     top of mind Marca   
p3_share <- base_con[,11:21]               # ru=2     share of mind" Marca
p5_guiado <- base_con[,33:42]    # ru=2     guiado de marca " Marca
p4_Top <-  base_con[,22]         # ru=1     top de publicidad
p4_share <-  base_con[,22:32]    # ru=2     share de publicidad
p6_guiado <-  base_con[,43:52]   # ru=2     guiado de publicidad
p7 <-  base_con[,53:62]          # ru=2  cual de estas marcas considera la proxima vez que compre un producto
p8 <-  base_con[,63:72]          # ru=2  en cuales tiendas ha comprado ultimamente
p9 <-  base_con[,73:82]          # ru=2  en cuales tiendas ha comprado usted un producto de electronica,linea blanca,computo
p10 <-  base_con[,83:92]         # ru=2  en cuales tiendas a comprado alguna vez un producto de elctronica
p11 <-  base_con[,93]            # ru=1  en cuales tiendas es en la que compra usted con mayor frecuancia este tipo de productos
p11a <-  base_con[,94]           # ru=1  si algun momento decidiera dejar de comprar en su tienda más frecuente, en dondé compraría este tipo de productos?
p14 <- base_eq[,42:47]           # que probabilidad hay que usted compre productos de electronica,linea blanca, muebles en estas tiendas en el futuro
p15 <- base_evap[,6:82]          # anterior me dijo que recordaba haber visto publicidad, digame para cada una de ellas los lugares en los que recuerda haber visto, leido o escuchado.
p17 <- base_evap[,114]           # ru=1  Basandose en lo que recuerda de este comercial ¿Como lo hace sentir con respecto a Elektra
p18 <- base_evap[,115:128]       # Ahora voy a leer algunas afirmaciones sobre este comercial. que tan de acuerdo o en desacuerdo está con cada una de ellas.
p20 <- base_evap[,131]           # ru=1  Basandose en lo que recuerda de este comercial ¿Como lo hace sentir con respecto a Coppel.
p21 <- base_evap[,132:145]       # Ahora voy a leer algunas afirmaciones sobre este comercial. que tan de acuerdo o en desacuerdo está con cada una de ellas.


