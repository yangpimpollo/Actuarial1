##############################################################
#
#     2024.10.08
#   Seguros de Una Vida, Calculo Numerico
#
##############################################################


###----- importando datos --------------------------------

  library(csv)
  data <-read.csv("tabSPP.csv",header=T,sep=";",dec=",")
  head(data)   # viendo los datos
  str(data)

###----- creando funcion qx ------------------------------

  qx <- function(type, x) {
    return(data[[type]][data$age == x])
  }
  #qx("HI",2)   # probando la funcion

###----- creando funcion px ------------------------------

  px <- function(type, x) { return(1 - qx(type,x)) }
  #px("HI",2)   # probando la funcion

###----- creando funcion kpx ------------------------------

  kpx <- function(type, k, x) {
    if(k<1) return(1)   #  si k vale 0 retorna 1
    P <- 1
    for (i in 0:(k-1)) { P <- P*px(type,x+i) }
    return(P) 
  }

###----- creando funcion k1qx ------------------------------

  k1qx <- function(type, k, x) { return(kpx(type,k,x)*qx(type,x+k)) }


###----- creando funcion para whole life ------------------------------

  Ax_apv <- function(type, w, x, i) {
    Ax <- 0
    v <- 1 + i
    for (i in 0:(w-x-1)){
      Ax <- Ax + (v**(-(i+1))*k1qx(type,i,x))
    }
    return(Ax)
  }

###----- creando funcion para n-year term increasing annually ---------

  IA1xn_apv <- function(type, n, x, i) {
    IA1xn <- 0
    v <- 1 + i
    for (i in 0:(n-1)){
      IA1xn <- IA1xn + (i+1)*(v**(-(i+1))*k1qx(type,i,x))
    }
    return(IA1xn)
  }

##############################################################
##############################################################
#    EJECUCION

  Ax_apv("HS",110,10,0.06)
  Ax_apv("MS",110,10,0.06)
  Ax_apv("HI",110,10,0.06)
  Ax_apv("MI",110,10,0.06)
  
  IA1xn_apv("HS",5,10,0.06)
  IA1xn_apv("MS",5,10,0.06)
  IA1xn_apv("HI",5,10,0.06)
  IA1xn_apv("MI",5,10,0.06)

#############
