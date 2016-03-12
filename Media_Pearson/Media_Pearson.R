## ----media---------------------------------------------------------------
media <- function(x=NA){
  x <- x[!is.na(x)]
  
  media <- sum(x) / length(x)
  
  return(media)
}

## ----media_MOD-----------------------------------------------------------
media.MOD <- function(x=NA){
  tamanio.antes <- length(x)
  
  x <- x[!is.na(x)]
  
  num.quitados <- tamanio.antes - length(x)
  
  media <- sum(x) / length(x)
  
  return(list("Media" = media, "Tamanio" = tamanio.antes, "Quitados" = num.quitados))
}


## ----PruebaMedia---------------------------------------------------------
entrada <- c(1,2,3,NA,4,NA)

print(media.MOD(entrada))

## ----momentoCentrado-----------------------------------------------------
momento <- function(x = NA, k = 1) {
  x <- x[!is.na(x)]
  return( media((x - media(x)) ^ k) )
}


## ----Asimetria-----------------------------------------------------------
coeficientes.asimetria.curtosis <- function(x = NA){
  mu_3 <- momento(x,3)
  sigma_2 <- momento(x,2)
  sigma_3 <- sigma_2 ^ (3/2)
  
  mu_4 <- momento(x,4)
  sigma_4 <- sigma_2 ^ 2
  
  asimetria <- mu_3 / sigma_3
  curtosis <- mu_4 / sigma_4
  
  return(list("Asimetria" = asimetria, "Curtosis" = curtosis))
}

## ----pruebaCoeficientes--------------------------------------------------
x <- c(1,2,3,NA,4,NA)

print(coeficientes.asimetria.curtosis(x))

