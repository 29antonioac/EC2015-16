## ------------------------------------------------------------------------
movimiento <- function(maxIter = 10, mostrar = F){
  # Definimos la posición inicial
  pos <- c(0,0)
  
  # Inicializamos la matriz que contendrá, en la fila i-ésima, la posición
  # de la partícula en el instante i-ésimo
  camino <- matrix(pos, ncol=2, byrow=T)
  
  # Inicializamos el contador de iteraciones
  iteraciones <- 0
  
  while( !(100 %in% abs(pos)) && iteraciones < maxIter){
    # Simulamos las dos monedas: 1 es cara; 2 es cruz:
    monedas <- sample(1:2, 2, replace = T)
    
    # Determinamos el movimiento: si la primera moneda es cara (1), nos
    # movemos una unidad a la derecha; es decir, sumamos uno en el eje X;
    # si es cruz (2), a la izquierda: restamos uno en el eje X. Hacemos
    # lo mismo con la segunda moneda, que representa el eje Y.
    movimiento <- ifelse(monedas == 1, 1, -1)
    
    # Actualizamos la posición.
    pos <- pos + movimiento
    
    # Actualizamos la progresión
    camino <- rbind(camino, pos)
    
    # Actualizamos las iteraciones
    iteraciones <- iteraciones + 1
  }
  
  if(100 %in% abs(pos)){
    if(mostrar){
      cat("He llegado al límite.\n")
    }
    exito <- T
  }
  else{
    if(mostrar){
      cat("No he terminado de moverme.\n")
    }
    exito <- F
  }
  
  return(list(Progresion = camino, Pasos = iteraciones, Exito = exito))
}

## ------------------------------------------------------------------------
res <- movimiento(10000, mostrar = T)

## ----include=FALSE-------------------------------------------------------
mensaje <- ifelse(res$Exito, "sí", "no")

## ------------------------------------------------------------------------
# Gráfico del progreso
plot(res$Progresion, type="l", col="steelblue", asp=1,
     xlim=c(-100,100), xlab="Eje X",
     ylim=c(-100,100), ylab="Eje Y",
     main="Progresión del punto")

# Añadimos los puntos iniciales y final
punto.inicial <- c(0,0)
punto.final <- t(res$Progresion[nrow(res$Progresion),])
points(rbind(punto.inicial, punto.final), pch=20, col="blue")

# # Añadimos los bordes del recinto cuadrado
# abline(h=c(-100,100), v=c(-100,100), col="coral")

# Añadimos los bordes del recinto cuadrado con polygon
polygon(c(-100,100,100,-100), c(100,100,-100,-100), border = "coral")

## ------------------------------------------------------------------------
repeticion <- function(n = 1000, maxPasos = Inf){
  return(replicate(n, movimiento(maxPasos)$Pasos))
}

## ------------------------------------------------------------------------
# Definimos el número de repeticiones
num.rep <- 100

# Tomamos nota del reloj en el instante actual
inicio <- Sys.time()

# Hacemos las 100 repeticiones solicitadas
pasos <- repeticion(num.rep)

# Tomamos nota del reloj en el instante actual
final <- Sys.time()

# Calculamos el tiempo total
tiempo.total <- final - inicio

# Nos aseguramos de que estamos trabajando con segundos
units(tiempo.total) <- "secs"

# Calculamos medias de pasos y de tiempos
pasos.media <- mean(pasos)
tiempo.media <- (tiempo.total)/num.rep

