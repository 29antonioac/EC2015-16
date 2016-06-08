#################################################################################
################################## EJERCICIO 1 ##################################
#################################################################################

# Devuelve el número de extracciones que han sido necesarias para encontrar
# cuatro ases tomando muestras aleatorias de una baraja con o sin reemplazamiento
# (dependiente del parámetro Reemplazamiento). Como máximo, se toman Maximo
# muestras. Además, se imprime un mensaje si Mostrar es True.
# Esta función generaliza -con el objetivo además de conseguir un código más compacto-
# a las funciones CuatroAses y CuatroAses.Sin vistas en teoría.

CuatroAses <- function(Mostrar = F, Maximo = 1000, Reemplazamiento = T){
  # Si no hay reemplazamiento podemos probar, como mucho, con todas las cartas: 52.
  if(Reemplazamiento == F){
    Maximo <- min(52, Maximo)
  }
  
  # Tomamos `Maximo` muestras de entre los números 1 al 52
  resultado <- sample(52, Maximo, replace = Reemplazamiento)
  
  # Recupera los índices en los que hay ases; es decir, aquellos
  # cuyo valor módulo 13 es igual a 1. En caso de éxito, en 
  # extracciones[4] está el número de extracciones que han sido
  # necesarias para encontrar los cuatro ases.
  extracciones <- which(resultado %% 13 == 1)
  
  # Si ha encontrado los cuatro ases...
  if(length(extracciones) >= 4){
    # ... informamos del proceso si así se ha solicitado
    if(Mostrar){
      cat("He necesitado", extracciones[4],
          "extracciones para obtener cuatro ases\n")
    }
    
    # ... y devolvemos las extracciones que han sido necesarias,
    # así como todas las muestras extraídas.
    return(list(E = extracciones[4], R = resultado, Conseguido = T))
  }
  # Si no ha encontrado los cuatro ases...
  else{
    # ... devolvemos NA, así como todas las muestras
    # extraídas
    return(list(E = NA, R = resultado, Conseguido = F))
  }
}


#################################################################################
################################## EJERCICIO 2 ##################################
#################################################################################

# Devuelve un vector de tamaño n con los valores obtenidos de llamar
# n veces a la función foo.
# Esta función generaliza a los métodos DistriAses y DistriAses.Sin vistos
# en teoría. La función presupone que la función foo contabiliza, de alguna forma,
# el número de extracciones necesarias para conseguir un objetivo, con lo que
# foo debe tener los siguientes requisitos:
#     * Devuelve una lista, con el resultado en el elemento E.
#     * Se puede elegir el máximo de extracciones mediante el parámetro Maximo.
#     * Se puede elegir si se realiza reemplazamiento con el parámetro Reemplazamiento.

Distribucion <- function(n = 5, foo = CuatroAses, Max = 1000, Reemp = T){
  # Repetimos el experimento n veces con los parámetros indicados
  distrib <- replicate(n, foo(Maximo = Max, Reemplazamiento = Reemp)$E)
  
  # Devolvemos el vector resultante
  return(distrib)
}


#################################################################################
################################## EJERCICIO 3 ##################################
#################################################################################

# Devuelve una lista con las potencias del número x de exponente igual a los elementos
# del vector potencias. El elemeto i-ésimo de la lista es la potencia potencia[i]-ésima
# de x, y tiene como nombre la cadena Potencia `i`ª.

suma.pot.nombres  <- function(x, potencias=2:3)
{
  # Creamos una lista vacía de tamaño cero. La indexación posterior de los elementos
  # creará el tamaño que necesite. Si le pusiéramos aquí tamaño, la indexación posterior
  # empezaría a partir del último elemento no vacío; es decir, terminaríamos con una 
  # lista con el doble de tamaño deseado.
  lista <- vector("list")
  
  # Generamos los nombres de la forma Potencia iª, donde i se mueve en los elementos del
  # vector potencias
  nombres <- sapply(potencias, function(i){ return(paste("Potencia ", i, "ª", sep="")) })
  
  # Generamos las potencias
  resultado <- x^potencias
  
  # Indexamos la lista con los nombres generados e insertamos las potencias calculadas
  lista[nombres] <- resultado
  
  # Devolvemos la lista
  return(lista)
}