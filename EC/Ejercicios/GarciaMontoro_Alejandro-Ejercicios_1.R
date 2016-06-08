# Media de un vector x. Se comporta como mean(x, na.rm=T)
media <- function(x=NA){
    # Calculamos la longitud del vector inicial
    long_previa <- length(x)
    
    # Eliminamos los NA
    x<-x[!is.na(x)]
    
    # Calculamos la longitud del nuevo vector
    long_actual <- length(x)
    
    # Generamos una lista con la longitud previa, la diferencia entre 
    # las longitudes antes y después de eliminar los NA y la media
    solucion <- list(LongitudPrevia = long_previa,
                     ElementosEliminados = long_previa - long_actual,
                     Media = sum(x)/length(x))
    
    # Devolvemos la lista
    return(solucion)
}

# Varianza de un vector x.
varianza <- function(x=NA){
    # Eliminamos los NA
    x <- x[!is.na(x)]
    
    # Calculamos la media del vector x y nos quedamos sólo con el valor
    # Media de la lista devuelta
    y <- media(x)$Media
    
    # Calculamos la varianza
    varianza <- media((x-y)^2)$Media
    
    # Devolvemos la varianza
    return(varianza)
}

# Devuelve el coeficiente de asimetría de Pearson
Pearson.asimetria <- function(x=NA){
    # Eliminamos los NA
    x <- x[!is.na(x)]
    
    # Calculamos la media del vector x y nos quedamos sólo con el valor
    # Media de la lista devuelta
    y <- media(x)$Media
    
    # Calculamos mu_3
    mu_3 <- media((x - y) ** 3)$Media
    
    # Calculamos sigma_3
    sigma_3 <- sqrt(varianza(x)) ** 3
    
    # Devolvemos el coeficiente de asimetría de Pearson
    return(mu_3 / sigma_3)
}

# Devuelve el coeficiente de curtosis de Pearson
Pearson.curtosis <- function(x=NA){
    # Eliminamos los NA
    x <- x[!is.na(x)]
    
    # Calculamos la media del vector x y nos quedamos sólo con el valor
    # Media de la lista devuelta
    y <- media(x)$Media
    
    # Calculamos mu_4
    mu_4 <- media((x - y) ** 4)$Media
    
    # Calculamos sigma_4
    sigma_4 <- varianza(x) ** 2
    
    # Devolvemos el coeficiente de curtosis de Pearson
    return(mu_4 / sigma_4)
}
