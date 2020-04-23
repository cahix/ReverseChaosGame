#Diese Funktionen sind zum paketweisen Verschlüsseln
#benötigt "getNSeq"

library(kaos)

splittedCgr <- function(data,packagesize,alphabet) {
  #Eine Seqzenz in der angegebenen Paketgröße abschnittweise Verschlüsseln und als Liste zurückgeben
  splittedData <-  split(data, ceiling(seq_along(data)/packagesize))        #Sequenz aufteilen
  x_values <- c()
  y_values <- c()
  for(d in splittedData){                                                   #mehrere CGR-Objekte erstellen
    unlistedData <- unlist(d, use.names=FALSE)                              
    dcgr <- cgr(d,seq.base = alphabet)
    x_values <- c(x_values,dcgr$x[length(dcgr$x)])                          #x- und y- Werte der letzten Koordinaten speichern
    y_values <- c(y_values,dcgr$y[length(dcgr$y)])
  }
  res <- list("last_x_values" = x_values, "last_y_values" = y_values, "packagesize" = packagesize, "last_packagesize" = length(splittedData[length(splittedData)][[1]]))
  return(res)
}


getLongNSeq <- function(data,alphabet) {
  #Data erhält ein splittedCgr, dieses wird formatiert und die ursprüngliche Sequenz wird entschlüsselt
  x_values         <- unlist(data[1],use.names = FALSE)
  y_values         <- unlist(data[2],use.names = FALSE)
  packagesize      <- unlist(data[3],use.names = FALSE)
  last_packagesize <- unlist(data[4],use.names = FALSE)
  length           <- length(x_values)
  res <- c()
  for(i in 1:(length-1)){                                                     #pakete entschlüsseln
    seq = getNSeq(x_values[i],y_values[i],alphabet,packagesize)
    res <- c(res,seq)
  }
  seq = getNSeq(x_values[length],y_values[length],alphabet,last_packagesize)  #letztes Paket hat ggf. andere Größe
  res <- c(res,seq)
  return(res)
}

  
