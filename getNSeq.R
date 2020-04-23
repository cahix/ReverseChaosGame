#Diese Funktionen sind zum Entschlüsseln von Texten aus
#generischen Alphabeten.
#die entschlüsselnde Funktion heißt "getNSeq"

library(reshape2)
library(kaos)


li <- function(i,max_i) {
  #liefert vorgänger einer zahl in einer reihe von i bis max_i
  if (i == 1) {return(max_i)}
  else        {return(i-1)  }
}


ni <- function(i,max_i) {
  #liefert nachfolger einer zahl in einer reihe  von i bis max_i
  if (i == max_i) {return(1)  }
  else            {return(i+1)}
}


getAngle <- function(x1, y1, x2, y2, tol = 1e-6) {
  #innerer Winkel zwischen zwei Vektoren  
  a <- c(x1,y1)
  b <- c(x2,y2)
  cost <- as.numeric((a %*% b) / (sqrt(a %*% a) * sqrt(b %*% b)))
  if ((x1 == 0 && y1 == 0 || x2 == 0 && y2 == 0) && is.nan(cost)) return(0)   #Sonderfall. Tritt nur bei n=4 auf, sobald P(0,0) getroffen wird.
  if (abs(cost - 1) < tol) return(0)                                          #Korrektur bei Rechenfehlern
  else if (abs(cost + 1) < tol) return(180)                                   #Korrektur bei Rechenfehlern
  else return(acos(cost))
}



inAreaBetween <- function(px,py,x1,y1,x2,y2){
  #prüfen, ob P zwischen V1 und V2 liegt
  angleArea = getAngle(x1,y1,x2,y2)
  return (getAngle(x1,y1,px,py) < angleArea && getAngle(x2,y2,px,py) <  angleArea)
}


getCurrentPolygon <- function(x,y,alphabet,borders) {
  #den Koordinaten einen Buchstaben zuordnen
  #borders sind die Mittelpunkte zwischen den Eckpunkten (berechnen mit getDividingVectors)
  len = length(alphabet)
    for (i in 1:len) {
      if(inAreaBetween(x,y,borders[[1]][i], borders[[2]][i], borders[[1]][ni(i,len)], borders[[2]][ni(i,len)]))  
        return(alphabet[i])
  }
}


getPolygonCorners <- function(n){
  #Eckpunkte eines Polygons der Dimension n berechnen
  x = vector("double", n)
  y = vector("double", n)
  if(n == 4) {
    x[1] =  1; y[1] = -1
    x[2] = -1; y[2] = -1
    x[3] = -1; y[3] =  1
    x[4] =  1; y[4] =  1
  }
  else {
    for (i in 1:n){
      x[i] = sinpi((2*i+1)/n)
      y[i] = cospi((2*i+1)/n)
    } 
  }
  res <- list("x_values" = x, "y_values" = y)
  return(res)
}

getDividingVectors <- function(corners){
  #Mittelpunkte zwischen den Eckpunkten berechnen (corners berechnen mit getPolygonCorners)
  len <- length(corners[[1]])
  corners_x_values = corners[[1]]
  corners_y_values = corners[[2]]
  borders_x <- c()
  borders_y <- c()
  for (i in 1:len) {
    borders_x[i] = (corners_x_values[i] + corners_x_values[li(i,len)])/2
    borders_y[i] = (corners_y_values[i] + corners_y_values[li(i,len)])/2
  }
  return(list(x_values = borders_x, y_values = borders_y))
}



stretchPolygon <- function(x,y,alphabet,corners,current_polygon,stretch) {
  #Koordinaten Abbilden gemäß einer Streckung des Polygons
  alen = length(alphabet)
  Z <- data.frame(x=0, y=0)                   #Streckzentrum
  for (i in 1:alen) {
    if(current_polygon == alphabet[i]) {      #Streckzentrum zuweisen. Dafür muss der passende Eckpunkt gefunden werden
      Z$x = corners[[1]][i]
      Z$y = corners[[2]][i]
    }
  }
  dx=x-Z$x                                      #Abstand zwischen Zentrum und unserem Punkt
  dy=y-Z$y
  stretched_x = Z$x + (stretch)*dx
  stretched_y = Z$y + (stretch)*dy
  
  return(data.frame(x=stretched_x, y=stretched_y))
}


getNSeq <- function(x,y,alphabet,len){
  #Funktion zum Entschlüssen
  res <- c()
  corners = getPolygonCorners(length(alphabet))                                             #Eckpunkte berechnen
  borders = getDividingVectors(corners)                                                     #Trennende Vektoren berechnen
  alen = length(alphabet)
  stretch = 1/((sinpi(1/alen)/(sinpi(1/alen) + sinpi(1/alen +2 * (floor(alen/4)/alen)))))   #Streckfaktor
  letter = getCurrentPolygon(x,y,alphabet,borders)                                          #Der erste Buchstabe wird außerhalb der Schleife berechnet, um am ende eine Streckung zu sparen
  res <- c(letter,res)
  if(len > 1) {
    for (i in 2:len) {
      strechedPolygon = stretchPolygon(x,y,alphabet,corners,letter,stretch)                 #Strecken
      x = strechedPolygon$x
      y = strechedPolygon$y
      letter = getCurrentPolygon(x,y,alphabet,borders)                                      #Buchstaben zuweisen
      if(is.null(letter)) {return(res)}                                                     #Sonderfall, falls die Koordinaten das Polygon verlassen haben (z.B. wegen fehlender Präzision)
      res <- c(letter,res)
    }
  }
  return(res)
}





