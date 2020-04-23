#Hier wurden die Funktionen aus getNSeq mit precision numbers versehen


library(Rmpfr)


                                     prec <- 400 #GLOBALER WERT FÜR DIE ANZAHL AN PRÄZISEN BITS

                                     
                                     
li <- function(i,max_i) {
  #liefert vorgänger einer Zahl in einer reihe
  if (i == 1) {return(max_i)}
  else        {return(i-1)  }
}

ni <- function(i,max_i) {
  #liefert nachfolger einer Zahl in einer reihe
  if (i == max_i) {return(1)    }
  else            {return(i+1)  }
}



prec.getAngle <- function(x1, y1, x2, y2, tol = 1e-6) {
  #innerer Winkel zwischen zwei Vektoren
  a <- c(x1,y1)
  b <- c(x2,y2)
  cost <- ((a %*% b) / (sqrt(a %*% a) * sqrt(b %*% b))) #as.numeric
  if (abs(cost - 1) < tol) return(0) 
  else if (abs(cost + 1) < tol) return(180)
  else return(mpfr(acos(cost),prec))

}


prec.inAreaBetween <- function(px,py,x1,y1,x2,y2){
  #prüfen, ob P zwischen V1 und V2 liegt
  angleArea = prec.getAngle(x1,y1,x2,y2)
  return (prec.getAngle(x1,y1,px,py) < angleArea && prec.getAngle(x2,y2,px,py) <  angleArea)
}



prec.getDividingVectors <- function(corners){
  #trennende Vektoren zwischen den Eckpunkten berechnen
  len <- length(corners[[1]])
  corners_x_values = corners[[1]]
  corners_y_values = corners[[2]]
  borders_x <- c(mpfr(0, prec))
  borders_y <- c(mpfr(0, prec))
  for (i in 1:len) {
    borders_x[i] = (corners_x_values[i] + corners_x_values[li(i,len)])/2
    borders_y[i] = (corners_y_values[i] + corners_y_values[li(i,len)])/2
  }
  return(list(x_values = borders_x, y_values = borders_y))
}


prec.getCurrentPolygon <- function(x,y,alphabet,borders) {
  #den Koordinaten einen Buchstaben zuordnen
  len = length(alphabet)
    for (i in 1:len) {
      if(prec.inAreaBetween(x,y,borders[[1]][i], borders[[2]][i], borders[[1]][ni(i,len)], borders[[2]][ni(i,len)])){
        return(alphabet[i])}
  }
}


prec.getPolygonCorners <- function(n){
  #Eckpunkte berechnen
  mpi <- Const("pi",prec)
  x = c(mpfr(0, prec))
  y = c(mpfr(0, prec))
  if(n == 4) {
    x[1] =  1; y[1] = -1
    x[2] = -1; y[2] = -1
    x[3] = -1; y[3] =  1
    x[4] =  1; y[4] =  1
  }
  else{
    for (i in 1:n){
      x[i] = sin(mpi*(2*i+1)/n)
      y[i] = cos(mpi*(2*i+1)/n)
    } 
  }
  res <- list("x_values" = x, "y_values" = y)
  return(res)
}


prec.stretchPolygon <- function(x,y,alphabet,corners,current_polygon,stretch) {
  #Punkt Abbilden gemäß einer Streckung
  alen = length(alphabet)
  Z   <- data.frame(x=0, y=0) #Streckzentrum
  res <- data.frame(x=0,y=0)
  for (i in 1:alen) {
    if(current_polygon == alphabet[i]) {
      Z$x = corners[[1]][i]
      Z$y = corners[[2]][i]
    }
  }
  #Abstand zwischen Zentrum und unserem Punkt
  dx=mpfr(x-Z$x, prec)
  dy=mpfr(y-Z$y, prec)
  res$x = mpfr(Z$x + (stretch)*dx, prec)  #stretched x
  res$y = mpfr(Z$y + (stretch)*dy, prec)  #stretched y
  return(res)
}


prec.getNSeq <- function(x,y,alphabet,len){
  #Decodieren Anhand von Koordinaten, Alphabet und Länge der Sequenz
  corners = prec.getPolygonCorners(length(alphabet))
  borders = prec.getDividingVectors(corners)
  alen = length(alphabet)
  mpi <- Const("pi",prec)
  stretch = 1/((sin(mpi/alen)/(sin(mpi/alen) + sin(mpi/alen +2 * mpi*(floor(alen/4)/alen)))))
  res <- c()
  letter = prec.getCurrentPolygon(x,y,alphabet,borders)
  res <- c(letter,res)
  if(len > 1){
    for (i in 2:len) {
      strechedPolygon = prec.stretchPolygon(x,y,alphabet,corners,letter,stretch)
      x = strechedPolygon$x
      y = strechedPolygon$y
      letter = prec.getCurrentPolygon(x,y,alphabet,borders)
      if(is.null(letter)) {return(res)} 
      res <- c(letter,res)
    }
  }
  return(res)
}


