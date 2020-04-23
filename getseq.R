#Die Funktionen in diesem Abschnitt dienen nur zur Entschlüsselung mit dem  Alphabet {C,A,T,G}

library(kaos)


in_C <- function(x,y) {x <= 0 & y < 0}
in_A <- function(x,y) {x > 0 & y <= 0}
in_T <- function(x,y) {x >= 0 & y > 0}
in_G <- function(x,y) {x < 0 & y >= 0}


getCurrentSquare <- function(x,y) {
  if (in_C(x,y)) {return("C")}
  if (in_A(x,y)) {return("A")}
  if (in_T(x,y)) {return("T")}
  if (in_G(x,y)) {return("G")}
}


stretchCurrentSquare <- function(x,y) {
  #Hier passiert eine zentrische Streckung mit dem äußeren Eckpunkt des jeweiligen Quadranten als Zentrum
  #Es wird mit dem Faktor 2 gestreckt, um die Quadranten auf die Größe des ganzen Quadrates zu bringen
  #Eingabe: Koordinaten eines Punktes
  #Ausgabe: Koordinaten des Punktes nach Streckung
  Z <- data.frame(x=0, y=0)                   #Streckzentrum
  if (in_C(x,y)) {Z$x = -1; Z$y = -1}         
  if (in_A(x,y)) {Z$x =  1; Z$y = -1}
  if (in_T(x,y)) {Z$x =  1; Z$y =  1}
  if (in_G(x,y)) {Z$x = -1; Z$y =  1}
  dx=x-Z$x                                   #Abstand zwischen Zentrum und unserem Punkt
  dy=y-Z$y
  streched_x = Z$x + 2*dx
  streched_y = Z$y + 2*dy
  return(streched_P <- data.frame(x=streched_x, y=streched_y))
}


getSeq <- function(len, x, y) {
  #Funktion zum Entschlüsseln
  #Eingabe: Länge der Sequenz, x- und y-Koordinate
  #Ausgabe: Entschlüsselte Sequenz
  res = ""
   for (i in 1:len) {
     res = paste(getCurrentSquare(x,y), res, sep="")
     strechedQuadrant = stretchCurrentSquare(x,y)
     x = strechedQuadrant$x
     y = strechedQuadrant$y
   }
  return(res)
}






