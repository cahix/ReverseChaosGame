#Dateiverwaltung mit precision numbers 

library(Rmpfr)



readTextFile <- function(path){
  #Textdatei einlesen und formatieren
  text <- readLines(path)
  pastedText <- ""
  for(i in 1:length(text)){
    pastedText <- paste(pastedText, text[i], sep ="")
  }
  return(pastedText)
}



alphabetOfString <- function(string){
  #Alle vorkommenden Buchstaben eines Strings bestimmen
  string_split <- strsplit(string, "")[[1]]
  alphabet <- unique(string_split)
  return(alphabet)
}

alphabetOfStringVec <- function(stringvec){
  #Alle vorkommenden Buchstaben in einem Vektor bestimmen
  alphabet <- c()
  for(s in stringvec){
    alphabet <- c(alphabet, alphabetOfString(s))
  }
  return(unique(alphabet))
}



prec.writeEncodedFile <- function(encodedList, path = "C:\\Uni\\ChaosGame\\prec_encrypted_Text.txt"){
  #Dient zum schreiben von verschlüsselten Dateien 
  sink(path)
  cat(formatMpfr(encodedList$last_x_value,trim = TRUE))
  cat("\n")
  cat(formatMpfr(encodedList$last_y_value,trim = TRUE))
  cat("\n")
  cat(encodedList$length)
  cat("\n")
  sink()
}


prec.readCompressed <- function(path){
  #Formatierung für das Einlesen von verschlüsselten Dateien
  lines            <- readLines(path)
  x_value          <- mpfr((strsplit(lines[1], " ")[[1]]),prec)
  y_value          <- mpfr((strsplit(lines[2], " ")[[1]]),prec)
  length           <- as.numeric(strsplit(lines[3], " ")[[1]])
  return(list(last_x_value = x_value, last_y_value = y_value, length = length))
}



prec.encodeTextFromPath <- function(path){
  #Textdatei anhand des Dateipfades einlesen und verschlüsseln
  text <- readTextFile(path)
  alphabet <- alphabetOfString(text)
  splittedText <- strsplit(text ,"")[[1]]
  textlen <- length(splittedText)
  prec <<- 50 + (textlen*3)                                          #überschlagener Wert, bei sehr langen Texten evtl. nicht ausreichend
  p.cgr <- prec.cgr(splittedText,seq.base = alphabet)
  res <- list("last_x_value" = p.cgr$x[length(p.cgr$x)], "last_y_value" = p.cgr$y[length(p.cgr$y)], "length" = textlen)
  return(res)
}

prec.decodeFromPath <- function(path,alphabet){
  #Verschlüsselte Datei anhand des Dateipfades Entschlüsseln
  data  = prec.readCompressed(path)
  res = prec.getNSeq(data$last_x_value, data$last_y_value, alphabet, data$length)
  return(paste(res,collapse=""))
}


runTestFuncs <- function(){
  path = "C:\\Uni\\ChaosGame\\bigText.txt"
  text = readTextFile(path)
  alph = alphabetOfString(text)
  pzip = prec.encodeTextFromPath(path)
  prec.writeEncodedFile(pzip)
  rzip <- prec.readCompressed("C:\\Uni\\ChaosGame\\prec_encrypted_Text.txt")
  prec.decodeFromPath("C:\\Uni\\ChaosGame\\prec_encrypted_Text.txt",alph)
}



