#Diese Funktionen sind zum Lesen und Schreiben von Verschlüsselten Dateien
#Benötigt werden die "getLongNSeq" Funktionen


options(digits=22) # Wichtig, da sonst versteckte Nachkommastellen nicht exportiert werden



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



getPackageSize <- function(lengthOfAlphabet){
  #Paketgröße bestimmen in Abhängigkeit von der größe des Alphabetes
  if(lengthOfAlphabet < 10)          return (20)
    else if(lengthOfAlphabet < 15)   return (15)
      else if(lengthOfAlphabet < 50) return(10)
        else                         return(7)
}

splitStringByLenght <- function(string,splitSize){
  #String in Pakete der Größe splitSize aufteilen. Letztes Paket hat Restgröße
  res <- c()
  iterator = 1
  for(i in 1:(ceiling(nchar(string)/splitSize))){
    res[i] = substr(string,iterator,iterator+splitSize-1)
    iterator = iterator + splitSize
  }
  return(res)
}

writeListTo <- function(list, path = "C:\\Users\\tamino\\Desktop\\ChaosGame\\zippedText.txt"){
  #Dient zum schreiben von verschlüsselten Dateien 
  lapply(list, write, path, append=TRUE, ncolumns=1000)
}


readCompressed <- function(path){
  #Formatierung für das Einlesen von verschlüsselten Dateien
  lines             <- readLines(path)
  x_values          <- as.numeric(strsplit(lines[1], " ")[[1]])
  y_values          <- as.numeric(strsplit(lines[2], " ")[[1]])
  packagesizes      <- as.numeric(strsplit(lines[3], " ")[[1]])
  last_packagesize  <- as.numeric(strsplit(lines[4], " ")[[1]])
  alphabet          <-           (strsplit(lines[5], "" )[[1]])
  return(list(last_x_values = x_values, last_y_values = y_values, packagesizes = packagesizes,last_packagesize = last_packagesize, alphabet = alphabet))
}



encodeTextFromPath <- function(path){
  #Textdatei anhand des Dateipfades einlesen und verschlüsseln
  text <- readTextFile(path)
  alphabet <- alphabetOfString(text)
  packageSize <- getPackageSize(length(alphabet))
  splittedText <- strsplit(text ,"")[[1]]
  scgr <- splittedCgr(splittedText,packageSize,alphabet)
  scgr$alphabet <- paste(alphabet,collapse="")
  return(scgr)
}

decodeFromPath <- function(path){
  #Verschlüsselte Datei anhand des Dateipfades Entschlüsseln
  data  = readCompressed(path)
  res = getLongNSeq(data, data$alphabet)
  return(paste(res,collapse=""))
}



runTests <- function(){
options(digits = 22) ######### WICHTIG
path = "C:\\Uni\\ChaosGame\\bigText.txt"
text = readTextFile(path)
zip1 = encodeTextFromPath(path)
writeListTo(zip1,"C:\\Uni\\ChaosGame\\zippedShorterText.txt")
rzip <- readCompressed("C:\\Uni\\ChaosGame\\zippedShorterText.txt")
decodeFromPath("C:\\Uni\\ChaosGame\\zippedShorterText.txt")
}
