#Funktionen zum Testen und Plotten




library(ggplot2)


testFunc <- function(alphabet_length,word_len,seed=1){
  set.seed(seed)
  sample <- sample(1:alphabet_length,word_len,replace=TRUE)
  alphabet <- c(1:alphabet_length)
  corners <- getPolygonCorners(alphabet_length)
  cgr = cgr(sample,seq.base = c(1:alphabet_length))
  cgr.plot(cgr,mode = "points",labels=TRUE)
  last_x <- cgr$x[length(cgr$x)]
  last_y <- cgr$y[length(cgr$y)]
  result <- getNSeq(last_x,last_y,alphabet,word_len)
  print("SAMPLE:")
  print(sample)
  print("=========================================================================================")
  print("RESULT:")
  print(result)
  print("=========================================================================================")
  print("NUMBER OF FOLLOWING MATCHING ELEMENTS FROM THE END:")
  cntr = 0
  sample <- rev(sample)
  result <- rev(result)
  for(i in 1:length(result)) {
    if(sample[i] == result[i]) {
      cntr = cntr +1
    }
    else break
  }
  print(cntr)
  print("NUMBER OF ELEMENTS:")
  print(word_len)
}


recognizedLength <- function(seed,alphabet_length,word_len){
  set.seed(seed)
  sample <- sample(1:alphabet_length,word_len,replace=TRUE)
  alphabet <- c(1:alphabet_length)
  corners <- getPolygonCorners(alphabet_length)
  cgr = cgr(sample,seq.base = c(1:alphabet_length))
  last_x <- cgr$x[length(cgr$x)]
  last_y <- cgr$y[length(cgr$y)]
  result <- getNSeq(last_x,last_y,alphabet,word_len)
  cntr = 0
  sample <- rev(sample)
  result <- rev(result)
  for(i in 1:length(result)) {
    if(sample[i] == result[i]) {
      cntr = cntr +1
    }
    else break
  }
  return(cntr)
}


plotFuncAccuracy <- function(seed,alph_len) {
  accs <- c()
  for(i in 1:200) {
    accs <- c(accs,getAccuracy(seed,alph_len,i))
  }
  plot(accs~c(1:200),xlab="Länge des Wortes",ylab="Genauigkeit",main= c("Alphabetlänge",alph_len))
}


plotFuncRecognizedLength <- function(seed,alph_len,word_len_limit, alphabetname = "Alphabetlänge") {
  lens <- c()
  for(i in 1:word_len_limit) {
    lens <- c(lens,recognizedLength(seed,alph_len,i))
  }
  plot(lens~c(1:word_len_limit),xlab="Länge des Wortes",ylab="erkannte Buchstaben",main= c(alphabetname,alph_len))
}



getMeasures <- function(seeds,word_len_limit,alph_len){
  messwerte <- list()
  for(s in 1:seeds){
    erkannte.laengen <- c()
    for(j in 1:word_len_limit) {
      erkannte.laengen <- c(erkannte.laengen,recognizedLength(s,alph_len,j))
    }
    messwerte[[s]] <- erkannte.laengen
  }
  return(messwerte)
}


lowestNumberAfterRise <- function(vectorOfNumbers){
  #returns the lowest number after numbers stop rising in a vector
  res <- 0
  isRising <- TRUE
  for(i in vectorOfNumbers){
    if(isRising & i > res) res = i
    else if (i <= res) {isRising = FALSE;res =i}
  }
  return(res)
}


getHighestGuaranteedLength <- function(seeds,word_len_limit,alph_count){
  messwerte <- list()
  messwerte[[1]] = messwerte[[2]] = c(0)  # keine Alphabete der länge 1 oder 2
  for(a in 3:alph_count){
    lowestCaps <- c()
    for(s in 1:seeds){
      erkannte.laengen <- c()
      for(j in 1:word_len_limit) {
        erkannte.laengen <- c(erkannte.laengen,recognizedLength(s,a,j))
      }
      lowestCaps <- c(lowestCaps,lowestNumberAfterRise(erkannte.laengen))
    }
    messwerte[[a]] <- lowestCaps
  }
  return(messwerte)
}

getLowestHighestGuaranteedLength <- function(seeds,word_len_limit,alph_count){
  messwerte <- c()
  messwerte[1] = messwerte[2] = 0  # keine Alphabete der länge 1 oder 2
  for(a in 3:alph_count){
    print(paste("Alphabet number: ", a))
    lowestCaps <- c()
    for(s in 1:seeds){
      erkannte.laengen <- c()
      for(j in 1:word_len_limit) {
        erkannte.laengen <- c(erkannte.laengen,recognizedLength(s,a,j))
      }
      lowestCaps <- c(lowestCaps,lowestNumberAfterRise(erkannte.laengen))
    }
    messwerte[a] <- min(lowestCaps)
  }
  return(messwerte[3:length(messwerte)])
}



makePlot_wordlength_letters <- function(alphabet_length, max_word_length) {
  measures <- getMeasures(10,max_word_length,alphabet_length)
  seed1  = data.frame(WORTLÄNGE=c(1:max_word_length),ERKANNTE_BUCHSTABEN=measures[[1]])
  seed2  = data.frame(WORTLÄNGE=c(1:max_word_length),ERKANNTE_BUCHSTABEN=measures[[2]])
  seed3  = data.frame(WORTLÄNGE=c(1:max_word_length),ERKANNTE_BUCHSTABEN=measures[[3]])
  seed4  = data.frame(WORTLÄNGE=c(1:max_word_length),ERKANNTE_BUCHSTABEN=measures[[4]])
  seed5  = data.frame(WORTLÄNGE=c(1:max_word_length),ERKANNTE_BUCHSTABEN=measures[[5]])
  seed6  = data.frame(WORTLÄNGE=c(1:max_word_length),ERKANNTE_BUCHSTABEN=measures[[6]])
  seed7  = data.frame(WORTLÄNGE=c(1:max_word_length),ERKANNTE_BUCHSTABEN=measures[[7]])
  seed8  = data.frame(WORTLÄNGE=c(1:max_word_length),ERKANNTE_BUCHSTABEN=measures[[8]])
  seed9  = data.frame(WORTLÄNGE=c(1:max_word_length),ERKANNTE_BUCHSTABEN=measures[[9]])
  seed10 = data.frame(WORTLÄNGE=c(1:max_word_length),ERKANNTE_BUCHSTABEN=measures[[10]])
  seeds = rbind(seed1,seed2,seed3,seed4,seed5,seed6,seed7,seed8,seed9,seed10)
  
  plot <- ggplot(seeds, aes(x=as.factor(seeds$WORTLÄNGE), y=seeds$ERKANNTE_BUCHSTABEN)) + 
    geom_boxplot(fill="slateblue", alpha=0.2) + 
    xlab("Wortlänge") + ylab("erkannte Buchstaben") + ggtitle(paste("Alphabetlänge", alphabet_length)) +
    scale_x_discrete(breaks=seq(0, max_word_length, 5)) +  scale_y_continuous(breaks=seq(0, max_word_length, 5), limits = c(0, 60)) 
  
  print(plot)
}


makePlot_alphlength_letters <- function(number_of_alphabets, max_word_length,number_of_seeds=10) {
  measures <- getHighestGuaranteedLength(number_of_seeds, max_word_length, number_of_alphabets)
  seedlist <- list()
  for(i in 1:number_of_seeds){
    erk.buchstaben <- c()
    for(b in 3:number_of_alphabets){
      erk.buchstaben = c(erk.buchstaben, measures[[b]][i])
    }
    seedlist[[i]] = data.frame(ALPHABETLÄNGE=c(3:number_of_alphabets),MAXIMAL_ERKANNTE_BUCHSTABEN=erk.buchstaben)
  }
  seeds = do.call(rbind, seedlist)
  plot <- ggplot(seeds, aes(x=as.factor(seeds$ALPHABETLÄNGE), y=seeds$MAXIMAL_ERKANNTE_BUCHSTABEN)) + 
    geom_boxplot(fill="slateblue", alpha=0.2) + 
    xlab("Alphabetlänge") + ylab("maximal erkannte Buchstaben") + ggtitle("mit Sicherheit erkannte Buchstaben nach Alphabetlänge") +
    scale_x_discrete(breaks=seq(0, number_of_alphabets, 5)) +  scale_y_continuous(breaks=seq(0, max_word_length, 5), limits = c(0, 60)) 
  
  print(plot)
}


makePlot_alphlength_letters_lowest <- function(number_of_alphabets, max_word_length) {
  number_of_seeds = 10
  measures <- getLowestHighestGuaranteedLength(number_of_seeds, max_word_length, number_of_alphabets)
  df <- data.frame(x = c(3:number_of_alphabets), y = measures)
  spline_int <- as.data.frame(spline(df$x, df$y))
  ggplot(df) + 
    geom_line(data = spline_int , aes(x = x, y = y)) +
    scale_y_continuous(breaks=seq(0, max_word_length, 5), limits = c(0, 60)) + scale_x_continuous(breaks=seq(0, number_of_alphabets, 10)) +
    xlab("Alphabetlänge") + ylab("maximal erkannte Buchstaben") + ggtitle("mit Sicherheit erkannte Buchstaben nach Alphabetlänge")
}



