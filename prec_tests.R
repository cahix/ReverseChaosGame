#Funktionen zum Testen und Plotten von den Funktionen mit precision numbers

library(ggplot2)


prec.testFunc <- function(alphabet_length,word_len,seed=1){
  set.seed(seed)
  sample <- sample(1:alphabet_length,word_len,replace=TRUE)
  alphabet <- c(1:alphabet_length)
  corners <- prec.getPolygonCorners(alphabet_length)
  cgr = prec.cgr(sample,seq.base = c(1:alphabet_length))
  last_x <- cgr$x[length(cgr$x)]
  last_y <- cgr$y[length(cgr$y)]
  result <- prec.getNSeq(last_x,last_y,alphabet,word_len)
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



prec.recognizedLength <- function(seed,alphabet_length,word_len){
  set.seed(seed)
  sample <- sample(1:alphabet_length,word_len,replace=TRUE)
  alphabet <- c(1:alphabet_length)
  corners <- prec.getPolygonCorners(alphabet_length)
  prec.cgr = prec.cgr(sample,seq.base = c(1:alphabet_length))
  last_x <- prec.cgr$x[length(prec.cgr$x)]
  last_y <- prec.cgr$y[length(prec.cgr$y)]
  result <- prec.getNSeq(last_x,last_y,alphabet,word_len)
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


prec.getHighestLength <- function(seeds,word_len_limit,alph_count){
  messwerte <- list()
  messwerte[[1]] = messwerte[[2]] = c(0)  # keine Alphabete der länge 1 oder 2
  for(a in 3:alph_count){
    print(paste("alphabet ", a))
    erkannte.laengen <- c()
    for(s in 1:seeds){
      erkannte.laengen <- c(erkannte.laengen,prec.recognizedLength(s,a,word_len_limit))
    }
    messwerte[[a]] <- erkannte.laengen
  }
  return(messwerte)
}

prec.makePlot_alphlength_letters <- function(number_of_alphabets, max_word_length,number_of_seeds=10,ylim) {
  measures <- prec.getHighestLength(number_of_seeds, max_word_length, number_of_alphabets)
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
    xlab("Alphabetlänge") + ylab("maximal erkannte Buchstaben") + ggtitle(paste("erkannte Buchstaben nach Alphabetlänge mit",prec,"Bits Präzision",sep = " ")) +
    scale_x_discrete(breaks=seq(0, number_of_alphabets, 5)) +  
    scale_y_continuous(breaks=seq(0, max_word_length, 20), limits = c(0, (max_word_length+10))) 
  
  print(plot)
}



prec.getHighestLengthByPrecision <- function(seeds,prec_count,alen){
  messwerte <- list()
  messwerte[[1]]= c(0)  
  for(b in 54:prec_count){
    prec <<- b
    print(paste("prec ", b))
    erkannte.laengen <- c()
    for(s in 1:seeds){
      erkannte.laengen <- c(erkannte.laengen,prec.recognizedLength(s,alen,b+50))
    }
    messwerte[[b]] <- erkannte.laengen
  }
  return(messwerte)
}


prec.makePlot_precision_letters <- function(number_of_alphabets, max_word_length,number_of_seeds=10) {
  measures <- prec.getHighestLengthByPrecision(number_of_seeds,500,4)
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
    xlab("Alphabetlänge") + ylab("maximal erkannte Buchstaben") + ggtitle(paste("erkannte Buchstaben nach Alphabetlänge mit",prec,"Bits Präzision",sep = " ")) +
    scale_x_discrete(breaks=seq(0, number_of_alphabets, 5)) +  
    scale_y_continuous(breaks=seq(0, max_word_length, 20), limits = c(0, 210)) 
  
  print(plot)
}

