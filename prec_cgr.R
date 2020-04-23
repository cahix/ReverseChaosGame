#Hier wurden die Funktionen aus dem Kaos Paket mit precision numbers versehen

library(Rmpfr)
library(kaos)


prec.distr.pts <- function(n,  r,  plot = F){
  mpi <- Const("pi",prec)
  #get coordinates for a regular polygon
  x = c(mpfr(0,prec))
  y = c(mpfr(0,prec))
  for (i in 1:n){
    x[i] = r*sin(mpi*(2*i+1)/n)
    y[i] = r*cos(mpi*(2*i+1)/n)
  }
  #generates a plot if required
  if (plot) {plot(x, y, pch = 20)}
  
  #return coordinates
  return(c(x,y))
}

distr.pts = function(n,  r,plot = F){
  #get coordinates for a regular polygon
  x = vector("double", n)
  y = vector("double", n)
  for (i in 1:n){
    x[i] = r*sinpi((2*i+1)/n)
    y[i] = r*cospi((2*i+1)/n)
  }
  
  #generates a plot if required
  if (plot) {plot(x, y, pch = 20)}
  
  #return coordinates
  return(xy.coords(x, y))
} 


prec.cgr <- function (data, seq.base = row.names(table(data)), sf = F, res = 100) 
{
  mpi <- Const("pi",prec)
  r = 1
  if (is.character(seq.base) && length(seq.base) == 1) {
    if (seq.base == "digits") {
      seq.base = c(0:9)
    }
    else if (seq.base == "AMINO") {
      seq.base = c("A", "C", "D", "E", 
                   "F", "G", "H", "I", "K", 
                   "L", "M", "N", "P", "Q", 
                   "R", "S", "T", "V", "W", 
                   "Y")
    }
    else if (seq.base == "amino") {
      seq.base = c("a", "c", "d", "e", 
                   "f", "g", "h", "i", "k", 
                   "l", "m", "n", "p", "q", 
                   "r", "s", "t", "v", "w", 
                   "y")
    }
    else if (seq.base == "DNA") {
      seq.base = c("A", "G", "T", "C")
    }
    else if (seq.base == "dna") {
      seq.base = c("a", "g", "t", "c")
    }
    else if (seq.base == "LETTERS") {
      seq.base = LETTERS
    }
    else if (seq.base == "letters") {
      seq.base = letters
    }
  }
  stopifnot(length(seq.base) >= length(table(data)), all(row.names(table(data)) %in% 
                                                           seq.base), sf <= 1, sf >= 0, res >= 1)
  base.num = length(seq.base)
  if (base.num == 4) {
    x = c(mpfr(1,prec),mpfr(-1,prec),mpfr(-1,prec),mpfr(1,prec))
    y = c(mpfr(-1,prec),mpfr(-1,prec),mpfr(1,prec),mpfr(1,prec))
    #base.coord = xy.coords(x, y)
    base.coord = c(x,y)
  }
  else {
    base.coord = prec.distr.pts(base.num, r)
  }
  if (!sf) {
    sf = mpfr(1-((sin(mpi/base.num)/(sin(mpi/base.num) + sin(mpi/base.num +2 * mpi*(floor(base.num/4)/base.num))))),prec)
  }
  #base = data.frame(x = base.coord$x, y = base.coord$y, row.names = seq.base)
  base.x.values = base.coord[1:base.num]
  base.y.values = base.coord[(base.num+1):(base.num*2)]
  data.length   = length(data)
  x = c(mpfr(0,prec))
  y = c(mpfr(0,prec))
  A = matrix(data = 0, ncol = res, nrow = res)
  pt = c(mpfr(0,prec),mpfr(0,prec)) 
  for (i in 1:data.length) {
    #pt = pt + (unlist(base[data[i], ]) - pt) * sf
    pt[1] = pt[1] + mpfr((base.x.values[match(data[i],seq.base)] - pt[1]) *sf  ,prec)
    pt[2] = pt[2] + mpfr((base.y.values[match(data[i],seq.base)] - pt[2]) *sf  ,prec)
    x[i] = pt[1]
    y[i] = pt[2]
    #x.matrix = ceiling((x[i] + r) * res/(2 * r))
    #y.matrix = ceiling((y[i] + r) * res/(2 * r))
    #A[x.matrix, y.matrix] = A[x.matrix, y.matrix] + 1
  }
  return(list(matrix = A, x = x, y = y, scaling_factor = sf,    resolution = res, base = c(base.x.values,base.y.values)))
  #return(c(x,y))
}

