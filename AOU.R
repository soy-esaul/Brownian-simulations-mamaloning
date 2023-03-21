library(ACSWR)

z0=runif(1,0,1)
zk=Ehrenfest(3/2)

trans_ehren <- function(N,i){
  uni <- runif(1,0,1)
  if(uni > i/N){
    return(1)
  }
  else{
    return(-1)
  }
}

cad <- c()

N <- 10**10

cad[1] = floor(N/2)



for(i in 2:(N-1)){
  cad[i] <- cad[i-1] + trans_ehren(N,cad[i-1])
}

plot(cad, type="l")

