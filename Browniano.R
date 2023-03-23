library(ggplot2)

# Esta función simula una aproximación de un movimiento Browiniano
browniano <- function(N,p){
  n <- rbinom(N,1,p)
  n <- 1*(n==1) - 1*(n==0)
  z <- c(0)
  for(i in 1:(N-1)){
    z[i+1] <- z[i] + n[i]
  }
  z <- z/sqrt(N)
  return(z)
}

# Generar trayectorias del movimiento browninano
p <- 1/2
N <- 10**4
axis <- seq(0,1,length=N)
frame <- data.frame(axis)
for(i in 1:6){
  b <- browniano(N,p)
  frame <- data.frame(frame,b)
}

bplot <- ggplot( data=frame, aes(x=axis)) + geom_line( aes(y=b), color="red") +
  geom_line( aes(y=b.1), color="blue") + geom_line( aes(y=b.2), color="darkolivegreen") + geom_line( aes(y=b.3), color="purple") +
  geom_line( aes(y=b.4), color="orange") + geom_line( aes(y=b.5), color="salmon") +
  labs(x=quote(t), y=quote(X[t]))
print(bplot)


# Una sola tryectoria

ggplot(data = frame, aes(x=axis)) + geom_line( aes(y=b.2), color="darkolivegreen") + labs(x=quote(t), y=quote(X[t]))

# Función sqrt(t)
msqrt <- function(vec){
  return(-sqrt(vec))
}

sqrtframe <- data.frame(axis)
for(i in 1:6){
  b <- browniano(N,p)
  sqrtframe <- data.frame(sqrtframe,b)
}

sqrtplot <- ggplot( data=sqrtframe, aes(x=axis)) + geom_line( aes(y=b), color="red") +
  geom_line( aes(y=b.1), color="blue") + geom_line( aes(y=b.2), color="darkolivegreen") + geom_line( aes(y=b.3), color="purple") +
  geom_line( aes(y=b.4), color="orange") + geom_line( aes(y=b.5), color="salmon") +
  geom_function(fun=sqrt, aes(color="Desviación estándar") ) + geom_function(fun=msqrt, aes(color="Desviación estándar") ) + labs(x=quote(t), y=quote(X[t])) +
  scale_color_manual(name=NULL , breaks=c('Desviación estándar'), values=c('Desviación estándar'='#00B8E7')) +
  theme(legend.position = c(0.15, 0.9) )
print(sqrtplot)


# Función deriva (variar p_N)
p_N <- 0.58

deriva <- function(vec){
  sqrt(N)*(2*p_N - 1)*vec
}

derframe <- data.frame(axis)
for(i in 1:6){
  b <- browniano(N,p_N)
  derframe <- data.frame(derframe,b)
}
derplot <- ggplot( data=derframe, aes(x=axis)) + geom_line( aes(y=b), color="red") +
  geom_line( aes(y=b.1), color="blue") + geom_line( aes(y=b.2), color="darkolivegreen") + geom_line( aes(y=b.3), color="purple") +
  geom_line( aes(y=b.4), color="orange") + geom_line( aes(y=b.5), color="salmon") +
  geom_function(fun=deriva, aes(color="Deriva") ) + labs(x=quote(t), y=quote(X[t])) + 
  labs(x=quote(t), y=quote(X[t])) + scale_color_manual(name=NULL , breaks=c('Deriva'), values=c('Deriva'='#00B8E7')) +
  theme(legend.position = c(0.15, 0.9) )
print(derplot)

# Con p_N = 0.42
p_N <- 0.42

deriva <- function(vec){
  sqrt(N)*(2*p_N - 1)*vec
}

derframe <- data.frame(axis)
for(i in 1:6){
  b <- browniano(N,p_N)
  derframe <- data.frame(derframe,b)
}
derplot <- ggplot( data=derframe, aes(x=axis)) + geom_line( aes(y=b), color="red") +
  geom_line( aes(y=b.1), color="blue") + geom_line( aes(y=b.2), color="darkolivegreen") + geom_line( aes(y=b.3), color="purple") +
  geom_line( aes(y=b.4), color="orange") + geom_line( aes(y=b.5), color="salmon") +
  geom_function(fun=deriva, aes(color="Deriva") ) + labs(x=quote(t), y=quote(X[t])) + 
  labs(x=quote(t), y=quote(X[t])) + scale_color_manual(name=NULL , breaks=c('Media'), values=c('Media'='#00B8E7')) +
  theme(legend.position = c(0.15, 0.9) )
print(derplot)

# Con p_N = 0.8
p_N <- 0.8

deriva <- function(vec){
  sqrt(N)*(2*p_N - 1)*vec
}

derframe <- data.frame(axis)
for(i in 1:6){
  b <- browniano(N,p_N)
  derframe <- data.frame(derframe,b)
}
derplot <- ggplot( data=derframe, aes(x=axis)) + geom_line( aes(y=b), color="red") +
  geom_line( aes(y=b.1), color="blue") + geom_line( aes(y=b.2), color="darkolivegreen") + geom_line( aes(y=b.3), color="purple") +
  geom_line( aes(y=b.4), color="orange") + geom_line( aes(y=b.5), color="salmon") +
  geom_function(fun=deriva, aes(color="Deriva") ) + labs(x=quote(t), y=quote(X[t])) + 
  labs(x=quote(t), y=quote(X[t])) + scale_color_manual(name=NULL , breaks=c('Media'), values=c('Media'='#00B8E7')) +
  theme(legend.position = c(0.15, 0.9) )
print(derplot)

# Con p_N = 0.2
p_N <- 0.2

deriva <- function(vec){
  sqrt(N)*(2*p_N - 1)*vec
}

derframe <- data.frame(axis)
for(i in 1:6){
  b <- browniano(N,p_N)
  derframe <- data.frame(derframe,b)
}
derplot <- ggplot( data=derframe, aes(x=axis)) + geom_line( aes(y=b), color="red") +
  geom_line( aes(y=b.1), color="blue") + geom_line( aes(y=b.2), color="darkolivegreen") + geom_line( aes(y=b.3), color="purple") +
  geom_line( aes(y=b.4), color="orange") + geom_line( aes(y=b.5), color="salmon") +
  geom_function(fun=deriva, aes(color="Deriva") ) + labs(x=quote(t), y=quote(X[t])) + 
  labs(x=quote(t), y=quote(X[t])) + scale_color_manual(name=NULL , breaks=c('Media'), values=c('Media'='#00B8E7')) +
  theme(legend.position = c(0.15, 0.9) )
print(derplot)


# Función sqrt con deriva
alpha <- 4
p_N <- 1/2 + alpha / (2*sqrt(N))
banda1 <- function(vec){
  return( alpha*vec + sqrt(vec) )
}

banda2 <- function(vec){
  return( alpha*vec - sqrt(vec) )
}

evoframe <- data.frame(axis)
for(i in 1:6){
  b <- browniano(N,p_N)
  evoframe <- data.frame(evoframe,b)
}

evoplot <- ggplot( data=evoframe, aes(x=axis)) + geom_line( aes(y=b), color="red") +
  geom_line( aes(y=b.1), color="blue") + geom_line( aes(y=b.2), color="darkolivegreen") + geom_line( aes(y=b.3), color="purple") +
  geom_line( aes(y=b.4), color="orange") + geom_line( aes(y=b.5), color="salmon") +
  geom_function(fun=banda1, aes(color="Desviación estándar")) + geom_function(fun=banda2, aes(color="Desviación estándar")) + 
  labs(x=quote(t), y=quote(X[t])) + scale_color_manual(name=NULL , breaks=c('Desviación estándar'), values=c('Desviación estándar'='#00B8E7')) +
  theme(legend.position = c(0.15, 0.9) )
print(evoplot)


# Función sqrt con deriva con alpha = 20
alpha <- 20
p_N <- 1/2 + alpha / (2*sqrt(N))
banda1 <- function(vec){
  return( alpha*vec + sqrt(vec) )
}

banda2 <- function(vec){
  return( alpha*vec - sqrt(vec) )
}

evoframe <- data.frame(axis)
for(i in 1:6){
  b <- browniano(N,p_N)
  evoframe <- data.frame(evoframe,b)
}

evoplot <- ggplot( data=evoframe, aes(x=axis)) + geom_line( aes(y=b), color="red") +
  geom_line( aes(y=b.1), color="blue") + geom_line( aes(y=b.2), color="darkolivegreen") + geom_line( aes(y=b.3), color="purple") +
  geom_line( aes(y=b.4), color="orange") + geom_line( aes(y=b.5), color="salmon") +
  geom_function(fun=banda1, aes(color="Desviación estándar")) + geom_function(fun=banda2, aes(color="Desviación estándar")) + 
  labs(x=quote(t), y=quote(X[t])) + scale_color_manual(name=NULL , breaks=c('Desviación estándar'), values=c('Desviación estándar'='#00B8E7')) +
  theme(legend.position = c(0.15, 0.9) )
print(evoplot)


# Función sqrt con deriva con alpha = 40
alpha <- 20
p_N <- 1/2 + alpha / (2*sqrt(N))
banda1 <- function(vec){
  return( alpha*vec + sqrt(vec) )
}

banda2 <- function(vec){
  return( alpha*vec - sqrt(vec) )
}

evoframe <- data.frame(axis)
for(i in 1:6){
  b <- browniano(N,p_N)
  evoframe <- data.frame(evoframe,b)
}

evoplot <- ggplot( data=evoframe, aes(x=axis)) + geom_line( aes(y=b), color="red") +
  geom_line( aes(y=b.1), color="blue") + geom_line( aes(y=b.2), color="darkolivegreen") + geom_line( aes(y=b.3), color="purple") +
  geom_line( aes(y=b.4), color="orange") + geom_line( aes(y=b.5), color="salmon") +
  geom_function(fun=banda1, aes(color="Desviación estándar")) + geom_function(fun=banda2, aes(color="Desviación estándar")) + 
  labs(x=quote(t), y=quote(X[t])) + scale_color_manual(name=NULL , breaks=c('Desviación estándar'), values=c('Desviación estándar'='#00B8E7')) +
  theme(legend.position = c(0.15, 0.9) )
print(evoplot)
