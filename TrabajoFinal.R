#librerías usadas

library(dplyr)
library(R2jags)
library(HDInterval)
library(coda)
library(rjags)
library(lattice)
library(superdiag)
library(stringr)
library(ISLR)
library(ggplot2)
library("MASS"); library(lattice);library("coda");library("MCMCpack")
library(splines); library("survival"); library(leaps)


# Lectura base de datos completa

baseComp <- read.csv2("baseUsar.csv")

# Base de datos filtrada por las variables a usar

base <- baseComp %>%
  select("pac_hos_","Edad","sexo_", "comuna","tip_cas_","tipo_ss_",
         "modoViol", "year_") %>%
  filter(baseComp$comuna !="SIN INFORMACION",baseComp$comuna != "Sin informacion")

base$comuna <- str_replace(base$comuna, "Doce De Octubre","Doce de Octubre")
base$comuna <- str_replace(base$comuna, "Corregimiento De Santa Elena","Santa Elena")

# Aunque la variable Comuna no se usa en nuestro modelo, la quisimos incluir para hacer futuras
# comparaciones con los resultados.

# Ya que en el modelo logistico, la var. respuesta se tiene en cuenta una distribución 
# bernoulli la cual solo toma valores entre 0 y 1, y en la base están en 1 y 2, 
# restamos 1 para que esté entre 0 y 1

base$pac_hos_ <- base$pac_hos_ -2
base$pac_hos_ <- abs(base$pac_hos_)

# Convertir variales que son factores

base$sexo_ <- as.factor(base$sexo_)
base$modoViol <- as.factor(base$modoViol) #hay presencia de NA's
base$tipo_ss_ <- as.factor(base$tipo_ss_)

# eliminar NA's de la variable modoViolencia y los datos atípicos de la edad

Datos1 <- base
Datos1 <- Datos1[!is.na(Datos1$modoViol),]
Datos1 <- Datos1[!Datos1$Edad < 1,]
Datos1 <- Datos1[!Datos1$Edad >=100,]

#Selección de una muestra para correr el modelo logístico

indv <- sample(1:53220, 20000)
Datos <- Datos1[indv,]
Datos <- Datos %>%
  select("pac_hos_","Edad","sexo_","tipo_ss_","modoViol" )

attach(Datos)

# declaración de las variables a usar en el modelo

#variable respuesta: Si el paciente es hospitalizado o no.
y <- pac_hos_

# Covariables: 
#   Edad 
#   Modo violencia, 3 niveles (fisica, sexual y sicológica)
#   Tipo de seguridad social, 4 niveles (contr, subs, especial, no tiene)
#   genero, 2 niveles (masculino, femenino)
x1 <- Edad
# table(x1)
x2 <- tipo_ss_
# table(x2)
x3 <-  modoViol
# table(x3)
x4 <- sexo_ 
# table(x4)

#Matriz que almacena los betas de las covariables

X <-  model.matrix(~ x1 + x2 + x3 + x4) 

betasDim <- head(X) #8 betas por que las var son categoricas 

# Definición del modelo 

modelo <- function(){
  for (i in 1:N) {
    y[i] ~ dbern(p[i]) 
    logit(p[i]) <- inprod(b[], x[i,])
  }
  for (j in 1:K) { # apriori del modelo (vaga o poco informativa)
    b[j] ~ dnorm(0,1.0E-12) # definimos beta como vector
  }
}

#Tamano de la muestra

N = dim(Datos)[1]

# Input o informacion de entrada

data.input.jags <- list(y=y, x = X, N = N, K = ncol(X))  #k N. de parametros de las x


#Parametros a monitorear

bayes.mod.params <- c("b") 

#Puntos iniciales de la cadena MCMC

set.seed(123)
bayes.mod.inits <- function(){ # 8 valores iniciales de la cadena.
  list("b" = rnorm(8,0,0.001)) #simular los valores iniciales de los betas, con
  # una normal poco informativa, no en 0 ya que los datos reales son > 0
}

bayes.mod.fit <- jags(data = data.input.jags, inits = bayes.mod.inits,
                      parameters.to.save = bayes.mod.params, 
                      n.chains = 3, n.iter = 9000,
                      n.burnin = 1500, model.file = modelo)


print(bayes.mod.fit)

## Mismo modelo pero para poder sacar las gráficas de densidad y los traceplot
## de la convergencia

posterior2<-MCMClogit(pac_hos_~ Edad+tipo_ss_+modoViol+sexo_ , b0=0, B0=.001,
                      data=Datos,burnin=1000,mcmc=25000)
summary(posterior2)

# trace plot
win.graph()
plot(posterior2,trace=FALSE)
#densidad
win.graph()
plot(posterior2, density=FALSE)
# autocorrelación
win.graph()
autocorr.plot(posterior2)
# geweke con zscore
geweke.diag(posterior2)
win.graph() nnnnnnnnnnn
geweke.plot(posterior2) 

#Modelos para comparar

# Modelo sin la variable sexo

X2 <-  model.matrix(~ x1 + x2 + x3) 

betasDim <- head(X2) #8 betas por que las var son categoricas 

# Definición del modelo 

modelo <- function(){
  for (i in 1:N) {
    y[i] ~ dbern(p[i]) 
    logit(p[i]) <- inprod(b[], x[i,])
  }
  for (j in 1:K) { # apriori del modelo (vaga o poco informativa)
    b[j] ~ dnorm(0,1.0E-12) # definimos beta como vector
  }
}

#Tamano de la muestra

N = dim(Datos)[1]

# Input o informacion de entrada

data.input.jags2 <- list(y=y, x = X2, N = N, K = ncol(X2))  #k N. de parametros de las x


#Parametros a monitorear

bayes.mod.params2 <- c("b") 

#Puntos iniciales de la cadena MCMC

set.seed(123)
bayes.mod.inits2 <- function(){ # 8 valores iniciales de la cadena.
  list("b" = rnorm(7,0,0.001)) #simular los valores iniciales de los betas, con
  # una normal poco informativa, no en 0 ya que los datos reales son > 0
}

bayes.mod.fit2 <- jags(data = data.input.jags2, inits = bayes.mod.inits2,
                      parameters.to.save = bayes.mod.params2, 
                      n.chains = 3, n.iter = 9000,
                      n.burnin = 1500, model.file = modelo)


print(bayes.mod.fit2)

## Mismo modelo pero para poder sacar las gráficas de densidad y los traceplot
## de la convergencia

posterior22<-MCMClogit(pac_hos_~ Edad+tipo_ss_+modoViol , b0=0, B0=.001,
                      data=Datos,burnin=1000,mcmc=25000)
summary(posterior22)

# trace plot
win.graph()
plot(posterior22,trace=FALSE)
#densidad
win.graph()
plot(posterior22, density=FALSE)
# autocorrelación
win.graph()
autocorr.plot(posterior22)
# geweke con zscore
geweke.diag(posterior22)
win.graph()
geweke.plot(posterior22) 

# Modelo sin la variable EDAD

X3 <-  model.matrix(~ x2 + x3 + x4) 

betasDim <- head(X3) #8 betas por que las var son categoricas 

# Definición del modelo 

modelo <- function(){
  for (i in 1:N) {
    y[i] ~ dbern(p[i]) 
    logit(p[i]) <- inprod(b[], x[i,])
  }
  for (j in 1:K) { # apriori del modelo (vaga o poco informativa)
    b[j] ~ dnorm(0,1.0E-12) # definimos beta como vector
  }
}

#Tamano de la muestra

N = dim(Datos)[1]

# Input o informacion de entrada

data.input.jags3 <- list(y=y, x = X3, N = N, K = ncol(X3))  #k N. de parametros de las x


#Parametros a monitorear

bayes.mod.params3 <- c("b") 

#Puntos iniciales de la cadena MCMC

set.seed(123)
bayes.mod.inits3 <- function(){ # 8 valores iniciales de la cadena.
  list("b" = rnorm(7,0,0.001)) #simular los valores iniciales de los betas, con
  # una normal poco informativa, no en 0 ya que los datos reales son > 0
}

bayes.mod.fit3 <- jags(data = data.input.jags3, inits = bayes.mod.inits3,
                       parameters.to.save = bayes.mod.params3, 
                       n.chains = 3, n.iter = 9000,
                       n.burnin = 1500, model.file = modelo)


print(bayes.mod.fit3)

## Mismo modelo pero para poder sacar las gráficas de densidad y los traceplot
## de la convergencia

posterior23<-MCMClogit(pac_hos_~ tipo_ss_+modoViol+sexo_ , b0=0, B0=.001,
                       data=Datos,burnin=1000,mcmc=25000)
summary(posterior23)

# trace plot
win.graph()
plot(posterior23,trace=FALSE)
#densidad
win.graph()
plot(posterior23, density=FALSE)
# autocorrelación
win.graph()
autocorr.plot(posterior23)
# geweke con zscore
geweke.diag(posterior23)
win.graph()
geweke.plot(posterior23) 


