#---------------------------------------------------------------------------------
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
library(rstan);library(StanHeaders)
library("MASS");library("MCMCpack")
library(splines); library("survival"); library(leaps)


# Lectura base de datos completa

baseComp <- read.csv2("baseUsar.csv")

# Base de datos filtrada por las variables a usar

base <- baseComp %>%
  dplyr::select("pac_hos_","Edad","sexo_", "comuna","tip_cas_","tipo_ss_","modoViol", "year_") %>%
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

indv <- sample(1:53220, 25000)
Datos <- Datos1[indv,]
Datos <- Datos %>%
  dplyr::select("pac_hos_","Edad","sexo_","tipo_ss_","modoViol" )

attach(Datos)

#-------------------------------------------------------------------------------------

datos2 <- Datos
# declaración de las variables a usar en el modelo

#variable respuesta: Si el paciente es hospitalizado o no.
y <- pac_hos_

# Covariables: 
#   Edad 
#   Modo violencia, 3 niveles (fisica, sexual y sicológica)
#   Tipo de seguridad social, 4 niveles (contr, subs, especial, no tiene)
#   genero, 2 niveles (masculino, femenino)
x1 <- (Edad - mean(Edad))/sd(Edad) #edad estandarizada
# table(x1)
x2 <- tipo_ss_
# table(x2)
x3 <-  modoViol
# table(x3)
x4 <- sexo_ 
# table(x4)
#---------------Modelo completo--------------------------------------------
X = model.matrix(~ x1 + x2 + x3+x4) #8 betas por que las var son categoricas 
head(X)

stan_data <- list(
  
  "X" = X,
  
  "y" = y,
  
  "N" = nrow(datos2), # Numero de observaciones
  "P" = ncol(X) # numero de varaibles
)


fit <- stan(file = 'pois_model.stan', data = stan_data, chains = 3, iter = 9000)

print(fit)

str(fit@sim$samples)
fit@sim$samples
win.graph()
par(mfrow=c(2,4))
for(i in 1:8){
  ts.plot(parameters$beta[,i])
  
}

# Resultado de referencia

modelo_freq <- glm(y ~ x1 + x2+x3+x4, data = datos2, family = binomial)
summary(modelo_freq)

Beta.poste = extract(fit, pars = "beta")

win.graph()
par(mfrow=c(2,4))
for(i in 1:dim(Beta.poste$beta)[2]){
  HDI.interval.beta <- hdi(Beta.poste$beta[,i])
  value1 <- HDI.interval.beta[1]
  value2 <- HDI.interval.beta[2]
  DENSITITY.BETA <- density(Beta.poste$beta[,i])
  plot(DENSITITY.BETA, main = "Densidad Posterior", xlab = parse(text=(paste0("beta[",i,"]"))))
  DENSITITY.BETAy <- DENSITITY.BETA$y
  DENSITITY.BETAx <- DENSITITY.BETA$x
  # Lower and higher indices on the X-axis
  l <- min(which(DENSITITY.BETAx >= value1))
  h <- max(which(DENSITITY.BETAx < value2))
  
  polygon(c(DENSITITY.BETAx[c(l, l:h, h)]),
          c(0, DENSITITY.BETAy[l:h], 0),
          col = "slateblue1")
  #Fin
}

verosimilitud = function(Beta, X, y){  
  res = ((exp(X%*%Beta)/(1+exp(X%*%Beta)))^y) * (( 1/(1+exp(X%*%Beta))  )^(1-y))
  return(res)
}



#----------------------Modelo 1----------------------------
 
X_2 = model.matrix(~ x1+x2+x3) #7 betas. sin género 
head(X_2)
stan_data_2 <- list(
  
  "X" = X_2,
  
  "y" = y,
  
  "N" = nrow(datos2), # Numero de observaciones
  "P" = ncol(X_2) # numero de varaibles
)

fit2 <- stan(file = 'pois_model.stan', data = stan_data_2, chains = 3, iter = 9000)
print(fit2)

###############Factor de bayes######################
post_mol1 <- extract(fit_signi, pars="beta") 
post_mol2 <- extract(fit_model2, pars="beta") 
post_mol3 <- extract(fit_models12, pars="beta") 

vero.marginal1 = mean(sapply(1:dim(post_mol1$beta)[1], 
                             function(j) exp(sum(log(sapply(1:length(y), 
                                                            function(i){verosimilitud(post_mol1$beta[j,], 
                                                                                      X_signi[i,], y[i])}))))))
vero.marginal2 = mean(sapply(1:dim(post_mol2$beta)[1], 
                             function(j) exp(sum(log(sapply(1:length(y), 
                                                            function(i){verosimilitud(post_mol2$beta[j,], 
                                                                                      X2[i,], y[i])}))))))
vero.marginal3 = mean(sapply(1:dim(post_mol3$beta)[1], 
                             function(j) exp(sum(log(sapply(1:length(y), 
                                                            function(i){verosimilitud(post_mol3$beta[j,], 
                                                                                      X_models12[i,], y[i])}))))))

######################LOO - WAIC######################
comp1 = extract_log_lik(fit_signi, merge_chains = F)
comp2 = extract_log_lik(fit_model2, merge_chains = F)
comp3 = extract_log_lik(fit_models12, merge_chains = F)

#####################Comparación modelo 1 y 3 #################
BF1_3 = vero.marginal1/vero.marginal3
comp_1= loo(comp1)
comp_3= loo(comp3)

######################Comparación 2 y 3##################
BF2_3 = vero.marginal2/vero.marginal3
comp_2= loo(comp2)
comp_3= loo(comp3)

#####################Comparación modelo 1 y 2 #################
BF1_2 = vero.marginal1/vero.marginal2
comp_1= loo(comp1)
waic(comp1)
comp_2= loo(comp2)

