library("MASS"); library(lattice);library("coda");library("MCMCpack")
library(splines); library("survival"); library(leaps)

Datos <- Datos

y<- y

posterior<-MCMClogit(pac_hos_~ Edad+tipo_ss_+modoViol+sexo_
                       , data=Datos,burnin=1000,mcmc=25000)
summary(posterior)
win.graph()
plot(posterior,trace=FALSE)
win.graph()
plot(posterior, density=FALSE)
win.graph()
autocorr.plot(posterior)
win.graph()
geweke.diag(posterior,frac1=0.1, frac2=0.5)
win.graph()
geweke.plot(posterior,pvalue=0.05)


#normal a priori

posterior2<-MCMClogit(pac_hos_~ Edad+tipo_ss_+modoViol+sexo_ , b0=0, B0=.001,
                        data=Datos,burnin=1000,mcmc=25000)
summary(posterior2)
win.graph()
plot(posterior2,trace=FALSE)
win.graph()
plot(posterior2, density=FALSE)
win.graph()
autocorr.plot(posterior2)
geweke.diag(posterior2)
win.graph() nnnnnnnnnnn
geweke.plot(posterior2) 