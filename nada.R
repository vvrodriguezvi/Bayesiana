modelo1 <- MCMClogit(pac_hos_~ Edad+tipo_ss_+modoViol+sexo_ , b0=0, B0=.001,
                     data=Datos,burnin=1000,mcmc=25000, marginal.likelihood = "Laplace")
modelo2 <- MCMClogit(pac_hos_~ Edad+tipo_ss_+modoViol, b0=0, B0=.001,
                     data=Datos,burnin=1000,mcmc=25000, marginal.likelihood = "Laplace")
modelo3 <- MCMClogit(pac_hos_~ tipo_ss_+modoViol+sexo_ , b0=0, B0=.001,
                     data=Datos,burnin=1000,mcmc=25000, marginal.likelihood = "Laplace")
modelo4 <- MCMClogit(pac_hos_~ tipo_ss_+modoViol , b0=0, B0=.001,
                     data=Datos,burnin=1000,mcmc=25000, marginal.likelihood = "Laplace")

BF = BayesFactor ( modelo1,modelo4 )
print(BF)
PostProbMod ( BF )
