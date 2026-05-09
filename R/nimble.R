library(tidyverse)
library(nimble)


ads <- read.csv("/data/Advertising_Budget_and_Sales.csv")
y_values <- ads$Sales....
nomes <- c("X","TV","Radio","Jornal","Vendas")
view(ads)

X <- matrix(nrow = 200, ncol = 4)
X[,1] <- 1
X[,2] <- ads$TV.Ad.Budget....
X[,3] <- ads$Radio.Ad.Budget....
X[,4] <- ads$Newspaper.Ad.Budget....
  
names(ads) <- nomes
# NIMBLE ----

modelo_codigo <- nimbleCode({
  
  # --- Prioris ---
  for(j in 1:P_plus_1) {
    beta[j] ~ dnorm(mean = M[j], sd = sqrt(V[j] * sigma2))
  }
  sigma2 ~ dinvgamma(shape = a, scale = d)
  
  # --- Verossimilhança ---
  for (i in 1:N) {

    # inprod = Beta_i * X[,i]
    mu[i] <- inprod(beta[1:P_plus_1], X[i, 1:P_plus_1])
    y[i] ~ dnorm(mu[i], sd = sqrt(sigma2))
  }
  
})


modelo_constantes <- list(
  M = rep(0,4),
  P_plus_1 = 4,
  V = rep(100^2,4),
  a = 1/1000,
  d = 1/1000,
  N = length(y_values),
  X = X
)

modelo_inits <- list(
  inits1 = list(beta = runif(4,-10,10), sigma2 = 1),
  inits2 = list(beta = runif(4,-50,50), sigma2 = 99)
)

modelo_dados <- list(
  y = y_values
)

modelo <- nimbleModel(modelo_codigo, modelo_constantes, modelo_dados, modelo_inits)

modelo$getNodeNames()

MCMCconf <- configureMCMC(modelo)

MCMCconf$addMonitors("beta") 

# Se quiser conferir o que ele vai salvar, rode:
MCMCconf$getMonitors() 
# A saída deve mostrar "beta" e "sigma2"

# Daqui para baixo o código continua igual:
modelo_MCMC <- buildMCMC(MCMCconf)

Cmodelo <- compileNimble(modelo)
Cmodelo_MCMC <- compileNimble(modelo_MCMC, project = modelo)

modelo_res <- runMCMC(Cmodelo_MCMC, niter = 10000,
                      inits = modelo_inits,
                      samplesAsCodaMCMC = TRUE,
                      setSeed = c(2312312*1,123332*2),
                      nchains=2,
                      nburnin=1000)
plot(modelo_res)

df_cadeia1 <- as.data.frame(modelo_res[[1]])
df_cadeia2 <- as.data.frame(modelo_res[[2]])

ggplot() +
  geom_line(data = df_cadeia1, aes(y = `beta[1]`, x = 1:9000)) +
  geom_line(data = df_cadeia2, aes(y = `beta[1]`, x = 1:9000), color = 'red') +
  labs(x = 'Iteração', y = expression(beta[1])) +
  scale_x_continuous(breaks = seq(0, 10000, by = 1000)) +
  theme_bw()

modelo_res[[1]] %>% 
  ggplot()+
  geom_line(aes(y = sigma2, x = 1:9000))+
  geom_line(aes(y = sigma2, x = 1:9000),
            data = modelo_res[[2]], color = 'red')+
  labs(x = 'iteração')+
  scale_x_continuous(breaks = seq(0,10000,by=1000))+
  theme_bw()

