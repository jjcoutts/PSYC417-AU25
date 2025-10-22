# Central Limit Theorem illustration
# PSYC417
# R script created by Jacob J. Coutts

# load required packages 
library(ggplot2)
library(jtools)

# set seed to reproduce results
set.seed(100)

# create vectors to store results
cltpois = rep(NA,tot)
cltbin = rep(NA,tot)
cltnorm = rep(NA,tot)
cltuni = rep(NA,tot)

# set number of draws
tot=5000

# illustrate what one sample mean looks like
hist(rpois(1000,lambda = 1))
hist(rbinom(15,100,.05))
hist(rnorm(1000))
hist(runif(500))

# sample from distirbutions 5,000 times, computer mean of each
for(i in 1:tot){
  cltpois[i] <- mean(rpois(1000,lambda = 1))
  cltbin[i] <- mean(rbinom(15,100,.05))
  cltnorm[i] <- mean(rnorm(1000))
  cltuni[i] <- mean(runif(500))
}
# fancy ggplot
sim = data.frame(distribution = c(rep("Poisson", tot), rep("Binomial",tot),rep("Normal",tot),rep("Uniform",tot)), mdist = c(cltpois,cltbin,cltnorm,cltuni))
ggplot(sim, aes(x=mdist)) + geom_histogram() + facet_grid(.~distribution, scales="free_x") + jtools::theme_apa()

# less fancy 
hist(cltpois)
hist(cltbin)
hist(cltnorm)
hist(cltuni)

### end of script