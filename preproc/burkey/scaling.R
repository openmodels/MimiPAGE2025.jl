setwd("~/research/iamup2/MimiPAGE2020.jl/preproc/burkey")

library(readxl)
library(MASS)
library(dplyr)

icc <- read_xlsx("INFORM_Risk_2023__v065.xlsx", sheet=4)
icc2 <- icc[-1:-3, c('...1', '...2', 'Gini Index', 'INFORM Vulnerable Groups')]
icc2$`Gini Index` <- (65 - 25) * as.numeric(icc2$`Gini Index`) / 10 + 25
icc2$`INFORM Vulnerable Groups` <- as.numeric(icc2$`INFORM Vulnerable Groups`) / 10

sigma.c <- sqrt(2) * qnorm((icc2$`Gini Index` / 100 + 1) / 2)
sigma.c[is.na(sigma.c)] <- mean(sigma.c, na.rm=T)
deltamu.c <- -(sigma.c^2)/2

sigma.v <- icc2$`INFORM Vulnerable Groups`
deltamu.v <- -(sigma.v^2)/2

prefs <- read.csv("../../data/preferences/druppetal2018.csv")

rho <- -.5

## uu.isoelastic <- function(cc, eta) {
##     if (eta == 1)
##         log(cc)
##     else
##         (cc^(1 - eta) - 1) / (1 - eta)
## }
## cc.isoelastic <- function(uu, eta) {
##     if (eta == 1)
##         exp(uu)
##     else
##         (uu * (1 - eta) + 1)^(1 / (1 - eta))
## }

uu.raise <- function(xx, eta) {
    if (eta == 1)
        xx^(.01) + xx^(-.01)
    else
        xx^(eta - 1)
}

results <- data.frame()
for (ii in 1:nrow(icc2)) {
    print(icc2$...1[ii])
    ## mean(exp(rnorm(1e6, deltamu.c[ii], sigma.c[ii])))
    ## mean(exp(rnorm(1e6, deltamu.v[ii], sigma.v[ii])))
    bigsigma <- diag(c(sigma.c[ii], sigma.v[ii])) %*% matrix(c(1, rho, rho, 1), 2, 2) %*% diag(c(sigma.c[ii], sigma.v[ii]))
    ccvv <- exp(mvrnorm(1e5, c(deltamu.c[ii], deltamu.v[ii]), bigsigma))
    ## ccvv2 <- ccvv[, 1] / ccvv[, 2]
    for (kk in 1:nrow(prefs)) {
        ## effect <- cc.isoelastic(mean(sapply(ccvv2, function(cv) uu.isoelastic(cv, prefs$eta[kk]))), prefs$eta[kk])
        effect <- mean(ccvv[, 2] * uu.raise(ccvv[, 1], 1 - prefs$eta[kk])) / mean(uu.raise(ccvv[, 1], 1 - prefs$eta[kk]))
        results <- rbind(results, data.frame(country=icc2$...1[ii], iso=icc2$...2[ii], pref=kk, eta=prefs$eta[kk], effect))
    }
}

results2 <- results %>% group_by(country, iso) %>% summarize(mu=mean(effect), sd=sd(effect))
results2[order(results2$mu),]

write.csv(results, "../../data/damages/subnational.csv", row.names=F)

setwd("~/research/iamup2/MimiPAGE2020.jl/preproc/burkey")

library(ggplot2)

results <- read.csv("../../data/damages/subnational.csv")

source("../../analysis/map.R", chdir=T)

make.map(results %>% group_by(iso) %>% summarize(mu=mean(effect)), 'iso', 'mu',
         "Sub-national equity coefficient", trans='identity')
ggsave("subnational-scaling.png", width=10, height=5.5)
