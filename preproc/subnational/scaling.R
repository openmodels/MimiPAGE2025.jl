setwd("~/research/iamup2/MimiPAGE2020.jl/preproc/subnational")

library(readxl)
library(MASS)
library(dplyr)
library(ggplot2)

icc <- read_xlsx("../burkey/INFORM_Risk_2023__v065.xlsx", sheet=4)
icc2 <- icc[-1:-3, c('...1', '...2', 'Gini Index', 'INFORM Vulnerable Groups')]
icc2$`Gini Index` <- (65 - 25) * as.numeric(icc2$`Gini Index`) / 10 + 25
icc2$`INFORM Vulnerable Groups` <- as.numeric(icc2$`INFORM Vulnerable Groups`) / 10
names(icc2)[1:2] <- c('Country', 'ISO3')

## Estimate relationship between vulnerability and income
bycountry <- read.csv("../../data/bycountry.csv")
bycountry2 <- bycountry %>% left_join(icc2, by='ISO3')
bycountry2$gdppc.rank <- rank(bycountry2$GDP2015 / bycountry2$Pop2015) / nrow(bycountry2)

ggplot(bycountry2, aes(gdppc.rank, `INFORM Vulnerable Groups`)) +
    geom_point() +
    xlab("2015 GDP per capita (normalized rank)") + ylab("INFORM Vulnerability Groups (normalized rank)") +
    theme_bw()
ggsave("subnational-rho.pdf", width=5, height=5)

rho <- cor(bycountry2$gdppc.rank, bycountry2$`INFORM Vulnerable Groups`, use='complete')

prefs <- read.csv("../../data/preferences/druppetal2018.csv")

rhos <- sapply(1:nrow(prefs), function(ii) {
    ii <- sample(1:nrow(bycountry2), nrow(bycountry2), replace=T)
    cor(bycountry2$gdppc.rank[ii], bycountry2$`INFORM Vulnerable Groups`[ii], use='complete')
})

ggplot(data.frame(rhos), aes(rhos)) +
    geom_histogram() + theme_bw() +
    xlab("Bootstrapped Estimates of Rho")
ggsave("subnational-rho2.pdf", width=5, height=5)

sigma.c <- sqrt(2) * qnorm((icc2$`Gini Index` / 100 + 1) / 2)
sigma.c[is.na(sigma.c)] <- mean(sigma.c, na.rm=T)
deltamu.c <- -(sigma.c^2)/2

sigma.v <- icc2$`INFORM Vulnerable Groups`
deltamu.v <- -(sigma.v^2)/2

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
    print(icc2$Country[ii])
    ## mean(exp(rnorm(1e6, deltamu.c[ii], sigma.c[ii])))
    ## mean(exp(rnorm(1e6, deltamu.v[ii], sigma.v[ii])))
    for (kk in 1:nrow(prefs)) {
        bigsigma <- diag(c(sigma.c[ii], sigma.v[ii])) %*% matrix(c(1, rhos[kk], rhos[kk], 1), 2, 2) %*% diag(c(sigma.c[ii], sigma.v[ii]))
        ccvv <- exp(mvrnorm(1e5, c(deltamu.c[ii], deltamu.v[ii]), bigsigma))
        ## ccvv2 <- ccvv[, 1] / ccvv[, 2]

        ## effect <- cc.isoelastic(mean(sapply(ccvv2, function(cv) uu.isoelastic(cv, prefs$eta[kk]))), prefs$eta[kk])
        effect <- mean(ccvv[, 2] * uu.raise(ccvv[, 1], 1 - prefs$eta[kk])) / mean(uu.raise(ccvv[, 1], 1 - prefs$eta[kk]))
        results <- rbind(results, data.frame(country=icc2$Country[ii], iso=icc2$ISO3[ii], pref=kk, eta=prefs$eta[kk], effect))
    }
}

results2 <- results %>% group_by(country, iso) %>% summarize(mu=mean(effect), sd=sd(effect))
results2[order(results2$mu),]

write.csv(results, "../../data/damages/subnational.csv", row.names=F)

setwd("~/research/iamup2/MimiPAGE2020.jl/preproc/subnational")

results <- read.csv("../../data/damages/subnational.csv")

source("../../analysis/map.R", chdir=T)

make.map(results %>% group_by(iso) %>% summarize(mu=mean(effect)), 'iso', 'mu',
         "Sub-national equity coefficient", trans='identity')
ggsave("subnational-scaling.png", width=10, height=5.5)
