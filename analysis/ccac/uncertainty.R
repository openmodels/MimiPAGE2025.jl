setwd("~/research/iamup2/MimiPAGE2025.jl/analysis/ccac")

library(dplyr)

df <- read.csv("mainruns-mc/Baseline/AbatementCostsCO2_tc_totalcost_national.csv") %>%
    left_join(read.csv("mainruns-mc/IntegratedAction/AbatementCostsCO2_tc_totalcost_national.csv"),
              by=c('time', 'country', 'trialnum'), suffix=c('.base', '.inta'))

df2 <- df %>% filter(time <= 2100) %>% mutate(diff=tc_totalcost_national.base - tc_totalcost_national.inta) %>%
    group_by(time, trialnum) %>% summarize(diff=sum(diff)) %>%
    group_by(time) %>% summarize(mu=mean(diff), ci25=quantile(diff, .25), ci75=quantile(diff, .75))

df3 <- rbind(subset(df2, time %in% c(2030, 2040)) %>% summarize(time=2035, mu=mean(mu), ci25=mean(ci25), ci75=mean(ci75)),
             subset(df2, time %in% c(2050, 2100))) %>%
    mutate(ci25.factor=ci25 / mu, ci75.factor=ci75 / mu)

## Estimate uncertainty factors

library(readxl)

disunc <- read.csv("/Users/admin/Library/CloudStorage/GoogleDrive-jrising@udel.edu/My Drive/Research/CCAC/data/tabula-Etude incertitude.csv") %>% filter(Concentration..ug.m3 %in% c("OgTOTAL", "OgTOTAL ")) %>% select(!Concentration..ug.m3) %>% mutate(across(everything(), as.numeric))
disunc2 <- data.frame(conc=seq(5, 100, by=5), sigma=as.numeric(colMeans(disunc)))
sigmafunc <- splinefun(disunc2$conc, disunc2$sigma, method='monoH.FC')

concs <- read_excel("/Users/admin/Library/CloudStorage/GoogleDrive-jrising@udel.edu/My Drive/Research/CCAC/data/GAINS/PM25_sourcecontrib_GAINS_4letter_20260126.xlsx")
concs2 <- cbind(concs[, 1:3], total=rowSums(concs[, -1:-3]))
concs2$sigma <- sigmafunc(concs2$total)

sigma <- 1.41 # for 25 um
exp(-0.5 * log(sigma) / log(2)) / sigma^2
exp(-0.5 * log(sigma) / log(2)) * sigma^2

quantile(rlnorm(1e6, 1, sqrt(sigma)), c(0.025, 0.975))
