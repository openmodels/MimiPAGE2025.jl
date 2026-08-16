setwd("~/research/iamup2/MimiPAGE2025.jl/analysis/ccac")

library(dplyr)
library(readxl)

## Compare final to raw inputs

gdp <- read.csv("mainruns/gdp-Baseline.csv") %>% group_by(time) %>% summarize(gdp=sum(gdp)) # million US\$2005/yr
gdp$PAGE2025 <- gdp$gdp * (100 / 65.0) / 1e3

sspgdp <- read_excel("/Users/admin/Library/CloudStorage/GoogleDrive-jrising@udel.edu/My Drive/Research/CCAC/data/GAINS/MESSAGE_system_cost_ssp2.xlsx") %>% filter(VARIABLE == "GDP|PPP" & SCENARIO == "SSP2 - Medium Emissions") # billion US$2010/yr
sspgdp$SSP2025 <- sspgdp$VALUE * 128.977 / 89.629

gdp %>% left_join(sspgdp, by=c('time'='YEAR')) %>% mutate(ratio=PAGE2025 / SSP2025)
## 4% higher, because of missing countries in MESSAGE?

base <- read.csv("mainruns/abateall-tct-Baseline.csv") %>% filter(time %in% c(2030, 2040)) %>% group_by(time) %>% summarize(tot=sum(tct_totalcosts)) # %>% ungroup() %>% summarize(tot=mean(tot))
inta <- read.csv("mainruns/abateall-tct-IntegratedAction.csv") %>% filter(time %in% c(2030, 2040)) %>% group_by(time) %>% summarize(tot=sum(tct_totalcosts)) # %>% ungroup() %>% summarize(tot=mean(tot))

(inta - base) / 1e3

base <- read.csv("mainruns/abateall-tct-Baseline.csv") %>% filter(time == 2050) %>% summarize(tot=sum(tct_totalcosts))
inta <- read.csv("mainruns/abateall-tct-IntegratedAction.csv") %>% filter(time == 2050) %>% summarize(tot=sum(tct_totalcosts))

(inta - base) / 1e3

base <- read.csv("mainruns/abateall-tct-Baseline.csv") %>% filter(time == 2100) %>% summarize(tot=sum(tct_totalcosts))
inta <- read.csv("mainruns/abateall-tct-IntegratedAction.csv") %>% filter(time == 2100) %>% summarize(tot=sum(tct_totalcosts))

(inta - base) / 1e3
