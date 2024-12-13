setwd("~/research/iamup2/MimiPAGE2020.jl")

source("map.R")

library(dplyr)
library(ggplot2)
library(scales)

scc.map <- function(filepath, byline, suffix) {
    df <- read.csv(filepath)
    df2 <- df %>% filter(country != 'global') %>% group_by(country) %>% summarize(scc=median(scc))
    gp <- make.map(df2 %>% group_by(country) %>% summarize(scc=median(scc)),
                   'country', 'scc', paste0(byline, "\n\nSocial Cost of Carbon\n(2015 USD / t CO2)"))
    ggsave(paste0("output/figures/sccmap", suffix, ".pdf"), width=10, height=5.5)
}

scc.map("output/allscc.csv", "All Updates", "")

scc.map("output/allscc-2050.csv", "Pulse in 2050", "-2050")
scc.map("output/allscc-2100.csv", "Pulse in 2100", "-2100")
scc.map("output/allscc-downscale-pageice.csv", "PAGE-ICE\nDownscaling", "-downscale-pageice")
scc.map("output/allscc-mac-pageice.csv", "PAGE-ICE\nAbatement Costs", "-mac-pageice")
scc.map("output/allscc-marketdmg-constoffset.csv", "Constant Offset\nMarket Damages", "-marketdmg-constoffset")
scc.map("output/allscc-marketdmg-nooffset.csv", "National-level\nBurke et al.", "-marketdmg-nooffset")
scc.map("output/allscc-marketdmg-pageice.csv", "PAGE-ICE\nBurke et al.", "-marketdmg-pageice")
scc.map("output/allscc-nodrupp.csv", "Drupp et al.\nmedian preferences", "-nodrupp")
scc.map("output/allscc-otherdmg-pageice.csv", "PAGE-ICE\nOther Damages", "-otherdmg-pageice")
scc.map("output/allscc-otherdmg-pinational.csv", "National PAGE-ICE\nOther Damages", "-otherdmg-pinational")
scc.map("output/allscc-otherdmg-pinonmarket.csv", "PAGE-ICE\nNon-Market Damages", "-otherdmg-pinonmarket")
scc.map("output/allscc-otherdmg-pislr.csv", "PAGE-ICE\nSea-Level Damages", "-otherdmg-pislr")
scc.map("output/allscc-ssp126.csv", "SSP1-2.6 Scenario", "-ssp126")
scc.map("output/allscc-ssp245.csv", "SSP2-4.5 Scenario", "-ssp245")
scc.map("output/allscc-ssp585.csv", "SSP5-8.5 Scenario", "-ssp585")
scc.map("output/allscc-subnational.csv", "Subnational Effects", "-subnational")
