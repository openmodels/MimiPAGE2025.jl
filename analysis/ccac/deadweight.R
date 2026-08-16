setwd("~/research/iamup2/MimiPAGE2025.jl/analysis/ccac")

library(dplyr)
library(ggplot2)
library(readxl)

source("helpers.R")

sspgdp <- read_excel("/Users/admin/Library/CloudStorage/GoogleDrive-jrising@udel.edu/My Drive/Research/CCAC/data/GAINS/SSP2_macro_4letter_20260126.xlsx") %>%
    group_by(IDYEARS) %>% summarize(VALUE=sum(GDP_GUSD2017_PPP))
sspgdp$GDP2025 <- sspgdp$VALUE * 128.970 / 100

syscost <- read_excel("/Users/admin/Library/CloudStorage/GoogleDrive-jrising@udel.edu/My Drive/Research/CCAC/data/GAINS/MESSAGE_system_cost_ssp2.xlsx") %>%
    group_by(IDYEARS=YEAR, VARIABLE) %>%
    summarize(value2025.low=VALUE[SCENARIO == "INDC2030i_weak_SSP2 - Low Emissions_b"] * 128.970 / 89.629,
              value2025.med=VALUE[SCENARIO == "SSP2 - Medium Emissions"] * 128.970 / 89.629,
              value2025=value2025.low - value2025.med)

df <- syscost %>% left_join(sspgdp %>% select(!VALUE)) %>%
    filter(VARIABLE != "GDP|MER") %>%
    mutate(frac=ifelse(VARIABLE == "GDP|PPP", -value2025 / GDP2025, value2025 / GDP2025),
           frac.low=ifelse(VARIABLE == "GDP|PPP", -value2025.low / GDP2025 + 1, value2025.low / GDP2025),
           frac.med=ifelse(VARIABLE == "GDP|PPP", -value2025.med / GDP2025 + 1, value2025.med / GDP2025))

ggplot(df, aes(IDYEARS, frac, colour=VARIABLE)) +
    geom_hline(aes(yintercept=0)) +
    geom_line(linewidth=1) +
    theme_bw() +
    scale_colour_manual(name="Long-term climate action\ncost estimates", breaks=c("Investment", "Cost|Cost Nodal Net", "GDP|PPP"),
                        labels=c("Investment costs only", "All engineering costs", "Engineering costs and\ndeadweight losses"),
                        values=c('#7fc97f', '#beaed4', '#fdc086')) +
    xlab(NULL) + scale_y_continuous("Long-Term Climate Action Costs\n(% of global GDP, vs. Baseline)", labels=scales::percent)
myggsave("Figure 3.31 Various cost estimates", width=6.5, height=3.5)

ggplot(subset(df, IDYEARS <= 2100), aes(IDYEARS, colour=VARIABLE)) +
    geom_hline(aes(yintercept=0)) +
    geom_line(aes(y=frac.low, linetype="Low Emissions"), linewidth=1) +
    geom_line(aes(y=frac.med, linetype="Medium Emissions"), linewidth=1) +
    theme_bw() +
    scale_colour_manual(name="Long-term climate action\ncost estimates", breaks=c("Investment", "Cost|Cost Nodal Net", "GDP|PPP"),
                        labels=c("Investment costs only", "All engineering costs", "Engineering costs and\ndeadweight losses"),
                        values=c('#7fc97f', '#beaed4', '#fdc086')) +
    xlab(NULL) + scale_y_continuous("Long-Term Climate Action Costs\n(% of global GDP)", labels=scales::percent)
