setwd("~/research/iamup2/MimiPAGE2025.jl/analysis/ccac")

library(dplyr)
library(tidyr)
library(ggplot2)
library(ggforce)
library(ggpattern)
library(readxl)

do.global <- T
source("loader.R")

pdf2 <- rbind(scen.diff.fracgdp("Decarb_CLE", "Baseline_CLE") %>% filter(prefix == 'd_slr') %>% mutate(name="Decarbonization"),
              scen.diff.fracgdp("Decarb_APC", "Decarb_CLE") %>% filter(prefix == 'd_slr') %>% mutate(name="SLCP Measures"))
pdf3 <- pdf2 %>% group_by(name) %>% reframe(year=seq(2025, 2100, by=5), diff=approxfun(time, diff)(year))

ggplot(pdf3, aes(year, diff, fill=name)) +
    geom_col() +
    scale_fill_discrete(name="Policy Category:") +
    theme_bw() + scale_y_continuous("Potential avoided damages (% Global GDP)", labels=scales::percent) +
    xlab(NULL)
ggsave(paste0("timeseries-d_slr.pdf"), width=6.5, height=4)
