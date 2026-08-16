setwd("~/research/iamup2/MimiPAGE2025.jl/analysis/ccac")

library(dplyr)
library(ggplot2)

df0 <- read.csv("mainruns/gsat-Baseline.csv") %>%
    reframe(year=2025:2100, gsat=splinefun(time, rt_g_globaltemperature, method='fmm')(year))
df1 <- read.csv("mainruns/gsat-IntegratedAction.csv") %>%
    reframe(year=2025:2100, gsat=splinefun(time, rt_g_globaltemperature, method='fmm')(year))

df <- rbind(cbind(scenario="Baseline", df0),
            cbind(scenario="Integrated Action", df1))

df0 <- read.csv("mainruns/pm-Baseline.csv") %>% left_join(read.csv("mainruns/pop-Baseline.csv")) %>%
    group_by(time) %>% summarize(pm_total=sum(pm_total * pop_population) / sum(pop_population)) %>%
    reframe(year=2025:2100, pm=splinefun(time, pm_total, method='fmm')(year))
df1 <- read.csv("mainruns/pm-IntegratedAction.csv") %>% left_join(read.csv("mainruns/pop-IntegratedAction.csv")) %>%
    group_by(time) %>% summarize(pm_total=sum(pm_total * pop_population) / sum(pop_population)) %>%
    reframe(year=2025:2100, pm=splinefun(time, pm_total, method='fmm')(year))

dfb <- rbind(cbind(scenario="Baseline", df0),
             cbind(scenario="Integrated Action", df1))

df.x <- df %>% group_by(year) %>% summarize(dgsat=gsat[scenario == "Baseline"] - gsat[scenario == "Integrated Action"]) %>% ungroup() %>% mutate(dgsat=dgsat / max(dgsat))
dfb.x <- dfb %>% group_by(year) %>% summarize(dpm=pm[scenario == "Baseline"] - pm[scenario == "Integrated Action"]) %>% ungroup() %>% mutate(dpm=dpm / max(dpm))

ggplot(df.x, aes(year, dgsat)) +
    coord_cartesian(xlim=c(2025, 2100), ylim=c(0, 1)) +
    geom_line(aes(colour="Climate Benefits"), linewidth=1.6) +
    geom_line(data=dfb.x, aes(year, dpm, colour="Pollution Benefits"), linewidth=1.6) +
    theme_bw() +
    scale_y_continuous("Benefits of Integrated Action (% of 2100 difference)", labels=scales::percent, expand=c(0, 0)) +
    scale_x_continuous(NULL, expand=c(0, 0)) +
    scale_colour_discrete(name="Integrated Action Effects:")
