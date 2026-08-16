setwd("~/research/iamup2/MimiPAGE2025.jl/analysis/ccac")

library(ggplot2)
library(dplyr)
library(tidyr)
library(readxl)

source("helpers.R")

get.row.helper <- function(prefix, scenarios=c('Baseline', 'LTCAction', 'IntegratedAction')) {
    info <- data.frame()
    for (scenario in scenarios) {
        df <- read.csv(file.path("mainruns", paste0(prefix, '-', scenario, '.csv')))
        if (names(df)[1] == 'time' & names(df)[2] == 'country') {
            names(df)[3] <- 'var'
        } else {
            names(df)[2] <- 'var'
        }

        info <- rbind(info, cbind(scenario=scenario, df))
    }
    info
}

pop <- get.row.helper('pop')
edr <- get.row.helper('cromar-mortality-edr')

df <- pop %>% left_join(edr, by=c('scenario', 'time', 'country'), suffix=c('.pop', '.edr'))
df$deaths <- df$var.pop * df$var.edr * 1e6 / 1000

df2 <- df %>% select(!c(var.pop, var.edr)) %>%
    group_by(scenario, country) %>%
        reframe(time2=sort(unique(c(time, 2035))), deaths=sapply(time2, function(tt) ifelse(tt %in% time, deaths[time == tt], (deaths[time == 2030] + deaths[time == 2040]) / 2))) %>%
        rename(time=time2)

df3 <- df2 %>% pivot_wider(names_from='scenario', values_from='deaths')

gainsregions <- read.csv("../../data/pollution/GAINS_4letter_regions_mapping.csv")
wbregions <- read_excel("regiongroups_WB_global_flagship_2024.xlsx")

df4 <- df3 %>% left_join(gainsregions, by=c('country'='ISO3')) %>%
    left_join(wbregions, by='REGION_4LETTER') %>%
    group_by(time, LABEL_REGIONGROUP) %>% reframe(saved=c(sum(Baseline - LTCAction, na.rm=T),
                                                          sum(LTCAction - IntegratedAction, na.rm=T)),
                                                  measures=c("Long-term climate solutions",
                                                             "Clean air and\nnear-term climate solutions"))

ggplot(subset(df4, time %in% c(2035, 2050, 2100) & !is.na(LABEL_REGIONGROUP)), aes(saved / 1e3, LABEL_REGIONGROUP)) +
    facet_wrap(~ time, space='free_x', scales='free_x') +
    geom_col(aes(fill=measures)) +
    scale_fill_manual(name="Measures:", breaks=c("Long-term climate solutions",
                                                 "Clean air and\nnear-term climate solutions"),
                      values=c('#33a02c', '#1f78b4')) +
    scale_x_continuous("Premature deaths avoided due to lower warming (1000 people / year)",
                       breaks=c(0, 15, 30, seq(0, 150, by=50)[-1]), expand=expansion(add=c(0, 15))) +
    theme_bw() + theme(panel.spacing=unit(0.8, "lines")) + ylab(NULL)
myggsave("Figure 3.3.4.1 Mortality avoided due to lower warming", width=7.5, height=4)

subset(df4, time %in% c(2050, 2100)) %>% group_by(time) %>% summarize(saved=sum(saved, na.rm=T))

## Deaths from delays
pop <- get.row.helper('pop', scenarios=c('IntegratedAction', 'IntegratedAction_delay'))
edr <- get.row.helper('cromar-mortality-edr', scenarios=c('IntegratedAction', 'IntegratedAction_delay'))

df <- pop %>% left_join(edr, by=c('scenario', 'time', 'country'), suffix=c('.pop', '.edr'))
df$deaths <- df$var.pop * df$var.edr * 1e6 / 1000

gainsregions <- read.csv("../../data/pollution/GAINS_4letter_regions_mapping.csv")
wbregions <- read_excel("regiongroups_WB_global_flagship_2024.xlsx")

df2 <- df %>% select(!c(var.pop, var.edr)) %>%
    group_by(country, time) %>%
    summarize(ddeaths=deaths[scenario == 'IntegratedAction_delay'] - deaths[scenario == 'IntegratedAction']) %>%
    left_join(gainsregions, by=c('country'='ISO3')) %>%
    left_join(wbregions, by='REGION_4LETTER') %>%
    group_by(LABEL_REGIONGROUP, time) %>%
    summarize(ddeaths=sum(ddeaths)) %>%
    group_by(LABEL_REGIONGROUP) %>%
    reframe(year=seq(2025, 2100, by=5), ddeaths=approxfun(c(2025, time), c(0, ddeaths))(year),
            cumdeaths=cumsum(ddeaths))

ggplot(subset(df2, !is.na(LABEL_REGIONGROUP)), aes(year, cumdeaths, fill=LABEL_REGIONGROUP)) +
    geom_col(colour='black', linewidth=.05) +
    ylab("Cumulative avoided deaths from removing delays") + xlab(NULL) +
    scale_fill_discrete(name="Region:") +
    theme_bw()
ggsave("livessaved-delay.pdf", width=8.5, height=5)

write.csv(subset(df2, !is.na(LABEL_REGIONGROUP)), "livessaved-delay.csv", row.names=F)

## Non-valuated labour productivity
pop <- get.row.helper('pop')
prod <- get.row.helper('dasgupta-labor-prod')

df <- pop %>% left_join(prod, by=c('scenario', 'time', 'country'), suffix=c('.pop', '.prod'))

df2 <- df %>%
    group_by(scenario, country) %>%
    reframe(time2=sort(unique(c(time, 2035))),
            pop=sapply(time2, function(tt) ifelse(tt %in% time, var.pop[time == tt], (var.pop[time == 2030] + var.pop[time == 2040]) / 2)),
            prod=sapply(time2, function(tt) ifelse(tt %in% time, var.prod[time == tt], (var.prod[time == 2030] + var.prod[time == 2040]) / 2))) %>%
        rename(time=time2)

df2 %>% group_by(scenario, time) %>% dplyr::summarize(prod=wtd.mean(prod, pop)) %>%
    group_by(time) %>% dplyr::summarize(dprod=prod[scenario == "Baseline"] - prod[scenario == "IntegratedAction"])

df3 <- df2 %>% pivot_wider(names_from='scenario', values_from=c('pop', 'prod'))

gainsregions <- read.csv("../../data/pollution/GAINS_4letter_regions_mapping.csv")
wbregions <- read_excel("regiongroups_WB_global_flagship_2024.xlsx")

library(Hmisc)

df4 <- df3 %>% left_join(gainsregions, by=c('country'='ISO3')) %>%
    left_join(wbregions, by='REGION_4LETTER') %>%
    group_by(time, LABEL_REGIONGROUP) %>% reframe(prod=c(wtd.mean(prod_Baseline - prod_LTCAction, pop_Baseline, na.rm=T),
                                                         wtd.mean(prod_LTCAction - prod_IntegratedAction, pop_Baseline, na.rm=T)),
                                                  measures=c("Long-term climate solutions",
                                                             "Clean air and\nnear-term climate solutions"))

ggplot(subset(df4, time %in% c(2035, 2050, 2100) & !is.na(LABEL_REGIONGROUP)), aes(-prod / 100, LABEL_REGIONGROUP)) +
    facet_wrap(~ time, space='free_x', scales='free_x') +
    geom_col(aes(fill=measures)) +
    scale_fill_manual(name="Measures:", breaks=c("Long-term climate solutions",
                                                 "Clean air and\nnear-term climate solutions"),
                      values=c('#33a02c', '#1f78b4')) +
    scale_x_continuous("Increase in effective productivity due to lower warming (%)",
                       breaks=c(0, .02, seq(.05, .2, by=.05)), expand=expansion(add=c(.01, .02)),
                       labels=scales::percent) +
    theme_bw() + theme(panel.spacing=unit(0.8, "lines")) + ylab(NULL)
myggsave("Figure 3.1.4.B Increased productivity due to lower warming", width=7.5, height=4)
