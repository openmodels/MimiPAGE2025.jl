setwd("~/research/iamup2/MimiPAGE2025.jl/analysis/ccac")

library(dplyr)
library(ggplot2)
source("helpers.R")

## Need to bias correct at the statistical level, because MCs don't match up

statag <- mean

df0.mc <- read.csv("mainruns-mc/Baseline/FaIRGrounds_rt_g_globaltemperature.csv") %>%
    group_by(trialnum) %>%
    reframe(year=2025:2100, gsat=splinefun(time, rt_g_globaltemperature, method='fmm')(year))
df0 <- df0.mc %>%
    group_by(year) %>% summarize(med=statag(gsat), ci05=quantile(gsat, .025), ci95=quantile(gsat, .975))

df1.mc <- read.csv("mainruns-mc/LTCAction/FaIRGrounds_rt_g_globaltemperature.csv") %>%
    group_by(trialnum) %>%
    reframe(year=2025:2100, gsat=splinefun(time, rt_g_globaltemperature, method='fmm')(year))
df1 <- df1.mc %>%
    group_by(year) %>% summarize(med=statag(gsat), ci05=quantile(gsat, .025), ci95=quantile(gsat, .975))

df2.mc <- read.csv("mainruns-mc/IntegratedAction/FaIRGrounds_rt_g_globaltemperature.csv") %>%
    group_by(trialnum) %>%
    reframe(year=2025:2100, gsat=splinefun(time, rt_g_globaltemperature, method='fmm')(year))
df2 <- df2.mc %>%
    group_by(year) %>% summarize(med=statag(gsat), ci05=quantile(gsat, .025), ci95=quantile(gsat, .975))

df1.bc <- df1 %>% filter(year == 2025) %>%
    left_join(df0 %>% filter(year == 2025), by='year', suffix=c('', '.baseline')) %>%
    mutate(bias.med=med.baseline - med, bias.ci05=ci05.baseline - ci05, bias.ci95=ci95.baseline - ci95)
df2.bc <- df2 %>% filter(year == 2025) %>%
    left_join(df0 %>% filter(year == 2025), by='year', suffix=c('', '.baseline')) %>%
    mutate(bias.med=med.baseline - med, bias.ci05=ci05.baseline - ci05, bias.ci95=ci95.baseline - ci95)

df <- rbind(cbind(scenario="Baseline", df0),
            cbind(scenario="Long-term Climate Action", df1 %>% mutate(med=med + df1.bc$bias.med, ci05=ci05 + df1.bc$bias.ci05, ci95=ci95 + df1.bc$bias.ci95)),
            cbind(scenario="Integrated Action", df2 %>% mutate(med=med + df2.bc$bias.med, ci05=ci05 + df2.bc$bias.ci05, ci95=ci95 + df2.bc$bias.ci95)))
df$scenario <- factor(df$scenario, levels=c("Baseline", "Long-term Climate Action", "Integrated Action"))

ggplot(df, aes(year, med, group=scenario)) +
    geom_ribbon(aes(ymin=ci05, ymax=ci95, alpha=scenario)) +
    geom_line(aes(y=med, colour=scenario), linewidth=1.5) +
    scale_x_continuous(NULL, expand=c(0, 0)) +
    theme_bw() + scale_colour_discrete(name="Scenario:", breaks=c("Baseline", "Long-term Climate Action", "Integrated Action", "",
                                                                  rev(c("SSP1-1.9", "SSP1-2.6", "SSP2-4.5", "SSP3-7.0")))) +
    scale_alpha_manual(name="Scenario:", breaks=c("Baseline", "Long-term Climate Action", "Integrated Action", "",
                                                  rev(c("SSP1-1.9", "SSP1-2.6", "SSP2-4.5", "SSP3-7.0"))), values=c(.25, 0, .5, 0, rep(1, 4))) +
    ylab("Global surface air temperature\n(change from pre-industrial, °C)")
myggsave("Figure 3.6 a GSAT - 95ci", width=6.5, height=4)

cmip6 <- data.frame(scenario=c("SSP1-1.9", "SSP1-2.6", "SSP2-4.5", "SSP3-7.0"),
                    med=c(1.4, 1.8, 2.7, 3.6),
                    ci05=c(1.0, 1.3, 2.1, 2.8), ci95=c(1.8, 2.4, 3.5, 4.6),
                    panel="CMIP6 Scenarios") # 2081 - 2100

ggplot(df %>% filter(year > 2080) %>% group_by(scenario) %>%
       summarize(med=statag(med), ci05=statag(ci05), ci95=statag(ci95)) %>%
       mutate(panel="This Report"),
       aes(med, scenario)) +
    facet_wrap(~ panel, ncol=1, scales='free_y', space='free_y') +
    geom_errorbar(aes(xmin=ci05, xmax=ci95)) +
    geom_point() +
    geom_errorbar(data=cmip6, aes(xmin=ci05, xmax=ci95)) +
    geom_point(data=cmip6) +
    ylab(NULL) +
    theme_bw() +
    xlab("Global surface air temperature\n(2081 - 2100 change from pre-industrial, °C)")
myggsave("Figure 3.6 a GSAT - annex", width=6.5, height=3)

## Save for table
df1.bc.mc <- df1.mc %>% filter(year == 2025) %>%
    left_join(df0.mc %>% filter(year == 2025), by=c('year', 'trialnum'), suffix=c('', '.baseline')) %>%
    mutate(bias=gsat.baseline - gsat)
df2.bc.mc <- df2.mc %>% filter(year == 2025) %>%
    left_join(df0.mc %>% filter(year == 2025), by=c('year', 'trialnum'), suffix=c('', '.baseline')) %>%
    mutate(bias=gsat.baseline - gsat)

df.gsat <- rbind(cbind(scenario="Baseline", df0.mc),
                 cbind(scenario="LTCAction", df1.mc %>% left_join(df1.bc.mc, by='trialnum', suffix=c('', '.bcmc')) %>%
                                             mutate(gsat=gsat + bias) %>% select(c(trialnum, year, gsat))),
                 cbind(scenario="IntegratedAction", df2.mc %>% left_join(df2.bc.mc, by='trialnum', suffix=c('', '.bcmc')) %>%
                                                    mutate(gsat=gsat + bias) %>% select(c(trialnum, year, gsat))))

## SLR

df0.mc <- read.csv("mainruns-mc/Baseline/SeaLevelRise_s_sealevel.csv") %>%
    group_by(trialnum) %>%
    reframe(year=2025:2100, slr=splinefun(time, s_sealevel, method='fmm')(year))
df0 <- df0.mc %>%
    group_by(year) %>% summarize(med=statag(slr), ci17=quantile(slr, .17), ci83=quantile(slr, .83))

df1.mc <- read.csv("mainruns-mc/LTCAction/SeaLevelRise_s_sealevel.csv") %>%
    group_by(trialnum) %>%
    reframe(year=2025:2100, slr=splinefun(time, s_sealevel, method='fmm')(year))
df1 <- df1.mc %>%
    group_by(year) %>% summarize(med=statag(slr), ci17=quantile(slr, .17), ci83=quantile(slr, .83))
df2.mc <- read.csv("mainruns-mc/IntegratedAction/SeaLevelRise_s_sealevel.csv") %>%
    group_by(trialnum) %>%
    reframe(year=2025:2100, slr=splinefun(time, s_sealevel, method='fmm')(year))
df2 <- df2.mc %>%
    group_by(year) %>% summarize(med=statag(slr), ci17=quantile(slr, .17), ci83=quantile(slr, .83))

df1.bc <- df1 %>% filter(year == 2025) %>%
    left_join(df0 %>% filter(year == 2025), by='year', suffix=c('', '.baseline')) %>%
    mutate(bias.med=med.baseline - med, bias.ci17=ci17.baseline - ci17, bias.ci83=ci83.baseline - ci83)
df2.bc <- df2 %>% filter(year == 2025) %>%
    left_join(df0 %>% filter(year == 2025), by='year', suffix=c('', '.baseline')) %>%
    mutate(bias.med=med.baseline - med, bias.ci17=ci17.baseline - ci17, bias.ci83=ci83.baseline - ci83)

df <- rbind(cbind(scenario="Baseline", df0),
            cbind(scenario="Long-term Climate Action", df1 %>% mutate(med=med + df1.bc$bias.med, ci17=ci17 + df1.bc$bias.ci17, ci83=ci83 + df1.bc$bias.ci83)),
            cbind(scenario="Integrated Action", df2 %>% mutate(med=med + df2.bc$bias.med, ci17=ci17 + df2.bc$bias.ci17, ci83=ci83 + df2.bc$bias.ci83)))
df$scenario <- factor(df$scenario, levels=c("Baseline", "Long-term Climate Action", "Integrated Action"))

bias.ssp119 <- df$med[df$year == 2030 & df$scenario == "Long-term Climate Action"] - 0.09
bias.ssp126 <- df$med[df$year == 2030 & df$scenario == "Integrated Action"] - 0.09
bias.ssp245 <- df$med[df$year == 2030 & df$scenario == "Baseline"] - 0.09 #0.10 <- should be 0.10 in 2030, but results in small bias in 2025.

dfb <- df %>% left_join(data.frame(scenario=c("Baseline", "Long-term Climate Action", "Integrated Action"),
                                   bias=c(bias.ssp119, bias.ssp126, bias.ssp245)), by='scenario')
dfb$scenario <- factor(dfb$scenario, levels=c("Baseline", "Long-term Climate Action", "Integrated Action"))

ggplot(dfb, aes(year, group=scenario)) +
    geom_ribbon(aes(ymin=ci17 - bias, ymax=ci83 - bias, alpha=scenario)) +
    geom_line(aes(y=med - bias, colour=scenario), linewidth=1.5) +
    scale_x_continuous(NULL, expand=c(0, 0)) +
    theme_bw() + scale_colour_discrete(name="Scenario:", breaks=c("Baseline", "Long-term Climate Action", "Integrated Action", "",
                                                                  rev(c("SSP1-1.9", "SSP1-2.6", "SSP2-4.5", "SSP3-7.0")))) +
    scale_alpha_manual(name="Scenario:", breaks=c("Baseline", "Long-term Climate Action", "Integrated Action", "",
                                                  rev(c("SSP1-1.9", "SSP1-2.6", "SSP2-4.5", "SSP3-7.0"))), values=c(.25, 0, .5, 0, rep(1, 4))) +
    ylab("Sea level rise (change from 1995 - 2014, m)")
myggsave("Figure 3.6 b SLR", width=6.5, height=4)

cmip6 <- data.frame(scenario=c("SSP1-1.9", "SSP1-2.6", "SSP2-4.5", "SSP3-7.0"),
                    med=c(0.38, 0.44, 0.56, 0.68) + 0.158,
                    ci17=c(0.28, 0.32, 0.44, 0.55) + 0.158, ci83=c(0.55, 0.62, 0.76, 0.90) + 0.158,
                    panel="CMIP6 Scenarios")

ggplot(dfb %>% filter(year > 2080) %>% group_by(scenario) %>%
       summarize(med=statag(med), ci17=statag(ci17), ci83=statag(ci83), bias=statag(bias)) %>%
       mutate(panel="This Report"),
       aes(med, scenario)) +
    facet_wrap(~ panel, ncol=1, scales='free_y', space='free_y') +
    geom_errorbar(aes(xmin=ci17 - bias, xmax=ci83 - bias)) +
    geom_point(aes(med - bias)) +
    geom_errorbar(data=cmip6, aes(xmin=ci17, xmax=ci83)) +
    geom_point(data=cmip6) +
    ylab(NULL) +
    theme_bw() +
    xlab("Sea level rise (2081 - 2100 change from 1995 - 2014, m)")
myggsave("Figure 3.6 b SLR - annex", width=6.5, height=3)

## Save for table
df1.bc.mc <- df1.mc %>% filter(year == 2025) %>%
    left_join(df0.mc %>% filter(year == 2025), by=c('year', 'trialnum'), suffix=c('', '.baseline')) %>%
    mutate(bias=slr.baseline - slr)
df2.bc.mc <- df2.mc %>% filter(year == 2025) %>%
    left_join(df0.mc %>% filter(year == 2025), by=c('year', 'trialnum'), suffix=c('', '.baseline')) %>%
    mutate(bias=slr.baseline - slr)

df.slr <- rbind(cbind(scenario="Baseline", df0.mc),
                cbind(scenario="LTCAction", df1.mc %>% left_join(df1.bc.mc, by='trialnum', suffix=c('', '.bcmc')) %>%
                                            mutate(slr=slr + bias) %>% select(c(trialnum, year, slr))),
                cbind(scenario="IntegratedAction", df2.mc %>% left_join(df2.bc.mc, by='trialnum', suffix=c('', '.bcmc')) %>%
                                                   mutate(slr=slr + bias) %>% select(c(trialnum, year, slr)))) %>%
    left_join(data.frame(scenario=c("Baseline", "LTCAction", "IntegratedAction"),
                         bias=c(bias.ssp119, bias.ssp126, bias.ssp245)), by='scenario') %>%
    mutate(slr=slr - bias) %>% select(!bias)

library(clipr)
write_clip(df.slr %>% group_by(scenario, year) %>%
           summarize(mu=mean(slr), ci17=quantile(slr, .17), ci83=quantile(slr, .83)) %>%
           filter(year %in% c(2050, 2100)))

df0 <- read.csv("mainruns-mc/Baseline/RegionTemperature_rtl_realizedtemperature_absolute.csv") %>%
    ## filter(time %in% c(2030, 2040, 2050, 2100)) %>%
    ## group_by(country, time=ifelse(time %in% c(2030, 2040), 2035, time), trialnum) %>%
    ## summarize(rtl_realizedtemperature_absolute=mean(rtl_realizedtemperature_absolute))
    group_by(country, trialnum) %>%
    reframe(year=seq(2025, 2100, by=5), rtl_realizedtemperature_absolute=splinefun(time, rtl_realizedtemperature_absolute, method='fmm')(year), time=year)
df02 <- df0 %>%
    filter(time %in% c(2035, 2050, 2100)) %>%
    r
left_join(df0 %>% filter(time == 2025) %>% select(!c(time, year)), by=c('country', 'trialnum'), suffix=c('', '.2025')) %>%
    mutate(rtl_realizedtemperature_absolute=rtl_realizedtemperature_absolute - rtl_realizedtemperature_absolute.2025)
df1 <- read.csv("mainruns-mc/IntegratedAction/RegionTemperature_rtl_realizedtemperature_absolute.csv") %>%
    ## filter(time %in% c(2030, 2040, 2050, 2100)) %>%
    ## group_by(country, time=ifelse(time %in% c(2030, 2040), 2035, time), trialnum) %>%
    ## summarize(rtl_realizedtemperature_absolute=mean(rtl_realizedtemperature_absolute))
    group_by(country, trialnum) %>%
    reframe(year=seq(2025, 2100, by=5), rtl_realizedtemperature_absolute=splinefun(time, rtl_realizedtemperature_absolute, method='fmm')(year), time=year)
df12 <- df1 %>%
    filter(time %in% c(2035, 2050, 2100)) %>%
    left_join(df1 %>% filter(time == 2025) %>% select(!c(time, year)), by=c('country', 'trialnum'), suffix=c('', '.2025')) %>%
    mutate(rtl_realizedtemperature_absolute=rtl_realizedtemperature_absolute - rtl_realizedtemperature_absolute.2025)

df <- rbind(cbind(scenario="Baseline", df02),
            cbind(scenario="Integrated Action", df12))

library(readxl)
gainsregions <- read.csv("../../data/pollution/GAINS_4letter_regions_mapping.csv")
wbregions <- read_excel("regiongroups_WB_global_flagship_2024.xlsx")

df2 <- df %>% group_by(trialnum, country, time) %>%
    summarize(dtemp=rtl_realizedtemperature_absolute[scenario == 'Baseline'] -
                  rtl_realizedtemperature_absolute[scenario == 'Integrated Action']) %>%
    left_join(gainsregions, by=c('country'='ISO3')) %>%
    left_join(wbregions, by='REGION_4LETTER') %>%
    group_by(trialnum, LABEL_REGIONGROUP, time) %>% summarize(dtemp=mean(dtemp)) %>%
    group_by(LABEL_REGIONGROUP, time) %>%
    summarize(med=statag(dtemp), ci05=quantile(dtemp, .025), ci95=quantile(dtemp, .975))

ggplot(subset(df2, !is.na(LABEL_REGIONGROUP)), aes(med, LABEL_REGIONGROUP)) +
    facet_wrap(~ time, space='free_x') +
    geom_col(fill='#d95f02') + geom_errorbar(aes(xmin=ci05, xmax=ci95), width=.5) +
    scale_fill_discrete(name="Scenario:") +
    theme_bw() + scale_x_continuous("Avoided temperature change (°C, relative to baseline)") +
    ylab(NULL)
myggsave("Figure 3.7 Changes in regional temperature - 95ci", width=6.5, height=4)
