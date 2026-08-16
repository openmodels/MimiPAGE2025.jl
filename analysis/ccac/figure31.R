setwd("~/research/iamup2/MimiPAGE2025.jl/analysis/ccac")

library(ggplot2)

do.global <- T
source("loader-mc.R")
source("helpers.R")

gdps <- rbind(cbind(name="Baseline", read.csv("mainruns-mc/Baseline/GDP_gdp.csv")),
              cbind(name="IntegratedAction", read.csv("mainruns-mc/IntegratedAction/GDP_gdp.csv"))) %>%
    group_by(name, time, trialnum) %>% summarize(gdp=sum(gdp))

pdf2 <- subset(pdf, panel == "Aggregate" & prefix %in% c('MarketDamagesBurke_isat_per_cap_ImpactperCapinclSaturationandAdaptation', 'SLRDamages_d_slr', 'TotalAbatementCosts_tct_percap_totalcostspercap', 'TotalAdaptationCosts_act_percap_adaptationcosts', 'Discontinuity_isat_per_cap_DiscImpactperCapinclSaturation', 'WBRegionCorrection_infrastructure_cost', 'capitalloss', 'apcosts', 'pm-market') &
                    name %in% c("Baseline", "IntegratedAction")) %>%
    group_by(name, time, trialnum) %>% summarize(cost=sum(cost, na.rm=T))

pdf3 <- gdps %>% left_join(pdf2, by=c('name', 'time', 'trialnum')) %>%
    mutate(dmg=cost / (gdp * 1e6)) %>% group_by(name, trialnum) %>%
    reframe(year=seq(2025, 2100, by=5), dmg=splinefun(time, dmg, method='fmm')(year))

pdf4 <- pdf3 %>% left_join(sspgdp[, c('YEAR', 'value2025')], by=c('year'='YEAR')) %>%
    mutate(dmg2025=dmg * value2025,
           aft2025=value2025 - dmg2025) %>% group_by(name, year) %>%
    reframe(metric=c('Before', 'After'), mu=c(mean(value2025, na.rm=T), mean(aft2025, na.rm=T)),
            ci05=c(quantile(value2025, .05, na.rm=T), quantile(aft2025, .05, na.rm=T)),
            ci95=c(quantile(value2025, .95, na.rm=T), quantile(aft2025, .95, na.rm=T))) %>%
    filter(!is.na(mu))

ggplot(pdf4, aes(year, mu / 1e3)) +
    geom_line(data=pdf4 %>% filter(name == 'Baseline' & metric == 'Before'),
              aes(linetype="Before Costs", colour="Before Costs")) +
    geom_line(data=pdf4 %>% filter(metric == 'After'), aes(colour=name, linetype=name)) +
    ## geom_ribbon(data=pdf4 %>% filter(metric == 'After'), aes(ymin=ci05 / 1e3, ymax=ci95 / 1e3, group=name), alpha=.5) +
    theme_bw() +
    scale_linetype_manual(name="Scenario:", breaks=c("Before Costs", "Baseline", "IntegratedAction"),
                          labels=c("Before Costs", "Baseline", "Integrated Action"),
                          values=c('dashed', 'solid', 'solid')) +
    scale_colour_discrete(name="Scenario:", breaks=c("Before Costs", "Baseline", "IntegratedAction"),
                          labels=c("Before Costs", "Baseline", "Integrated Action")) +
    scale_x_continuous(NULL, expand=c(0, 0)) +
    ylab("Global GDP, PPP (trillion 2025 USD)")
myggsave("Figure ‎3.24 Global GDP to 2100", width=6.5, height=4)


gdp.2015.2025 <- 100 / 75.4588

gdps <- rbind(cbind(name="Baseline", read.csv("mainruns-mc/Baseline/GDP_gdp.csv")),
              cbind(name="IntegratedAction", read.csv("mainruns-mc/IntegratedAction/GDP_gdp.csv"))) %>%
    group_by(name, time, trialnum) %>% summarize(gdp=sum(gdp))

gdps %>% group_by(name, time) %>% summarize(mu=mean(gdp), ci05=quantile(gdp, .05), ci95=quantile(gdp, .95))

mcinfo <- data.frame(prefix=c("TotalAbatementCosts_tct_percap_totalcostspercap.csv", "TotalAdaptationCosts_act_percap_adaptationcosts.csv", "Discontinuity_isat_per_cap_DiscImpactperCapinclSaturation.csv", "MarketDamagesBurke_isat_per_cap_ImpactperCapinclSaturationandAdaptation.csv", "NonMarketDamages_isat_per_cap_ImpactperCapinclSaturationandAdaptation.csv", "PM25Damage_Healthcare_cost", "PM25Damage_Productivity_cost", "PM25Damage_Disutility_cost", "PM25Damage_Mortality_cost", 'Gains_cost', "SLRDamages_d_slr", 'Capital_gdp_capital'),
                     units=...,
                     category=c(rep('Climate', 5), rep('Pollution', 5), rep('Climate', 2)),
                     welfare=c(rep('Market', 4), 'Non-Market', rep('Market', 2), rep('Non-Market', 2), rep('Market', 3)))

agcost <- data.frame()
for (ii in 1:nrow(mcinfo)) {
    if (mcinfo$prefix[ii] == 'Gains_cost') {
        apcosts <- load.apcosts(F)
        ...
    } else if (mcinfo$prefix[ii] == 'Gains_cost') {
        ## Calc difference from GDP
    }
}


gdps <- rbind(cbind(name="Baseline", read.csv("mainruns/gdp-Baseline.csv")),
              cbind(name="IntegratedAction", read.csv("mainruns/gdp-IntegratedAction.csv"))) %>%
    group_by(name, time) %>% summarize(gdp=sum(gdp))

pdf2 <- subset(pdf, panel == "Aggregate" & prefix %in% c('prefix', 'd_slr', 'abateall-tcpc', 'abateall-tcpc', 'discontinuity-percap', 'capitalloss') &
                    name %in% c("Baseline", "IntegratedAction")) %>%
    group_by(name, time) %>% summarize(cost=sum(cost, na.rm=T))

pdf3 <- gdps %>% group_by(name, time) %>% summarize(gdp=sum(gdp)) %>%
    left_join(pdf2, by=c('name', 'time'))


ggplot(pdf3 %>% filter(time <= 2100), aes(time, gdp.2015.2025 * gdp / 1e6, colour=name, linetype=name)) +
    geom_line(data=pdf3 %>% filter(time <= 2100 & name == 'Baseline'), aes(linetype="Before Costs", colour="Before Costs")) +
    geom_line(aes(y=gdp.2015.2025 * (gdp / 1e6 - cost / 1e12))) +
    theme_bw() +
    scale_linetype_manual(name="Scenario:", breaks=c("Before Costs", "Baseline", "IntegratedAction"),
                          labels=c("Before Costs", "Baseline", "Integrated Action"),
                          values=c('dashed', 'solid', 'solid')) +
    scale_colour_discrete(name="Scenario:", breaks=c("Before Costs", "Baseline", "IntegratedAction"),
                          labels=c("Before Costs", "Baseline", "Integrated Action")) +
    scale_x_continuous(NULL, expand=c(0, 0)) +
    ylab("Global GDP (trillion 2025 USD)")
ggsave("figure31.png", width=6.5, height=4)

subset(pdf3, time %in% c(2050, 2100)) %>% mutate(remain=gdp.2015.2025 * (gdp / 1e6 - cost / 1e12),
                                                 gdp=gdp.2015.2025 * gdp / 1e6,
                                                 fracloss=cost / (gdp * 1e12))
