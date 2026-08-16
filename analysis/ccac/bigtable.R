setwd("~/research/iamup2/MimiPAGE2025.jl/analysis/ccac")

library(dplyr)
library(tidyr)
library(ggplot2)
library(ggforce)
library(ggpattern)
library(readxl)

do.global <- F
source("loader.R")

pdf2 <- pdf %>%
    left_join(gdps, by=c('time', 'name', 'country')) %>%
    left_join(gainsregions, by=c('country'='ISO3')) %>%
    left_join(wbregions, by='REGION_4LETTER') %>%
    group_by(name, prefix, LABEL_REGIONGROUP, time) %>%
    summarize(cost=sum(cost)) %>%
    filter(name %in% c("Baseline", "LTCAction", "IntegratedAction", "IntegratedAction_delay",
                       "Baseline_nofeedback", "IntegratedAction_nofeedback"))

## Replace labor productivity using pop-weighted median (since log-effects can get extreme)
library(Hmisc)
wtd.median <- function(xx, ww) {
    if (length(xx) > 2)
        wtd.quantile(xx, ww, .5)
    else
        mean(xx)
}

pdf2.labor <- pdf %>%
    left_join(gdps, by=c('time', 'name', 'country')) %>%
    left_join(gainsregions, by=c('country'='ISO3')) %>%
    left_join(wbregions, by='REGION_4LETTER') %>%
    filter(time %in% c(2035, 2050, 2100) & prefix == 'dasgupta-labor-dmg' &
           name %in% c("Baseline", "LTCAction", "IntegratedAction", "IntegratedAction_delay",
                       "Baseline_nofeedback", "IntegratedAction_nofeedback")) %>%
    mutate(ratio=pmax(cost / (gdp * 1e6), -.1)) %>%
    left_join(read.csv(paste0(outdir, "/pop-Baseline.csv")) %>% group_by(country) %>%
        reframe(time2=sort(unique(c(time, 2035))), pop=sapply(time2, function(tt) ifelse(tt %in% time, pop_population[time == tt], (pop_population[time == 2030] + pop_population[time == 2040]) / 2))) %>%
        rename(time=time2)) %>%
    group_by(name, prefix, LABEL_REGIONGROUP, time) %>%
    dplyr::summarize(gdp=sum(gdp), ratio=wtd.median(ratio, pop), cost=gdp * ratio * 1e6) %>% dplyr::select(!c(gdp, ratio))

pdf3 <- rbind(pdf2 %>% filter(prefix != 'dasgupta-labor-dmg'),
              pdf2.labor) %>% filter(time %in% c(2035, 2050, 2100)) %>%
    left_join(infomap, by='prefix')
pdf3$panel <- ifelse(pdf3$prefix %in% c("abateco2-tc", "pmmarket-percchg"), "Other",
              ifelse(pdf3$prefix %in% c("abateall-tcpc", "adaptall-acpc", "discontinuity-percap", "d_market", "d_nonmarket", 'pm-nonmarket', 'pm-market', 'apcosts', "d_slr", 'capitalloss'),
                     'Aggregate', 'Bottom-up'))

pdf3$`Is Market` <- pdf3$prefix %in% c('MarketDamageAQ_AsthmaERVisits', 'MarketDamageAQ_CropLoss', 'MarketDamageAQ_LostWorkHours', 'MarketDamageAQ_RespiratoryAdmissions',
                                       'WBRC-morb_healthcare_new', 'WBRC-morb_productivity_new', 'WBRC-mort_productivity_new', 'abateall-tcpc', 'abateco2-tc', 'adaptall-acpc', 'apcosts', 'capitalloss',
                                       'd_market', 'd_slr', 'discontinuity-percap', 'pm-market', 'pmmarket-percchg',
                                       'WBRC-infrastructure_cost', 'dasgupta-labor-dmg')
nicenames <- list('dasgupta-labor-dmg'="Productivity Loss (Dasgupta et al.)",
                  'MarketDamageAQ_AsthmaERVisits'="Asthma ER Visits (GMA)",
                  'MarketDamageAQ_CropLoss'="Crop Loss (GMA)",
                  'MarketDamageAQ_LostWorkHours'="Lost Work Hours (GMA)",
                  'MarketDamageAQ_RespiratoryAdmissions'="Respiratory Admissions (GMA)",
                  'WBRC-infrastructure_cost'="Infrastructure Costs (IIASA)",
                  'WBRC-morb_disutility_new'="Morbidity Disutility (GAINS)",
                  'WBRC-morb_healthcare_new'="Healthcare Costs (GAINS)",
                  'WBRC-morb_productivity_new'="Morbidity Productivity Loss (GAINS)",
                  'WBRC-mort_disutility_new'="Mortality Risk Disutility (GAINS)",
                  'WBRC-mort_productivity_new'="Mortality Productivity Loss (GAINS)",
                  'abateall-tcpc'="Total Abatement Costs (MESSAGE)",
                  'abateco2-tc'="CO2 Abatement Costs (NGFS)",
                  'adaptall-acpc'="Adaptation Costs (PAGE)",
                  'apcosts'="Air Pollution Costs (GAINS)",
                  'capitalloss'="Capital Persistence Loss (PAGE)",
                  'cromar-mortality'="Mortality Risk Disutility (Cromar et al.)",
                  'd_market'="Aggregate Market Damages (Burke et al.+)",
                  'd_nonmarket'="Aggregate Non-Market Damages (Howard & Sterner+)",
                  'd_slr'="Sea-Level Rise Damages (pyCIAM+)",
                  'discontinuity-percap'="Discontinuity Risk (PAGE)",
                  'pm-market'="Aggregate Market Damages (GAINS)",
                  'pm-nonmarket'="Aggregate Non-Market Damages (GAINS)",
                  'pmmarket-percchg'="Aggregate Market Damages (Dechezleprêtre & Vienne)")
pdf3$Impact <- sapply(pdf3$prefix, function(p) nicenames[[p]])
nicenames2 <- list("Baseline"="Baseline",
                  "Baseline_nofeedback"="Baseline without feedback",
                  "IntegratedAction"="Integrated Action",
                  "IntegratedAction_delay"="Delayed Action",
                  "IntegratedAction_nofeedback"="Integrated Action without feedback",
                  "LTCAction"="Long-term Climate Action")
pdf3$Scenario <- sapply(pdf3$name, function(p) nicenames2[[p]])
names(pdf3)[names(pdf3) == "LABEL_REGIONGROUP"] <- "Region"
pdf3$`Cost (million 2025 USD)` <- pdf3$cost * (100 / 63.23579) / 1e6
names(pdf3)[names(pdf3) == "time"] <- "Year"

write.csv(pdf3[!is.na(pdf3$Region), c('Scenario', 'Impact', 'Region', 'Cost (million 2025 USD)', 'Year', 'channel', 'panel', 'Is Market')], "bigtable.csv", row.names=F)

as.data.frame(pdf3 %>% group_by(Impact, Region, Year, channel, panel, `Is Market`) %>%
    dplyr::summarize(diff=`Cost (million 2025 USD)`[Scenario == "Baseline"] - `Cost (million 2025 USD)`[Scenario == "Integrated Action"]) %>%
    filter(Impact == "Productivity Loss (Dasgupta et al.)"))

sspgdp <- read_excel("/Users/admin/Library/CloudStorage/GoogleDrive-jrising@udel.edu/My Drive/Research/CCAC/data/GAINS/SSP2_macro_4letter_20260126.xlsx")
sspgdp2 <- sspgdp %>%
    left_join(wbregions, by='REGION_4LETTER') %>%
    group_by(IDYEARS, LABEL_REGIONGROUP) %>%
    dplyr::summarize(POPULATION=sum(POPULATION),
                     GDP_GUSD2025_PPP=sum(GDP_GUSD2017_PPP) * 128.970 / 100,
                     GDP_GUSD2025_MER=sum(GDP_GUSD2017_MER) * 128.970 / 100) %>%
    filter(IDYEARS %in% c(2035, 2050, 2100)) %>%
    rename(Year=IDYEARS, Region=LABEL_REGIONGROUP)

write.csv(sspgdp2, "bigtable-gdp.csv", row.names=F)
