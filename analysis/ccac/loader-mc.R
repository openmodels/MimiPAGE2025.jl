library(dplyr)
library(tidyr)
library(readxl)

source("helpers.R")

wbcrs <- load.wbcrs(T)

outdir <- 'mainruns-mc'

sspgdp <- read_excel("/Users/admin/Library/CloudStorage/GoogleDrive-jrising@udel.edu/My Drive/Research/CCAC/data/GAINS/SSP2_macro_4letter_20260126.xlsx") %>%
    group_by(YEAR=IDYEARS) %>% summarize(VALUE=sum(GDP_GUSD2017_PPP))
sspgdp$value2025 <- sspgdp$VALUE * 128.970 / 100

sspscaling <- rbind(read.csv(file.path("mainruns-mc", "Baseline", 'GDP_gdp.csv')),
                    read.csv(file.path("mainruns-mc", "Baseline", 'GDP_gdp.csv')) %>% filter(time %in% c(2030, 2040)) %>%
                    group_by(country, trialnum) %>% summarize(gdp=mean(gdp), time=2035)) %>%
    group_by(time, trialnum) %>% summarize(gdp2025=sum(gdp) * 100 / 63.2) %>%
    left_join(sspgdp, by=c('time'='YEAR')) %>%
    mutate(scale=value2025 * 1000 / gdp2025)

## sspscaling is used with GAINS calculations reported as a fraction of global GDP, since GAINS does not include all countries

prefixes <- c('AbatementCostsCO2_tc_totalcost_national', 'MarketDamagesBurke_isat_per_cap_ImpactperCapinclSaturationandAdaptation',
              'NonMarketDamages_isat_per_cap_ImpactperCapinclSaturationandAdaptation', 'SLRDamages_d_slr', 'CromarMortality_mortality_costs',
              'MarketDamageAQ_AsthmaERVisits_total_market_damage', 'MarketDamageAQ_CropLoss_total_market_damage',
              'MarketDamageAQ_LostWorkHours_total_market_damage', 'MarketDamageAQ_RespiratoryAdmissions_total_market_damage',
              'PMMarketDamages_totalchange', 'LaborProductivity_outcome', 'DasguptaLabor_damages',
              'TotalAbatementCosts_tct_percap_totalcostspercap', 'TotalAdaptationCosts_act_percap_adaptationcosts',
              'Discontinuity_isat_per_cap_DiscImpactperCapinclSaturation',
              'WBRegionCorrection_morb_healthcare_new', 'WBRegionCorrection_morb_productivity_new', 'WBRegionCorrection_morb_disutility_new',
              'WBRegionCorrection_mort_productivity_new', 'WBRegionCorrection_mort_disutility_new', 'WBRegionCorrection_infrastructure_cost')
infomap <- data.frame(prefix=c(prefixes, 'capitalloss', 'apcosts', 'pm-nonmarket', 'pm-market'),
                      channel=c(rep('climate', 5), rep('combined', 4), 'pollution', 'climate',
                                rep('climate', 4), rep('pollution', 5), 'climate', 'combined', rep('pollution', 3)),
                      label=c('Costs', 'Aggregate Market Damages', 'Aggregate Non-Market Damages', 'Sea-Level Rise Damages', 'Mortality',
                              'Morbidity', 'Agriculture', 'Productivity', 'Morbidity',
                              'Aggregate Market Damages', 'Productivity', 'Productivity',
                              'Costs', 'Adaptation', 'Discontinuity',
                              rep('Morbidity', 3), rep('Mortality', 2), 'Costs',
                              'Feedback',
                              'Costs', 'Aggregate Non-Market Damages', 'Aggregate Market Damages'))

pdf <- data.frame()
for (nam in list.files(outdir)) {
    for (prfx in prefixes) {
        if (prfx %in% unique(wbcrs$prefix)) {
            df <- subset(wbcrs, name == nam & prefix == prfx) %>% select(!c(name, prefix))
        } else if (prfx == 'TotalAbatementCosts_tct_percap_totalcostspercap') {
            abatetot <- read.csv("../../data/climate/climatecosts-netnodal.csv") %>% reframe(time=seq(2020, 2100, by=5), Decarb=splinefun(YEAR, Decarb, method='monoH.FC')(time), Baseline=splinefun(YEAR, Baseline, method='monoH.FC')(time))
            if (nam == 'Baseline')
                df <- data.frame(time=abatetot$time, country='global', var=rep(abatetot$Baseline * 1e9 * (63.23579 / 69.49899), 1000),
                                 trialnum=rep(1:1000, each=nrow(abatetot)))
            else
                df <- data.frame(time=abatetot$time, country='global', var=rep(abatetot$Decarb * 1e9 * (63.23579 / 69.49899), 1000),
                                 trialnum=rep(1:1000, each=nrow(abatetot)))
        } else if (prfx == 'WBRegionCorrection_infrastructure_cost') {
            if (nam == 'Baseline')
                dfx <- read_excel("/Users/admin/Library/CloudStorage/GoogleDrive-jrising@udel.edu/My Drive/Research/CCAC/data/ACT_LCOE_COST_Transport_ELE_infrastructure.xlsx") %>% group_by(time=YEAR) %>% reframe(var=sum(TOTAL_ACTIVITY_MED) * 1e6 * 1.1102 * 81.551 / 97.315)
            else
                dfx <- read_excel("/Users/admin/Library/CloudStorage/GoogleDrive-jrising@udel.edu/My Drive/Research/CCAC/data/ACT_LCOE_COST_Transport_ELE_infrastructure.xlsx") %>% group_by(time=YEAR) %>% reframe(var=sum(TOTAL_ACTIVITY_MED + DELTA_COST) * 1e6 * 1.1102 * 81.551 / 97.315)

            df <- data.frame()
            for (year in c(2020, 2030, 2040, 2050, 2075, 2100)) {
                if (year < 2030)
                    df <- rbind(df, cbind(country='global', subset(dfx, time == 2030), trialnum=1:1000) %>% mutate(time=year))
                else if (year > 2050)
                    df <- rbind(df, cbind(country='global', subset(dfx, time == 2050), trialnum=1:1000) %>% mutate(time=year))
                else
                    df <- rbind(df, cbind(country='global', subset(dfx, time == year), trialnum=1:1000))
            }
        } else {
            df <- read.csv(paste0(outdir, '/', nam, "/", prfx, ".csv"))
            ## df <- subset(df, time == 2050)
            if (prfx %in% c('AbatementCostsCO2_tc_totalcost_national', 'WBRegionCorrection_infrastructure_cost', 'DasguptaLabor_damages')) {
                df[, 3] <- df[, 3] * 1e6
                df[is.na(df[, 3]), 3] <- 0
            } else if (prfx %in% c('MarketDamagesBurke_isat_per_cap_ImpactperCapinclSaturationandAdaptation', 'NonMarketDamages_isat_per_cap_ImpactperCapinclSaturationandAdaptation', 'TotalAbatementCosts_tct_percap_totalcostspercap', 'TotalAdaptationCosts_act_percap_adaptationcosts', 'Discontinuity_isat_per_cap_DiscImpactperCapinclSaturation')) {
                df.pop <- read.csv(file.path(outdir, nam, "Population_pop_population.csv"))
                df2 <- df %>% left_join(df.pop, by=c('trialnum', 'time', 'country'))
                df <- data.frame(time=df2$time, country=df2$country, total=df2[, 3] * df2$pop_population * 1e6, trialnum=df2$trialnum)
            } else if (prfx %in% c('PMMarketDamages_totalchange', 'LaborProductivity_outcome', 'SLRDamages_d_slr')) {
                df.gdp <- read.csv(file.path(outdir, nam, "GDP_gdp.csv"))
                names(df.gdp)[3] <- 'gdp'
                df2 <- df %>% left_join(df.gdp, by=c('trialnum', 'time', 'country'))
                if (prfx == 'PMMarketDamages_totalchange') {
                    df <- data.frame(time=df2$time, country=df2$country, total=-df2[, 3] * df2$gdp * 1e6 / 100, trialnum=df2$trialnum)
                } else if (prfx == 'LaborProductivity_outcome') {
                    df <- data.frame(time=df2$time, country=df2$country, total=-df2[, 3] * df2$gdp * 1e6, trialnum=df2$trialnum)
                } else if (prfx == 'SLRDamages_d_slr') {
                    df <- data.frame(time=df2$time, country=df2$country, total=df2[, 3] * df2$gdp * 1e6, trialnum=df2$trialnum)
                } else {
                    print(paste("No logic B path for", prfx))
                }
            } else if (prfx %in% c('CromarMortality_mortality_costs', 'MarketDamageAQ_AsthmaERVisits_total_market_damage',
                                   'MarketDamageAQ_CropLoss_total_market_damage',
                                   'MarketDamageAQ_LostWorkHours_total_market_damage',
                                   'MarketDamageAQ_RespiratoryAdmissions_total_market_damage')) {
                ## Nothing to do
            } else {
                print(paste("No logic A path for", prfx))
            }
        }

        names(df)[3] <- 'cost'

        if (do.global) {
            pdf <- rbind(pdf, cbind(name=nam, prefix=prfx, df %>% group_by(trialnum, time) %>% summarize(cost=sum(cost))))
        } else {
            pdf <- rbind(pdf, cbind(name=nam, prefix=prfx, df))
        }
    }

    df.gdp.base <- read.csv(file.path(outdir, nam, "GDP_gdp.csv"))
    df.gdp <- read.csv(file.path(outdir, nam, "Capital_gdp_capital.csv"))
    df.gdp2 <- df.gdp.base %>% left_join(df.gdp, by=c('trialnum', 'time', 'country')) %>%
        mutate(cost=(gdp - gdp_capital)*1e6)

    if (do.global) {
        pdf <- rbind(pdf, cbind(name=nam, prefix='capitalloss', df.gdp2 %>% group_by(trialnum, time) %>% summarize(cost=sum(cost))))
    } else {
        pdf <- rbind(pdf, cbind(name=nam, prefix='capitalloss', df.gdp2[, c('trialnum', 'time', 'country', 'cost')]))
    }
}

gdps <- data.frame()
for (name in list.files(outdir)) {
    gdps <- rbind(gdps, cbind(name=name,
                              read.csv(file.path(outdir, name, "GDP_gdp.csv"))))
}

if (do.global) {
    apcosts <- load.apcosts(T) %>%
        filter(IDSCENARIOS != "Delayed Action") %>%
        mutate(name=ifelse(IDSCENARIOS == "Baseline", "Baseline",
                    ifelse(IDSCENARIOS == "Integrated Action", "IntegratedAction",
                    ifelse(IDSCENARIOS == "Long-term Climate Action", "LTCAction",
                    ifelse(IDSCENARIOS == "Delayed Action", "IntegratedAction_delay", "Other"))))) %>% select(!IDSCENARIOS)
    names(apcosts)[names(apcosts) == 'year'] <- 'time'

    pdf <- rbind(pdf, crossing(apcosts %>% select(!'scenario_emt') %>% filter(time %in% unique(gdps$time)), trialnum=1:1000))

    pdf <- rbind(pdf, pdf %>% filter(prefix %in% c("WBRegionCorrection_morb_disutility_new", "WBRegionCorrection_mort_disutility_new")) %>%
                      group_by(trialnum, time, name) %>% summarize(cost=sum(cost), prefix='pm-nonmarket'),
                 pdf %>% filter(prefix %in% c("WBRegionCorrection_morb_healthcare_new", "WBRegionCorrection_morb_productivity_new", "WBRegionCorrection_mort_productivity_new")) %>%
                 group_by(trialnum, time, name) %>% summarize(cost=sum(cost), prefix='pm-market'))
} else {
    ## Disaggregate costs by GDP
    gainsregions <- read.csv("../../data/pollution/GAINS_4letter_regions_mapping.csv")
    apcosts <- load.apcosts(F)

    pdf <- rbind(pdf, apcosts)

    pdf <- rbind(pdf, pdf %>% filter(prefix %in% c("WBRegionCorrection_morb_disutility_new", "WBRegionCorrection_mort_disutility_new")) %>%
                      group_by(country, time, name) %>% summarize(cost=sum(cost), scenario_emt=scenario_emt[1],
                                                         scenario_eco=scenario_eco[1], pm25_gainsmatch=pm25_gainsmatch[1],
                                                         pm25_useext=pm25_useext[1], pm25_useekc=pm25_useekc[1], use_capital=use_capital[1],
                                                         emissionfeedback=emissionfeedback[1],
                                                         use_delays=use_delays[1], use_tippt=use_tippt[1],
                                                         gh_control_factor=gh_control_factor[1], use_pageghg=use_pageghg[1], prefix='pm-nonmarket'),
                 pdf %>% filter(prefix %in% c("WBRegionCorrection_morb_healthcare_new", "WBRegionCorrection_morb_productivity_new", "WBRegionCorrection_mort_productivity_new")) %>%
                      group_by(country, time, name) %>% summarize(cost=sum(cost), scenario_emt=scenario_emt[1],
                                                         scenario_eco=scenario_eco[1], pm25_gainsmatch=pm25_gainsmatch[1],
                                                         pm25_useext=pm25_useext[1], pm25_useekc=pm25_useekc[1], use_capital=use_capital[1],
                                                         emissionfeedback=emissionfeedback[1],
                                                         use_delays=use_delays[1], use_tippt=use_tippt[1],
                                                         gh_control_factor=gh_control_factor[1], use_pageghg=use_pageghg[1], prefix='pm-market'))
}
pdf$panel <- ifelse(pdf$prefix %in% c("AbatementCostsCO2_tc_totalcost_national", "PMMarketDamages_totalchange"), "Other",
             ifelse(pdf$prefix %in% c("TotalAbatementCosts_tct_percap_totalcostspercap", "TotalAdaptationCosts_act_percap_adaptationcosts", "Discontinuity_isat_per_cap_DiscImpactperCapinclSaturation", "MarketDamagesBurke_isat_per_cap_ImpactperCapinclSaturationandAdaptation", "NonMarketDamages_isat_per_cap_ImpactperCapinclSaturationandAdaptation", 'pm-nonmarket', 'pm-market', 'apcosts', "SLRDamages_d_slr", 'capitalloss', 'WBRegionCorrection_infrastructure_cost'),
                    'Aggregate', 'Bottom-up'))

wbregions <- read_excel("regiongroups_WB_global_flagship_2024.xlsx")

scen.diff.fracgdp <- function(pert, base) {
    gdps <- rbind(cbind(name='pert', read.csv(file.path(outdir, pert, "GDP_gdp.csv"))),
                  cbind(name='base', read.csv(file.path(outdir, base, "GDP_gdp.csv"))))
    gdps2 <- gdps %>% group_by(name, country, trialnum) %>% reframe(year=c(2020, seq(2030, 2100, by=5)), gdp=splinefun(time, gdp, method='monoH.FC')(year)) %>% rename(time=year)
    if (do.global) {
        gdps2 <- gdps2 %>% group_by(trialnum, name, time) %>% summarize(gdp=sum(gdp))
        joinby1 <- c('trialnum', 'time', 'name')
        joinby2 <- c('trialnum', 'prefix', 'time')
    } else {
        joinby1 <- c('time', 'name', 'country')
        joinby2 <- c('prefix', 'time', 'LABEL_REGIONGROUP')
    }

    pdf$name[pdf$name == pert] <- "pert"
    pdf$name[pdf$name == base] <- "base"
    if (do.global) {
        sspscale.prefixes <- c('WBRegionCorrection_morb_disutility_new', 'WBRegionCorrection_morb_healthcare_new',
                               'WBRegionCorrection_morb_productivity_new', 'WBRegionCorrection_mort_productivity_new',
                               'WBRegionCorrection_mort_disutility_new', 'apcosts', 'TotalAbatementCosts_tct_totalcosts',
                               'WBRegionCorrection_infrastructure_cost', 'pm-market', 'pm-nonmarket')
        pdf2 <- pdf %>% filter(name %in% c('pert', 'base')) %>%
            left_join(gdps2, by=joinby1) %>%
            mutate(cost=cost / (gdp * 1e6)) %>%
            left_join(sspscaling[, c('time', 'trialnum', 'scale')], by=c('time', 'trialnum')) %>%
            mutate(cost=ifelse(prefix %in% sspscale.prefixes, cost / scale, cost)) %>% select(!scale)
    } else {
        pdf2 <- pdf %>% filter(name %in% c('pert', 'base')) %>%
            left_join(gdps2, by=joinby1) %>%
            left_join(gainsregions, by=c('country'='ISO3')) %>%
            left_join(wbregions, by='REGION_4LETTER') %>%
            group_by(name, prefix, LABEL_REGIONGROUP, time) %>%
            summarize(cost=sum(cost) / sum(1e6 * gdp))
    }
    pdf3 <- subset(pdf2, name == 'pert') %>% left_join(subset(pdf2, name == 'base'), by=joinby2, suffix=c('.pert', '.base'))
    pdf3$diff <- pdf3$cost.base - pdf3$cost.pert
    pdf4 <- pdf3[, c(joinby2, 'diff')] %>% left_join(infomap, by='prefix')
    pdf4$panel <- ifelse(pdf4$prefix %in% c("AbatementCostsCO2_tc_totalcost_national", "PMMarketDamages_totalchange"), "Other",
                  ifelse(pdf4$prefix %in% c("TotalAbatementCosts_tct_percap_totalcostspercap", "TotalAdaptationCosts_act_percap_adaptationcosts", "Discontinuity_isat_per_cap_DiscImpactperCapinclSaturation", "MarketDamagesBurke_isat_per_cap_ImpactperCapinclSaturationandAdaptation", "NonMarketDamages_isat_per_cap_ImpactperCapinclSaturationandAdaptation", 'pm-nonmarket', 'pm-market', 'apcosts', "SLRDamages_d_slr", 'capitalloss', 'WBRegionCorrection_infrastructure_cost'),
                         'Aggregate', 'Bottom-up'))
    pdf4$name <- 'Cost'

    pdf4
}

scen.diff.fracgdp.mean <- function(pert, base) {
    pdf2 <- scen.diff.fracgdp(pert, base)

    pdf2 %>% group_by(prefix, time, channel, label, panel, name) %>% summarize(diff=mean(diff, na.rm=T))
}
