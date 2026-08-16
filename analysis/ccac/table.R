setwd("~/research/iamup2/MimiPAGE2025.jl/analysis/ccac")

library(dplyr)
library(tidyr)
library(readxl)
library(EnvStats)

source("hazards.R")
source("helpers.R")

wbcrs <- load.wbcrs(T)

sspgdp <- read_excel("/Users/admin/Library/CloudStorage/GoogleDrive-jrising@udel.edu/My Drive/Research/CCAC/data/GAINS/SSP2_macro_4letter_20260126.xlsx") %>%
    group_by(YEAR=IDYEARS) %>% dplyr::summarize(VALUE=sum(GDP_GUSD2017_PPP))
sspgdp$value2025 <- sspgdp$VALUE * 128.970 / 100

get.row.row <- function(df2, year) {
    if (year == 2035) {
        df2030 <- subset(df2, time == 2030)
        df2040 <- subset(df2, time == 2040)
        df2030$var <- (df2030$var + df2040$var) / 2
        df2030$time <- 2035
        return(df2030)
    } else
        return(subset(df2, time == year))
}

get.row.helper <- function(filebase, aggregate, years=c(2035, 2050, 2100)) {
    if (filebase == 'GDP_gdp_ssp') {
        scaling <- read.csv(file.path("mainruns-mc", "Baseline", 'GDP_gdp.csv')) %>%
            group_by(time, trialnum) %>% summarize(gdp2025=sum(gdp) * 100 / 63.2) %>%
            left_join(sspgdp, by=c('time'='YEAR')) %>%
            mutate(scale=value2025 * 1000 / gdp2025)
        return(get.row.helper('GDP_gdp',
                              function(x) aggregate(x %>% left_join(scaling[, c('time', 'trialnum', 'scale')], by=c('time', 'trialnum')) %>%
                                                    mutate(var=scale * var) %>% select(!scale))))
    }
    if (filebase %in% unique(wbcrs$prefix)) {
        info <- data.frame()
        for (scen in c('Baseline', 'LTCAction', 'IntegratedAction')) {
            info <- rbind(info, aggregate(subset(wbcrs, prefix == filebase & name == scen) %>% select(!c(prefix, name)) %>% rename(var=cost) %>% filter(time %in% years)) %>%
                          mutate(scenario=scen, var=var / 1e6))
        }
        return(info)
    }
    if (filebase == 'WBRegionCorrection_infrastructure_cost') {
        df <- read_excel("/Users/admin/Library/CloudStorage/GoogleDrive-jrising@udel.edu/My Drive/Research/CCAC/data/ACT_LCOE_COST_Transport_ELE_infrastructure.xlsx") %>% group_by(time=YEAR) %>% summarize(base=sum(TOTAL_ACTIVITY_MED) * 1e6 * 1.1102 * 81.551 / 97.315, delta=sum(DELTA_COST) * 1e6 * 1.1102 * 81.551 / 97.315)
        info <- data.frame()
        for (scen in c('Baseline', 'LTCAction', 'IntegratedAction')) {
            if (scen == 'Baseline')
                df$var <- df$base
            else
                df$var <- df$base + df$delta
            for (year in years) {
                if (year > 2050)
                    info <- rbind(info, cbind(scenario=scen, trialnum=1, df %>% get.row.row(2050) %>% mutate(time=year)))
                else
                    info <- rbind(info, cbind(scenario=scen, trialnum=1, df %>% get.row.row(year)))
            }
        }
        return(info)
    }
    if (filebase == "TotalAbatementCosts_tct_totalcosts") {
        abatetot <- read.csv("../../data/climate/climatecosts-netnodal.csv")
        decarb <- abatetot$Decarb[abatetot$YEAR %in% years]
        baseline <- abatetot$Baseline[abatetot$YEAR %in% years]
        info <- data.frame()
        for (scenario in c('Baseline', 'LTCAction', 'IntegratedAction')) {
            if (scenario == 'Baseline')
                df <- data.frame(time=years, country='global', var=baseline * 1000 * (63.23579 / 69.49899), trialnum=1)
            else
                df <- data.frame(time=years, country='global', var=decarb * 1000 * (63.23579 / 69.49899), trialnum=1)
            df2 <- aggregate(df)
            info <- rbind(info, cbind(scenario=scenario, df2))
        }
        return(info)
    }

    info <- data.frame()
    for (scenario in c('Baseline', 'LTCAction', 'IntegratedAction')) {
        df <- read.csv(file.path("mainruns-mc", scenario, paste0(filebase, '.csv')))
        if (names(df)[1] == 'time' & names(df)[2] == 'country') {
            names(df)[3] <- 'var'
            df2 <- aggregate(df)
        } else {
            names(df)[2] <- 'var'
            df2 <- df
        }


        for (year in years)
            info <- rbind(info, cbind(scenario=scenario, df2 %>% group_by(trialnum) %>% get.row.row(year)))
    }
    info
}

get.row <- function(filebase, aggregate, extra='none', years=c(2035, 2050, 2100), finalagg=T, addunc='none') {
    if (extra == 'bypop') {
        info <- get.row.helper(filebase, function(x) x, years=years)
        info <- info %>%
            left_join(get.row.helper('Population_pop_population', function(x) x, years=years),
                      by=c('scenario', 'time', 'country', 'trialnum'), suffix=c('', '.pop'))
        info <- aggregate(info)
    } else if (extra == 'addsoc') {
        info <- get.row.helper(filebase, function(x) x, years=years)
        info <- info %>%
            left_join(get.row.helper('Population_pop_population', function(x) x, years=years),
                      by=c('scenario', 'time', 'country', 'trialnum'), suffix=c('', '.pop')) %>%
            left_join(get.row.helper('GDP_gdp', function(x) x %>% mutate(var=var * 100 / 63.2), years=years),
                      by=c('scenario', 'time', 'country', 'trialnum'), suffix=c('', '.gdp'))
        info <- aggregate(info)
    } else {
        info <- get.row.helper(filebase, aggregate, years=years)
        if (extra == 'bygdp') {
            info <- info %>%
                left_join(get.row.helper('GDP_gdp', function(x) x %>% group_by(trialnum, time) %>%
                                                            summarize(var=sum(var) * 100 / 63.2), years=years),
                          by=c('scenario', 'time', 'trialnum'), suffix=c('', '.gdp')) %>%
                mutate(var=100 * var / var.gdp / 1e6) %>% select(!var.gdp)
        }
    }

    if (finalagg) {
        df.to.row(info, addunc)
    } else {
        info
    }
}

df.to.row <- function(info, addunc, cirange=.5) {
    info2 <- info %>% group_by(time, trialnum) %>%
        reframe(var=c(var, var[scenario == 'Baseline'] - var[scenario == 'LTCAction'],
                      var[scenario == 'LTCAction'] - var[scenario == 'IntegratedAction'],
                      var[scenario == 'Baseline'] - var[scenario == 'IntegratedAction']),
                scenario=c(scenario, '(Baseline - LTCAction)',
                           '(LTCAction - IntegratedAction)', '(Baseline - IntegratedAction)'))

    if (addunc == "pm") {
        info2 <- info2 %>% group_by(trialnum) %>%
            mutate(sigmaap=approx(c(2035, 2050, 2100), c(1.23, 1.25, 1.33), rule=2, time)$y,
                   var=var * rlnorm(1, -log(sigmaap)^2/2, log(sigmaap)))
    } else if (addunc == "cost") {
        if (all(info$trialnum == 1)) {
            info2 <- info2 %>% group_by(time, scenario) %>%
                reframe(trialnum=1:1000, var=var * rtri(1000, .6, 1.25, 1.15))
        } else {
            info2 <- info2 %>% group_by(trialnum) %>%
                mutate(var=var * rtri(1, .6, 1.25, 1.15))
        }
    } else if (addunc != 'none') {
        stopifnot(F)
    }

    info2 %>%
        group_by(scenario, time) %>% summarize(mu=mean(var, na.rm=T), med=median(var, na.rm=T),
                                               ci25=quantile(var, .5 - cirange/2, na.rm=T), ci75=quantile(var, .5 + cirange/2, na.rm=T)) %>%
        pivot_wider(names_from='scenario', values_from=c('mu', 'med', 'ci25', 'ci75'), names_glue = "{scenario}_{.value}", names_sort=T) %>%
        select(time,
               contains("Baseline_"),
               contains("LTCAction_"),
               contains("IntegratedAction_"),
               contains("(Baseline - LTCAction)"),
               contains("(LTCAction - IntegratedAction)"),
               contains("(Baseline - IntegratedAction)"))
}

gdp.uncorrected <- get.row('GDP_gdp', function(x) x %>% group_by(time, trialnum) %>% summarize(var=sum(var) * 100 / 63.2))
gdp.corrected <- get.row('GDP_gdp_ssp', function(x) x %>% group_by(time, trialnum) %>% summarize(var=sum(var) * 100 / 63.2))

sspscaling <- rbind(read.csv(file.path("mainruns-mc", "Baseline", 'GDP_gdp.csv')),
                    read.csv(file.path("mainruns-mc", "Baseline", 'GDP_gdp.csv')) %>% filter(time %in% c(2030, 2040)) %>%
                    group_by(country, trialnum) %>% summarize(gdp=mean(gdp), time=2035)) %>%
    group_by(time, trialnum) %>% summarize(gdp2025=sum(gdp) * 100 / 63.2) %>%
    left_join(sspgdp, by=c('time'='YEAR')) %>%
    mutate(scale=value2025 * 1000 / gdp2025)

sspscaling %>% group_by(time) %>% summarize(scale=mean(scale))

sspscale <- function(xx) {
    xx %>% left_join(sspscaling[, c('time', 'trialnum', 'scale')], by=c('time', 'trialnum')) %>%
        mutate(var=var / scale) %>% select(!scale)
}

## sspscale is used with GAINS calculations reported as a fraction of global GDP, since GAINS does not include all countries

out <- rbind(cbind(outcome='GSAT', units='°C',
                   df.to.row(df.gsat %>% rename(var=gsat, time=year) %>% filter(time %in% c(2035, 2050, 2100)), 'none')), #get.row('FaIRGrounds_rt_g_globaltemperature')),
             cbind(outcome='GSAT - 95CI', units='°C',
                   df.to.row(df.gsat %>% rename(var=gsat, time=year) %>% filter(time %in% c(2035, 2050, 2100)), 'none', cirange=.95)), #get.row('FaIRGrounds_rt_g_globaltemperature')),
             cbind(outcome='SLR', units='m',
                   df.to.row(df.slr %>% rename(var=slr, time=year) %>% filter(time %in% c(2035, 2050, 2100)), 'none')), #get.row('SeaLevelRise_s_sealevel')),
             cbind(outcome='PM2.5 (pop-weighted)', units='μg/m3',
                   get.row('PM25Pollution_pm_total', function(x) x %>% group_by(trialnum, time, scenario) %>%
                                             summarize(var=sum(var * var.pop) / sum(var.pop)), 'bypop')),
             cbind(outcome='Warming Excess Deaths', units='deaths/1000 persons/yr', get.row('CromarMortality_excess_death_rate', function(x) x %>% group_by(trialnum, time, scenario) %>%
                                                                                                                                summarize(var=sum(var * var.pop) / sum(var.pop)), 'bypop')),
             cbind(outcome='Warming Mortality Damages', units='Trillion 2025 USD', get.row('CromarMortality_mortality_costs', function(x) x %>% group_by(trialnum, time) %>% summarize(var=sum(var) * 100 / 63.2 / 1e12))),
             cbind(outcome='Warming Mortality Damages', units='% GGDP', get.row('CromarMortality_mortality_costs', function(x) x %>% group_by(trialnum, time) %>% summarize(var=sum(var) * 100 / 63.2), extra='bygdp')),
             cbind(outcome='GAINS Morbidity Disutility Damages', units='Trillion 2025 USD', get.row('WBRegionCorrection_morb_disutility_new', function(x) x %>% group_by(trialnum, time) %>% summarize(var=1e6 * sum(var) * 100 / 63.2 / 1e12), addunc='pm')),
             cbind(outcome='GAINS Morbidity Disutility Damages', units='% GGDP', get.row('WBRegionCorrection_morb_disutility_new', function(x) x %>% group_by(trialnum, time) %>% summarize(var=1e6 * sum(var) * 100 / 63.2) %>% sspscale, extra='bygdp', addunc='pm')),
             cbind(outcome='GAINS Healthcare Damages', units='Trillion 2025 USD', get.row('WBRegionCorrection_morb_healthcare_new', function(x) x %>% group_by(trialnum, time) %>% summarize(var=1e6 * sum(var) * 100 / 63.2 / 1e12), addunc='pm')),
             cbind(outcome='GAINS Healthcare Damages', units='% GGDP', get.row('WBRegionCorrection_morb_healthcare_new', function(x) x %>% group_by(trialnum, time) %>% summarize(var=1e6 * sum(var) * 100 / 63.2) %>% sspscale, extra='bygdp', addunc='pm')),
             cbind(outcome='GAINS Productivity Damages', units='Trillion 2025 USD', get.row('WBRegionCorrection_morb_productivity_new', function(x) x %>% group_by(trialnum, time) %>% summarize(var=1e6 * sum(var) * 100 / 63.2 / 1e12), addunc='pm')),
             cbind(outcome='GAINS Productivity Damages', units='% GGDP', get.row('WBRegionCorrection_morb_productivity_new', function(x) x %>% group_by(trialnum, time) %>% summarize(var=1e6 * sum(var) * 100 / 63.2) %>% sspscale, extra='bygdp', addunc='pm')),
             cbind(outcome='GAINS Mortality Productivity Damages', units='Trillion 2025 USD', get.row('WBRegionCorrection_mort_productivity_new', function(x) x %>% group_by(trialnum, time) %>% summarize(var=1e6 * sum(var) * 100 / 63.2 / 1e12), addunc='pm')),
             cbind(outcome='GAINS Mortality Productivity Damages', units='% GGDP', get.row('WBRegionCorrection_mort_productivity_new', function(x) x %>% group_by(trialnum, time) %>% summarize(var=1e6 * sum(var) * 100 / 63.2) %>% sspscale, extra='bygdp', addunc='pm')),
             cbind(outcome='GAINS Mortality Disutility Damages', units='Trillion 2025 USD', get.row('WBRegionCorrection_mort_disutility_new', function(x) x %>% group_by(trialnum, time) %>% summarize(var=1e6 * sum(var) * 100 / 63.2 / 1e12), addunc='pm')),
             cbind(outcome='GAINS Mortality Disutility Damages', units='% GGDP', get.row('WBRegionCorrection_mort_disutility_new', function(x) x %>% group_by(trialnum, time) %>% summarize(var=1e6 * sum(var) * 100 / 63.2) %>% sspscale, extra='bygdp', addunc='pm')),
             cbind(outcome='PM2.5 Aggregate Market Damages', units='% GGDP', get.row('PMMarketDamages_totalchange', function(x) x %>% group_by(trialnum, time, scenario) %>% summarize(var=(sum(var * var.gdp) / sum(var.gdp))), extra='addsoc', addunc='pm')),
             cbind(outcome='Methane Action Crop Damages', units='% GGDP', get.row('MarketDamageAQ_CropLoss_total_market_damage', function(x) x %>% group_by(trialnum, time, scenario) %>% summarize(var=100 * (sum(var) / sum(var.gdp)) * 100 / 63.2 / 1e6), extra='addsoc')),
             cbind(outcome='Methane Action Lost Work Damages', units='% GGDP', get.row('MarketDamageAQ_LostWorkHours_total_market_damage', function(x) x %>% group_by(trialnum, time, scenario) %>% summarize(var=100 * (sum(var) / sum(var.gdp)) * 100 / 63.2 / 1e6), extra='addsoc')),
             cbind(outcome='Warming Labour Productivity', units='% GGDP', get.row('DasguptaLabor_damages', function(x) x %>% group_by(trialnum, time) %>%
                                                                                                                       summarize(var=-sum(var, na.rm=T) * 100 / 63.2), 'bygdp')),
             cbind(outcome='Warming Aggregate Market Damages', units='% GGDP',
                   get.row('MarketDamagesBurke_isat_per_cap_ImpactperCapinclSaturationandAdaptation', function(x) x %>% group_by(trialnum, time, scenario) %>% summarize(var=100 * (sum(var * var.pop) / sum(var.gdp)) * 100 / 63.2), extra='addsoc')),
             cbind(outcome='Warming Aggregate SLR Market Damages', units='% GGDP', get.row('SLRDamages_d_slr', function(x) x %>% group_by(trialnum, time, scenario) %>% summarize(var=100 * (sum(var * var.gdp) / sum(var.gdp))), extra='addsoc')),
             cbind(outcome="Warming Market Feedback Damages", units='% GGDP',
                   get.row('Capital_gdp_capital', function(x) x %>% group_by(trialnum, time, scenario) %>% summarize(var=100 * (1 - (sum(var) * 100 / 63.2) / sum(var.gdp))), extra='addsoc')),
             cbind(outcome='Warming Aggregate Non-Market Damages', units='% GGDP', get.row('NonMarketDamages_isat_per_cap_ImpactperCapinclSaturationandAdaptation', function(x) x %>% group_by(trialnum, time, scenario) %>% summarize(var=100 * (sum(var * var.pop) / sum(var.gdp)) * 100 / 63.2), extra='addsoc')),
             cbind(outcome='Warming Aggregate Discontinuity Damages', units='% GGDP', get.row('Discontinuity_isat_per_cap_DiscImpactperCapinclSaturation', function(x) x %>% group_by(trialnum, time, scenario) %>% summarize(var=100 * (sum(var * var.pop) / sum(var.gdp)) * 100 / 63.2), extra='addsoc')),
             cbind(outcome='Air Pollution Control Costs', units='Trillion 2025 USD',
                   df.to.row(load.apcosts(T) %>% filter(year %in% c(2035, 2050, 2100)) %>%
                             mutate(time=year, trialnum=1, scenario=ifelse(IDSCENARIOS == "Long-term Climate Action", "LTCAction",
                                                                    ifelse(IDSCENARIOS == "Integrated Action", "IntegratedAction", IDSCENARIOS)), var=cost * 100 / 63.2 / 1e12), 'cost')),
             cbind(outcome='Air Pollution Control Costs', units='% GGDP',
                   df.to.row(load.apcosts(T) %>% filter(year %in% c(2035, 2050, 2100)) %>%
                             mutate(time=year, trialnum=1, scenario=ifelse(IDSCENARIOS == "Long-term Climate Action", "LTCAction",
                                                                    ifelse(IDSCENARIOS == "Integrated Action", "IntegratedAction", IDSCENARIOS)), var=cost) %>%
                             left_join(get.row.helper('GDP_gdp', function(x) x %>% group_by(trialnum, time) %>%
                                                                             summarize(var=sum(var))), by=c('scenario', 'time', 'trialnum'), suffix=c('.cost', '.gdp')) %>%
                             mutate(var=100 * var.cost / (1e6 * var.gdp)) %>% sspscale, 'cost')),
             cbind(outcome='Climate Net Nodal Costs', units='% GGDP', get.row('TotalAbatementCosts_tct_totalcosts', function(x) x %>% group_by(trialnum, time) %>% summarize(var=1e6 * sum(var) * 100 / 63.2) %>% sspscale, extra='bygdp', addunc='cost')),
             cbind(outcome='Climate Infrastructure Costs', units='% GGDP', get.row('WBRegionCorrection_infrastructure_cost', function(x) x %>% group_by(trialnum, time) %>% summarize(var=1e6 * sum(var) * 100 / 63.2) %>% sspscale, extra='bygdp', addunc='cost')))

library(writexl)
write_xlsx(out, "summary-top.xlsx")
