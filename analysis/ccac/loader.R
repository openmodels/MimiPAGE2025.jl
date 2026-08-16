library(dplyr)
library(readxl)

source("helpers.R")

wbcrs <- load.wbcrs(F)

runspecs <- read.csv("runspecs.csv")
## runspecs <- rbind(runspecs, runspecs[1,] %>% mutate(name="methaneonly"))
outdir <- 'mainruns'

prefixes <- c('abateco2-tc', 'd_market', 'd_nonmarket', 'd_slr', 'cromar-mortality',
              'MarketDamageAQ_AsthmaERVisits', 'MarketDamageAQ_CropLoss',
              'MarketDamageAQ_LostWorkHours', 'MarketDamageAQ_RespiratoryAdmissions',
              'pmmarket-percchg', 'dasgupta-labor-dmg',
              'abateall-tcpc', 'adaptall-acpc', 'discontinuity-percap',
              'WBRC-morb_healthcare_new', 'WBRC-morb_productivity_new', 'WBRC-morb_disutility_new',
              'WBRC-mort_productivity_new', 'WBRC-mort_disutility_new', 'WBRC-infrastructure_cost')
infomap <- data.frame(prefix=c(prefixes, 'capitalloss', 'apcosts', 'pm-nonmarket', 'pm-market'),
                      channel=c(rep('climate', 5), rep('combined', 4), 'pollution', 'climate',
                                rep('climate', 3), rep('pollution', 5), 'climate', 'combined', rep('pollution', 3)),
                      label=c('Costs', 'Aggregate Market Damages', 'Aggregate Non-Market Damages', 'Sea-Level Rise Damages', 'Mortality',
                              'Morbidity', 'Agriculture', 'Productivity', 'Morbidity',
                              'Aggregate Market Damages', 'Productivity',
                              'Costs', 'Adaptation', 'Discontinuity',
                              rep('Morbidity', 3), rep('Mortality', 2), 'Costs',
                              'Feedback',
                              'Costs', 'Aggregate Non-Market Damages', 'Aggregate Market Damages'))

pdf <- data.frame()
for (ii in 1:nrow(runspecs)) {
    for (prfx in prefixes) {
        if (prfx %in% unique(wbcrs$prefix)) {
            df <- subset(wbcrs, name == runspecs$name[ii] & prefix == prfx) %>% select(!c(name, prefix))
        } else {
            df <- read.csv(paste0(outdir, '/', prfx, "-", runspecs$name[ii], ".csv"))
            ## df <- subset(df, time == 2050)
            if (prfx %in% c('abateco2-tc', 'WBRC-infrastructure_cost', 'dasgupta-labor-dmg')) {
                df[, 3] <- df[, 3] * 1e6
            } else if (prfx %in% c('d_market', 'd_nonmarket', 'abateall-tcpc', 'adaptall-acpc', 'discontinuity-percap')) {
                df.pop <- read.csv(paste0(outdir, "/pop-", runspecs$name[ii], ".csv"))
                df2 <- df %>% left_join(df.pop, by=c('time', 'country'))
                df <- data.frame(time=df2$time, country=df2$country, total=df2[, 3] * df2$pop_population * 1e6)
            } else if (prfx %in% c('pmmarket-percchg', 'd_slr')) {
                if (runspecs$use_capital[ii]) {
                    df.gdp <- read.csv(paste0(outdir, "/capital-gdp-", runspecs$name[ii], ".csv"))
                    names(df.gdp)[3] <- 'gdp'
                } else
                    df.gdp <- read.csv(paste0(outdir, "/gdp-", runspecs$name[ii], ".csv"))
                df2 <- df %>% left_join(df.gdp, by=c('time', 'country'))
                if (prfx == 'pmmarket-percchg') {
                    df <- data.frame(time=df2$time, country=df2$country, total=-df2[, 3] * df2$gdp * 1e6 / 100)
                } else if (prfx == 'd_slr') {
                    df <- data.frame(time=df2$time, country=df2$country, total=df2[, 3] * df2$gdp * 1e6)
                }
            }
        }
        names(df)[3] <- 'cost'

        if (do.global) {
            pdf <- rbind(pdf, cbind(runspecs[ii, ], prefix=prfx, df %>% group_by(time) %>% summarize(cost=sum(cost))))
        } else {
            pdf <- rbind(pdf, cbind(runspecs[ii, ], prefix=prfx, df))
        }
    }

    if (runspecs$use_capital[ii]) {
        df.gdp.base <- read.csv(paste0(outdir, "/gdp-", runspecs$name[ii], ".csv"))
        df.gdp <- read.csv(paste0(outdir, "/capital-gdp-", runspecs$name[ii], ".csv"))
        df.gdp2 <- df.gdp.base %>% left_join(df.gdp, by=c('time', 'country')) %>%
            mutate(cost=(gdp - gdp_capital)*1e6)

        if (do.global) {
            pdf <- rbind(pdf, cbind(runspecs[ii, ], prefix='capitalloss', df.gdp2 %>% group_by(time) %>% summarize(cost=sum(cost))))
        } else {
            pdf <- rbind(pdf, cbind(runspecs[ii, ], prefix='capitalloss', df.gdp2[, c('time', 'country', 'cost')]))
        }
    }
}

gdps <- data.frame()
for (filename in list.files("mainruns", "^gdp.+csv$")) {
    gdps <- rbind(gdps, cbind(name=gsub("\\.csv", "", gsub("gdp-", "", filename)),
                              read.csv(file.path("mainruns", filename))))
}

if (do.global) {
    apcosts <- load.apcosts(T)
        filter(IDSCENARIOS != "Delayed Action") %>%
    names(apcosts)[names(apcosts) == 'year'] <- 'time'

    pdf <- rbind(pdf, apcosts %>% filter(time %in% unique(gdps$time)) %>% left_join(runspecs, by=c('scenario_emt'), relationship="many-to-many") %>% select(!IDSCENARIOS))

    pdf <- rbind(pdf, pdf %>% filter(prefix %in% c("WBRC-morb_disutility_new", "WBRC-mort_disutility_new")) %>%
                      group_by(time, name) %>% summarize(cost=sum(cost), scenario_emt=scenario_emt[1],
                                                         scenario_eco=scenario_eco[1], pm25_gainsmatch=pm25_gainsmatch[1],
                                                         pm25_useext=pm25_useext[1], pm25_useekc=pm25_useekc[1], use_capital=use_capital[1],
                                                         emissionfeedback=emissionfeedback[1],
                                                         use_delays=use_delays[1], use_tippt=use_tippt[1],
                                                         gh_control_factor=gh_control_factor[1], use_pageghg=use_pageghg[1], prefix='pm-nonmarket'),
                 pdf %>% filter(prefix %in% c("WBRC-morb_healthcare_new", "WBRC-morb_productivity_new", "WBRC-mort_productivity_new")) %>%
                      group_by(time, name) %>% summarize(cost=sum(cost), scenario_emt=scenario_emt[1],
                                                         scenario_eco=scenario_eco[1], pm25_gainsmatch=pm25_gainsmatch[1],
                                                         pm25_useext=pm25_useext[1], pm25_useekc=pm25_useekc[1], use_capital=use_capital[1],
                                                         emissionfeedback=emissionfeedback[1],
                                                         use_delays=use_delays[1], use_tippt=use_tippt[1],
                                                         gh_control_factor=gh_control_factor[1], use_pageghg=use_pageghg[1], prefix='pm-market'))
} else {
    ## Disaggregate costs by GDP
    gainsregions <- read.csv("../../data/pollution/GAINS_4letter_regions_mapping.csv")
    apcosts <- load.apcosts(F)

    pdf <- rbind(pdf, apcosts %>% left_join(runspecs[1:3,], by=c('scenario_emt')))

    pdf <- rbind(pdf, pdf %>% filter(prefix %in% c("WBRC-morb_disutility_new", "WBRC-mort_disutility_new")) %>%
                      group_by(country, time, name) %>% summarize(cost=sum(cost), scenario_emt=scenario_emt[1],
                                                         scenario_eco=scenario_eco[1], pm25_gainsmatch=pm25_gainsmatch[1],
                                                         pm25_useext=pm25_useext[1], pm25_useekc=pm25_useekc[1], use_capital=use_capital[1],
                                                         emissionfeedback=emissionfeedback[1],
                                                         use_delays=use_delays[1], use_tippt=use_tippt[1],
                                                         gh_control_factor=gh_control_factor[1], use_pageghg=use_pageghg[1], prefix='pm-nonmarket'),
                 pdf %>% filter(prefix %in% c("WBRC-morb_healthcare_new", "WBRC-morb_productivity_new", "WBRC-mort_productivity_new")) %>%
                      group_by(country, time, name) %>% summarize(cost=sum(cost), scenario_emt=scenario_emt[1],
                                                         scenario_eco=scenario_eco[1], pm25_gainsmatch=pm25_gainsmatch[1],
                                                         pm25_useext=pm25_useext[1], pm25_useekc=pm25_useekc[1], use_capital=use_capital[1],
                                                         emissionfeedback=emissionfeedback[1],
                                                         use_delays=use_delays[1], use_tippt=use_tippt[1],
                                                         gh_control_factor=gh_control_factor[1], use_pageghg=use_pageghg[1], prefix='pm-market'))
}
pdf$panel <- ifelse(pdf$prefix %in% c("abateco2-tc", "pmmarket-percchg"), "Other",
             ifelse(pdf$prefix %in% c("abateall-tcpc", "adaptall-acpc", "discontinuity-percap", "d_market", "d_nonmarket", 'pm-nonmarket', 'pm-market', 'apcosts', "d_slr", 'capitalloss', 'WBRC-infrastructure_cost'),
                    'Aggregate', 'Bottom-up'))

## Add 2035
if (do.global) {
    ## Not implemented yet: work is now done in loader-mc.R
} else {
    pdf <- pdf %>% group_by(name, scenario_emt, scenario_eco, pm25_gainsmatch, pm25_useext, pm25_useekc, use_capital, emissionfeedback, use_delays, gh_control_factor, use_pageghg, use_tippt, prefix, country, panel) %>%
        reframe(time2=sort(unique(c(time, 2035))), cost=sapply(time2, function(tt) ifelse(tt %in% time, cost[time == tt], (cost[time == 2030] + cost[time == 2040]) / 2))) %>%
        rename(time=time2)
    gdps <- gdps %>% group_by(name, country) %>%
        reframe(time2=sort(unique(c(time, 2035))), gdp=sapply(time2, function(tt) ifelse(tt %in% time, gdp[time == tt], (gdp[time == 2030] + gdp[time == 2040]) / 2))) %>%
        rename(time=time2)
}

wbregions <- read_excel("regiongroups_WB_global_flagship_2024.xlsx")

wtd.median <- function(xx, ww) {
    if (length(xx) > 2)
        Hmisc::wtd.quantile(xx, ww, .5)
    else
        mean(xx)
}

scen.diff.fracgdp <- function(pert, base) {
    gdps <- data.frame()
    for (filename in list.files("mainruns", "^gdp.+csv$")) {
        thisname <- gsub("\\.csv", "", gsub("gdp-", "", filename))
        if (thisname == pert) {
            gdps <- rbind(gdps, cbind(name='pert',
                                      read.csv(file.path("mainruns", filename))))
        } else if (thisname == base) {
            gdps <- rbind(gdps, cbind(name='base',
                                      read.csv(file.path("mainruns", filename))))
        }
    }
    if (do.global) {
        gdps <- gdps %>% group_by(name, time) %>% summarize(gdp=sum(gdp))
        joinby1 <- c('time', 'name')
        joinby2 <- c('prefix', 'time')
    } else {
        gdps <- gdps %>% group_by(name, country) %>%
            reframe(time2=sort(unique(c(time, 2035))), gdp=sapply(time2, function(tt) ifelse(tt %in% time, gdp[time == tt], (gdp[time == 2030] + gdp[time == 2040]) / 2))) %>%
            rename(time=time2)

        joinby1 <- c('time', 'name', 'country')
        joinby2 <- c('prefix', 'time', 'LABEL_REGIONGROUP')
    }

    pdf$name[pdf$name == pert] <- "pert"
    pdf$name[pdf$name == base] <- "base"
    if (do.global) {
        pdf2 <- pdf %>% filter(name %in% c('pert', 'base')) %>%
            left_join(gdps, by=joinby1) %>%
            mutate(cost=cost / (gdp * 1e6))

        ## Replace labor productivity using pop-weighted median (since log-effects can get extreme)
        ## Untested...
        pdf2.labor <- pdf %>%
            filter(name %in% c('pert', 'base') & prefix == 'dasgupta-labor-dmg') %>%
            left_join(gdps, by=joinby1) %>%
            mutate(ratio=pmax(cost / (1e6 * gdp), -.1)) %>%
            left_join(read.csv(paste0(outdir, "/pop-Baseline.csv")) %>% group_by(country) %>%
                      reframe(time2=sort(unique(c(time, 2035))), pop=sapply(time2, function(tt) ifelse(tt %in% time, pop_population[time == tt], (pop_population[time == 2030] + pop_population[time == 2040]) / 2))) %>%
                      rename(time=time2)) %>%
            group_by(name, prefix, time) %>%
            dplyr::summarize(cost=wtd.median(ratio, pop))
    } else {
        pdf2 <- pdf %>% filter(name %in% c('pert', 'base')) %>%
            left_join(gdps, by=joinby1) %>%
            left_join(gainsregions, by=c('country'='ISO3')) %>%
            left_join(wbregions, by='REGION_4LETTER') %>%
            group_by(name, prefix, LABEL_REGIONGROUP, time) %>%
            summarize(cost=sum(cost) / sum(1e6 * gdp))

        ## Replace labor productivity using pop-weighted median (since log-effects can get extreme)
        pdf2.labor <- pdf %>%
            filter(name %in% c('pert', 'base') & prefix == 'dasgupta-labor-dmg') %>%
            left_join(gdps, by=joinby1) %>%
            left_join(gainsregions, by=c('country'='ISO3')) %>%
            left_join(wbregions, by='REGION_4LETTER') %>%
            mutate(ratio=pmax(cost / (1e6 * gdp), -.1)) %>%
            left_join(read.csv(paste0(outdir, "/pop-Baseline.csv")) %>% group_by(country) %>%
                      reframe(time2=sort(unique(c(time, 2035))), pop=sapply(time2, function(tt) ifelse(tt %in% time, pop_population[time == tt], (pop_population[time == 2030] + pop_population[time == 2040]) / 2))) %>%
                      rename(time=time2)) %>%
            group_by(name, prefix, LABEL_REGIONGROUP, time) %>%
            dplyr::summarize(cost=wtd.median(ratio, pop))
    }

    pdf22 <- rbind(pdf2 %>% filter(prefix != 'dasgupta-labor-dmg'),
                   pdf2.labor)

    pdf3 <- subset(pdf22, name == 'pert') %>% left_join(subset(pdf22, name == 'base'), by=joinby2, suffix=c('.pert', '.base'))
    pdf3$diff <- pdf3$cost.base - pdf3$cost.pert
    pdf4 <- pdf3[, c(joinby2, 'diff')] %>% left_join(infomap, by='prefix')
    pdf4$panel <- ifelse(pdf4$prefix %in% c("abateco2-tc", "pmmarket-percchg"), "Other",
                  ifelse(pdf4$prefix %in% c("abateall-tcpc", "adaptall-acpc", "discontinuity-percap", "d_market", "d_nonmarket", 'pm-nonmarket', 'pm-market', 'apcosts', "d_slr", 'capitalloss', 'WBRC-infrastructure_cost'),
                         'Aggregate', 'Bottom-up'))
    pdf4$name <- 'Cost'

    pdf4
}
