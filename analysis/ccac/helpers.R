require(openxlsx)
library(readxl)

myggsave <- function(filebase, plot=get_last_plot(), width=6.5, height=4) {
    ggsave(file.path("For Document", paste0(filebase, ".png")), width=width, height=height)
    ggsave(file.path("For Share", paste0(filebase, ".pdf")), width=width, height=height)

    plotdata <- ggplot_build(plot)$data
    if (length(plot$layers) > 1) {
        sheets <- list()
        for (ii in 1:length(plot$layers)) {
            if (inherits(plot$layers[[ii]]$data, "waiver"))
                sheets[[ii]] <- plot$data
            else
                sheets[[ii]] <- plot$layers[[ii]]$data
            if (is.null(nrow(sheets[[ii]]))) {
                sheets[[ii]] <- plotdata[[ii]]
            }
        }
    } else {
        sheets <- plot$data
        if (is.null(nrow(sheets))) {
            sheets <- plotdata[[1]]
        }
    }

    write.xlsx(sheets, file=file.path("For Share", paste0(filebase, ".xlsx")))

    print("Copy For Share to https://drive.google.com/drive/u/1/folders/1l5P8xGyJSxU97wP7wAMgXnjcjnKCxOdp")
}

load.apcosts <- function(do.global) {
    apcosts <- read_excel("/Users/admin/Library/CloudStorage/GoogleDrive-jrising@udel.edu/My Drive/Research/CCAC/data/GAINS/airpoll_control_costs_20260424.xlsx") %>% select(!c('AP_CONTROL_COSTS_MEUR2015_MER', 'AP_PLUS_N2O_CONTROL_COSTS_MEUR2015_MER'))
    apcosts2020 <- subset(apcosts, IDYEARS == 2025)
    apcosts2020$IDYEARS <- 2020
    apcosts2 <- rbind(apcosts2020, apcosts)

    sspdata <- read_excel("/Users/admin/Library/CloudStorage/GoogleDrive-jrising@udel.edu/My Drive/Research/CCAC/data/GAINS/SSP2_macro_4letter_20260126.xlsx")
    sspdata$mer2ppp <- sspdata$GDP_GUSD2017_PPP / sspdata$GDP_GUSD2017_MER

    apcosts3 <- apcosts2 %>% left_join(sspdata[, c('REGION_4LETTER', 'IDYEARS', 'mer2ppp')], by=c('REGION_4LETTER', 'IDYEARS')) %>%
        mutate(AP_PLUS_N2O_CH4_CONTROL_COSTS_MEUR2015_PPP=AP_PLUS_N2O_CH4_CONTROL_COSTS_MEUR2015_MER * ifelse(is.na(mer2ppp), 1, mer2ppp)) %>%
        select(!c(AP_PLUS_N2O_CH4_CONTROL_COSTS_MEUR2015_MER, mer2ppp))

    if (do.global) {
        apcosts4 <- apcosts3 %>%
            mutate(time=IDYEARS) %>% group_by(time, LABEL_SCEN_REPORT) %>%
            summarize(cost=sum(AP_PLUS_N2O_CH4_CONTROL_COSTS_MEUR2015_PPP * 1e6 * 1.1102 * 81.551 / 97.315)) %>%
            filter(LABEL_SCEN_REPORT != "Delayed Action") %>%
            group_by(LABEL_SCEN_REPORT) %>% arrange(time) %>%
            reframe(year=seq(2020, 2100, by=5), cost=splinefun(time, cost, method='fmm')(year)) %>%
            mutate(scenario_emt=LABEL_SCEN_REPORT, prefix='apcosts') %>% rename(IDSCENARIOS=LABEL_SCEN_REPORT)

        ## For Check: apcosts4.global = apcosts4
    } else {
        name2scen <- list('Baseline'="Baseline", 'LTCAction'="Long-term Climate Action", 'IntegratedAction'="Integrated Action")
        gdps <- data.frame()
        for (filename in list.files("mainruns", "^gdp.+csv$")) {
            if (filename %in% paste0("gdp-", names(name2scen), '.csv')) {
                gdps <- rbind(gdps, cbind(name=name2scen[[gsub("\\.csv", "", gsub("gdp-", "", filename))]],
                                          read.csv(file.path("mainruns", filename))))
            }
        }
        gdps2 <- gdps %>% group_by(name, country) %>%
            reframe(year=seq(2020, 2100, by=5), gdp=splinefun(time, gdp, method='fmm')(year))
        gainsregions <- read.csv("../../data/pollution/GAINS_4letter_regions_mapping.csv")

        apcosts4 <- apcosts3 %>%
            left_join(gainsregions, by=c('REGION_4LETTER'), relationship="many-to-many") %>%
            left_join(gdps2, by=c('LABEL_SCEN_REPORT'='name', 'IDYEARS'='year', 'ISO3'='country')) %>%
            group_by(IDYEARS, LABEL_SCEN_REPORT, REGION_4LETTER) %>%
            mutate(cost=AP_PLUS_N2O_CH4_CONTROL_COSTS_MEUR2015_PPP * 1e6 * 1.1102 * (81.551 / 97.315) * gdp / sum(gdp, na.rm=T)) %>%
            rename(country=ISO3, time=IDYEARS) %>%
            mutate(scenario_emt=LABEL_SCEN_REPORT, prefix='apcosts') %>% ungroup() %>%
            select(!c(LABEL_SCEN_REPORT, REGION_4LETTER, AP_PLUS_N2O_CH4_CONTROL_COSTS_MEUR2015_PPP, REGION_4LETTER_LABEL, COUNTRY, gdp, isomatch))

        ## Check:
        ## apcosts4 %>% group_by(time, scenario_emt) %>% summarize(cost=sum(cost))
    }

    apcosts4
}

load.wbcrs <- function(do.mc) {
    if (do.mc) {
        prefixes <- c('WBRegionCorrection_morb_healthcare_new', 'WBRegionCorrection_morb_productivity_new', 'WBRegionCorrection_morb_disutility_new',
                      'WBRegionCorrection_mort_productivity_new', 'WBRegionCorrection_mort_disutility_new')
        outdir <- 'mainruns-mc'
        pdf <- data.frame()
        for (name in list.files(outdir)) {
            for (prefix in prefixes) {
                df <- read.csv(paste0(outdir, '/', name, "/", prefix, ".csv"))
                df[, 3] <- df[, 3] * 1e6
                names(df)[3] <- 'cost'
                pdf <- rbind(pdf, cbind(name=name, prefix=prefix, df))
            }
        }
        pdf <- rbind(pdf, pdf %>% filter(time %in% c(2030, 2040)) %>% group_by(name, prefix, country, trialnum) %>% summarize(cost=mean(cost), time=2035)) %>% arrange(time)
    } else {
        runspecs <- read.csv("runspecs.csv")
        outdir <- 'mainruns'
        prefixes <- c('WBRC-morb_healthcare_new', 'WBRC-morb_productivity_new', 'WBRC-morb_disutility_new',
                      'WBRC-mort_productivity_new', 'WBRC-mort_disutility_new')
        pdf <- data.frame()
        for (ii in 1:nrow(runspecs)) {
            for (prefix in prefixes) {
                df <- read.csv(paste0(outdir, '/', prefix, "-", runspecs$name[ii], ".csv"))
                df[, 3] <- df[, 3] * 1e6
                names(df)[3] <- 'cost'
                pdf <- rbind(pdf, cbind(name=runspecs$name[ii], prefix=prefix, df))
            }
        }
        pdf <- rbind(pdf, pdf %>% filter(time %in% c(2030, 2040)) %>% group_by(name, prefix, country) %>% summarize(cost=mean(cost), time=2035)) %>% arrange(time)
    }

    match <- read.csv("../../data/pollution/wbregioncorrections-match2.csv") %>% filter(!is.na(REGIONGROUP))
    match2 <- rbind(match, match %>% filter(IDYEARS %in% c(2070, 2080)) %>% group_by(SCEN_LABEL_H, SCEN_LABEL_L, REGIONGROUP, LABEL_REGIONGROUP) %>%
                           summarize(DELTA_COST_HEALTHCARE_MUSD2017_PPP_MORB=mean(DELTA_COST_HEALTHCARE_MUSD2017_PPP_MORB),
                                     DELTA_COST_HEALTHCARE_MUSD2017_PPP_MORT=mean(DELTA_COST_HEALTHCARE_MUSD2017_PPP_MORT),
                                     DELTA_COST_PRODUCTIVITY_MUSD2017_PPP_MORB=mean(DELTA_COST_PRODUCTIVITY_MUSD2017_PPP_MORB),
                                     DELTA_COST_PRODUCTIVITY_MUSD2017_PPP_MORT=mean(DELTA_COST_PRODUCTIVITY_MUSD2017_PPP_MORT),
                                     DELTA_COST_DISUTILITY_MUSD2017_PPP_MORB=mean(DELTA_COST_DISUTILITY_MUSD2017_PPP_MORB),
                                     DELTA_COST_DISUTILITY_MUSD2017_PPP_MORT=mean(DELTA_COST_DISUTILITY_MUSD2017_PPP_MORT),
                                     IDYEARS=2075))

    wbregions <- read_excel("regiongroups_WB_global_flagship_2024.xlsx")
    wbregions2 <- wbregions %>% left_join(read.csv("../../data/pollution/GAINS_4letter_regions_mapping.csv"), by='REGION_4LETTER')

    pdf.new <- data.frame()
    for (prfx in prefixes) {
        pdf2 <- pdf %>% filter(prefix == prfx & time > 2020 & time <= 2100 & name %in% c('Baseline', 'IntegratedAction')) %>% left_join(wbregions2, by=c('country'='ISO3'))
        if (do.mc) {
            pdf3 <- pdf2 %>% group_by(name, time, REGIONGROUP, trialnum) %>% summarize(cost=sum(cost, na.rm=T)) %>%
                group_by(time, REGIONGROUP, trialnum) %>% summarize(base=cost[name == 'Baseline'], diff=cost[name == 'Baseline'] - cost[name == 'IntegratedAction'])
        } else {
            pdf3 <- pdf2 %>% group_by(name, time, REGIONGROUP) %>% summarize(cost=sum(cost, na.rm=T)) %>%
                group_by(time, REGIONGROUP) %>% summarize(base=cost[name == 'Baseline'], diff=cost[name == 'Baseline'] - cost[name == 'IntegratedAction'])
        }
        pdf4 <- pdf3 %>% left_join(match2, by=c('REGIONGROUP', 'time'='IDYEARS')) %>%
            mutate(DELTA_COST_HEALTHCARE_MUSD2017_PPP_MORB=DELTA_COST_HEALTHCARE_MUSD2017_PPP_MORB * 1e6 * 81.551 / 100,
                   DELTA_COST_HEALTHCARE_MUSD2017_PPP_MORT=DELTA_COST_HEALTHCARE_MUSD2017_PPP_MORT * 1e6 * 81.551 / 100,
                   DELTA_COST_PRODUCTIVITY_MUSD2017_PPP_MORB=DELTA_COST_PRODUCTIVITY_MUSD2017_PPP_MORB * 1e6 * 81.551 / 100,
                   DELTA_COST_PRODUCTIVITY_MUSD2017_PPP_MORT=DELTA_COST_PRODUCTIVITY_MUSD2017_PPP_MORT * 1e6 * 81.551 / 100,
                   DELTA_COST_DISUTILITY_MUSD2017_PPP_MORB=DELTA_COST_DISUTILITY_MUSD2017_PPP_MORB * 1e6 * 81.551 / 100,
                   DELTA_COST_DISUTILITY_MUSD2017_PPP_MORT=DELTA_COST_DISUTILITY_MUSD2017_PPP_MORT * 1e6 * 81.551 / 100)
        pdf4$diff.match <- ifelse(rep(prfx, nrow(pdf4)) == prefixes[1], pdf4$DELTA_COST_HEALTHCARE_MUSD2017_PPP_MORB,
                          ifelse(rep(prfx, nrow(pdf4)) == prefixes[2], pdf4$DELTA_COST_PRODUCTIVITY_MUSD2017_PPP_MORB,
                          ifelse(rep(prfx, nrow(pdf4)) == prefixes[3], pdf4$DELTA_COST_DISUTILITY_MUSD2017_PPP_MORB,
                          ifelse(rep(prfx, nrow(pdf4)) == prefixes[4], pdf4$DELTA_COST_PRODUCTIVITY_MUSD2017_PPP_MORT,
                          ifelse(rep(prfx, nrow(pdf4)) == prefixes[5], pdf4$DELTA_COST_DISUTILITY_MUSD2017_PPP_MORT, UNKNOWN)))))
        pdf4$diff.scale <- pdf4$diff.match / pdf4$diff
        if (do.mc) {
            pdf.new.rows <- pdf %>% filter(prefix == prfx) %>% left_join(subset(pdf, name == 'Baseline') %>% select(!name), by=c('prefix', 'time', 'country', 'trialnum'), suffix=c('', '.base'))
        } else {
            pdf.new.rows <- pdf %>% filter(prefix == prfx) %>% left_join(subset(pdf, name == 'Baseline') %>% select(!name), by=c('prefix', 'time', 'country'), suffix=c('', '.base'))
        }
        pdf.new.rows2 <- pdf.new.rows %>%
            left_join(wbregions2[, c('REGIONGROUP', 'ISO3')], by=c('country'='ISO3'))
        if (do.mc) {
            pdf.new.rows3 <- pdf.new.rows2 %>%
                left_join(pdf4[, c('time', 'REGIONGROUP', 'diff.scale', 'trialnum')], by=c('time', 'REGIONGROUP', 'trialnum'))
        } else {
            pdf.new.rows3 <- pdf.new.rows2 %>%
                left_join(pdf4[, c('time', 'REGIONGROUP', 'diff.scale')], by=c('time', 'REGIONGROUP'))
        }
        pdf.new.rows4 <- pdf.new.rows3 %>%
            mutate(cost.base=ifelse(is.na(cost.base), 0, cost.base),
                   diff.scale=ifelse(!is.finite(diff.scale), 1, diff.scale),
                   cost=(cost - cost.base) * diff.scale + cost.base) %>%
            select(!c(cost.base, REGIONGROUP, diff.scale))
        pdf.new <- rbind(pdf.new, pdf.new.rows4)
    }

    pdf.new
}

## diff.smooth <- function(df) {
##     df %>% arrange(time) %>%
##         reframe(year=seq(2025, 2100, by=5),
##                 diff=splinefun(c(2025, 2035, time), c(0, ifelse(2035 %in% time, diff[time == 2035], (diff[time == 2030] + diff[time == 2040]) / 2), diff), method='monoH.FC')(year))
## }
