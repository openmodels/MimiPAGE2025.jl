setwd("~/research/iamup2/MimiPAGE2025.jl/analysis/ccac")

library(dplyr)
library(ggplot2)
library(readxl)

do.global <- F
source("loader.R")

source("helpers.R")
wbregions <- read_excel("regiongroups_WB_global_flagship_2024.xlsx")
gainsregions <- read.csv("../../data/pollution/GAINS_4letter_regions_mapping.csv")

pm25.wb <- read_excel("/Users/admin/Library/CloudStorage/GoogleDrive-jrising@udel.edu/My Drive/Research/CCAC/data/GAINS/PM25_rg_20260616.xlsx")

df <- read.csv("../../data/pollution/baseline.csv") %>%
    left_join(wbregions, by='REGION_4LETTER') %>%
    group_by(IDSCENARIOS, LABEL_REGIONGROUP, IDYEARS) %>%
    summarize(PM25_TOTAL=sum(PM25_TOTAL * POPULATION, na.rm=T) / sum(POPULATION * !is.na(PM25_TOTAL * POPULATION), na.rm=T),
              GDP_USD2017_PPP_PERCAP=1e9 * sum(GDP_GUSD2017_PPP * !is.na(POPULATION), na.rm=T) / sum(POPULATION * !is.na(GDP_GUSD2017_PPP), na.rm=T),
              GDP_GUSD2017_PPP=sum(GDP_GUSD2017_PPP, na.rm=T),
              POPULATION=sum(POPULATION, na.rm=T)) %>%
    left_join(pm25.wb, by=c('IDSCENARIOS'='LABEL_SCEN_REPORT', 'LABEL_REGIONGROUP', 'IDYEARS'), suffix=c('.old', ''))

df.co2 <- rbind(cbind(IDSCENARIOS="Baseline", read.csv("mainruns/e_co2-Baseline.csv")),
                cbind(IDSCENARIOS="Integrated Action", read.csv("mainruns/e_co2-IntegratedAction.csv"))) %>%
    left_join(gainsregions, by=c('country'='ISO3')) %>%
    left_join(wbregions, by='REGION_4LETTER') %>%
    group_by(IDSCENARIOS, LABEL_REGIONGROUP, time) %>%
    summarize(eco2=sum(e_countryCO2emissions, na.rm=T)) %>%
    group_by(IDSCENARIOS, LABEL_REGIONGROUP) %>%
    reframe(year=seq(2025, 2100, by=5), eco2=approxfun(time, eco2)(year)) %>%
    left_join(df %>% group_by(IDSCENARIOS, LABEL_REGIONGROUP) %>%
              reframe(year=seq(2025, 2100, by=5), pop=approxfun(IDYEARS, POPULATION)(year))) %>%
    mutate(eco2pc=1e6 * eco2 / pop)

df.co2b <- read_excel("CO2-GDP-capita.xlsx") %>%
    group_by(LABEL_REGIONGROUP) %>%
    reframe(IDYEARS=rep(IDYEARS, 2), IDSCENARIOS=rep(c("Baseline", "Integrated Action"), each=n()),
            eco2pc=c(CO2_CAPITA_BASELINE, CO2_CAPITA_INTEGRATED_ACTION)) %>%
    group_by(IDSCENARIOS, LABEL_REGIONGROUP) %>%
    reframe(year=seq(2025, 2100, by=5), eco2pc=approxfun(IDYEARS, eco2pc)(year))

## Compare it
ggplot(df.co2 %>% left_join(df.co2b, by=c('IDSCENARIOS', 'LABEL_REGIONGROUP', 'year'), suffix=c('.PAGE', '.GAINS')) %>% filter(IDSCENARIOS == "Integrated Action"), aes(year, eco2pc.GAINS, group=paste(IDSCENARIOS, LABEL_REGIONGROUP), colour=LABEL_REGIONGROUP)) +
    geom_line()

df2 <- df %>% left_join(df.co2b, by=c('LABEL_REGIONGROUP', 'IDYEARS'='year', 'IDSCENARIOS'))

df.base <- subset(df2, IDYEARS == 2025 & IDSCENARIOS == 'Baseline')

plot(log(df.base$GDP_USD2017_PPP_PERCAP), log(df.base$eco2pc))
mod.co2 <- lm(log(eco2pc) ~ log(GDP_USD2017_PPP_PERCAP), data=df.base)

plot(log(df.base$GDP_USD2017_PPP_PERCAP), log(df.base$PM25_ANTHROP))
mod.pm <- lm(log(PM25_ANTHROP) ~ log(GDP_USD2017_PPP_PERCAP), data=df.base)

df.sum <- read.csv("bigtable.csv")
df.sum2 <- df.sum %>% filter(Impact %in% c("Adaptation Costs (PAGE)", "Sea-Level Rise Damages (pyCIAM+)", "Aggregate Market Damages (Burke et al.+)", "Discontinuity Risk (PAGE)", "Capital Persistence Loss (PAGE)", "Aggregate Market Damages (GAINS)",
                                "Infrastructure Costs (IIASA)", "Total Abatement Costs (MESSAGE)", "Air Pollution Costs (GAINS)")) %>%
    group_by(Scenario, Region, Year) %>% summarize(cost2025=sum(Cost..million.2025.USD.)) %>%
    group_by(Region, Year) %>% summarize(diff2025=cost2025[Scenario == "Baseline"] - cost2025[Scenario == "Integrated Action"]) %>%
    filter(Year == 2050)

df3 <- rbind(df.base %>% mutate(colour="1 Today"),
             subset(df2, IDYEARS == 2050 & IDSCENARIOS == 'Baseline') %>%
             left_join(df.base, by=c('IDSCENARIOS', 'LABEL_REGIONGROUP'), suffix=c('', '.25')) %>%
             mutate(eco2pc=eco2pc.25 * exp((log(GDP_USD2017_PPP_PERCAP) - log(GDP_USD2017_PPP_PERCAP.25)) * coef(mod.co2)[2]),
                    PM25_ANTHROP=PM25_ANTHROP.25 * exp((log(GDP_USD2017_PPP_PERCAP) - log(GDP_USD2017_PPP_PERCAP.25)) * coef(mod.pm)[2])) %>%
             select(!c(IDYEARS.25, PM25_ANTHROP.25, GDP_USD2017_PPP_PERCAP.25, eco2pc.25, GDP_GUSD2017_PPP.25, POPULATION.25)) %>%
             mutate(colour="2 Elasticity-only"),
             subset(df2, IDYEARS == 2050 & IDSCENARIOS == 'Baseline') %>% mutate(colour="3 Baseline"),
             subset(df2, IDYEARS == 2050 & IDSCENARIOS == 'Integrated Action') %>%
             left_join(df.sum2, by=c('LABEL_REGIONGROUP'='Region', 'IDYEARS'='Year')) %>%
             ## mutate(diff2025.frac=diff2025 * 1e6 / (GDP_GUSD2017_PPP * 1e9))
             mutate(GDP_USD2017_PPP_PERCAP=(GDP_USD2017_PPP_PERCAP * POPULATION + 1e6 * diff2025 * 100 / 128.97) / POPULATION) %>% select(!diff2025) %>%
             mutate(colour="4 Integrated Action"))

ggplot(df3, aes(GDP_USD2017_PPP_PERCAP, eco2pc, group=LABEL_REGIONGROUP)) +
    geom_line() +
    geom_point(aes(colour=colour)) +
    scale_x_log10() + scale_y_log10()

df4 <- df3 %>% group_by(LABEL_REGIONGROUP) %>%
    mutate(eco2pc=pmax(eco2pc, .01)) %>%
    reframe(x=GDP_USD2017_PPP_PERCAP[-n()],
            xend=GDP_USD2017_PPP_PERCAP[-1],
            y.co2=eco2pc[-n()],
            yend.co2=eco2pc[-1],
            y.pm=PM25_ANTHROP[-n()],
            yend.pm=PM25_ANTHROP[-1],
            segment=c("Today's Patterns",
                      "Baseline Reductions",
                      "Integrated Action"))

ggplot(df4, aes(colour=LABEL_REGIONGROUP, linetype=segment)) +
    coord_cartesian(ylim=c(.1, 50)) +
    geom_segment(aes(x=x, xend=xend, y=y.co2, yend=yend.co2), arrow.fill='white',
                 arrow=arrow(length=unit(0.2, "cm"), type="closed"), linewidth=1) +
    scale_linetype_manual("Segment:", breaks=c("Today's Patterns",
                                               "Baseline Reductions",
                                               "Integrated Action"),
                          values=c('dashed', 'longdash', 'solid')) +
    scale_colour_discrete(name="Region:") +
    scale_x_log10("GDP per capita (PPP, $2017 USD)") + scale_y_log10("CO2 Emissions per capita (t / person year)") + theme_bw()
myggsave("arrowplot-co2", width=8.5, height=6)

ggplot(subset(df4, segment == 'Integrated Action'), aes(colour=LABEL_REGIONGROUP)) +
    #coord_cartesian(ylim=c(.1, 50)) +
    geom_segment(aes(x=x, xend=xend, y=y.co2, yend=yend.co2), arrow.fill='white',
                 arrow=arrow(length=unit(0.2, "cm"), type="closed"), linewidth=1) +
    scale_colour_discrete(name="Region:") +
    scale_x_log10("GDP per capita (PPP, $2017 USD)") + scale_y_log10("CO2 Emissions per capita (t / person year)") + theme_bw()
myggsave("arrowplot-co2-simple", width=8.5, height=6)

ggplot(df4, aes(colour=LABEL_REGIONGROUP, linetype=segment)) +
    geom_segment(aes(x=x, xend=xend, y=y.pm, yend=yend.pm), arrow.fill='white',
                 arrow=arrow(length=unit(0.2, "cm"), type="closed"), linewidth=1) +
    scale_linetype_manual("Segment:", breaks=c("Today's Patterns",
                                               "Baseline Reductions",
                                               "Integrated Action"),
                          values=c('dashed', 'longdash', 'solid')) +
    scale_colour_discrete(name="Region:") +
    scale_x_log10("GDP per capita (PPP, $2017 USD)") + scale_y_log10("Anthropogenic PM2.5 concentrations") + theme_bw()
ggsave("arrowplot-pm.pdf", width=8.5, height=6)

ggplot(subset(df4, segment == 'Integrated Action'), aes(colour=LABEL_REGIONGROUP)) +
    geom_segment(aes(x=x, xend=xend, y=y.pm, yend=yend.pm), arrow.fill='white',
                 arrow=arrow(length=unit(0.2, "cm"), type="closed"), linewidth=1) +
    scale_colour_discrete(name="Region:") +
    scale_x_log10("GDP per capita (PPP, $2017 USD)") + scale_y_log10("Anthropogenic PM2.5 concentrations") + theme_bw()
myggsave("arrowplot-pm-simple", width=8.5, height=6)

## Produce the continuous arrows

get.timeseries <- function(scenario) {
    scen2name <- list("Integrated Action"="IntegratedAction",
                      "Baseline"="Baseline")

    pdf2 <- pdf %>% filter(panel == "Aggregate" & name == scen2name[[scenario]]) %>%
        filter(prefix != "pm-market" | time > 2020) %>%
        filter(prefix %in% c("WBRC-infrastructure_cost", "abateall-tcpc", "adaptall-acpc", "apcosts", "capitalloss", "d_market",
                             "d_slr", "discontinuity-percap", "pm-market")) %>%
        left_join(gdps, by=c('time', 'name', 'country')) %>%
        left_join(gainsregions, by=c('country'='ISO3')) %>%
        left_join(wbregions, by='REGION_4LETTER') %>%
        group_by(prefix, LABEL_REGIONGROUP, time) %>%
        summarize(cost.2025=sum(cost) * (100 / 63.23579)) %>%
        group_by(prefix, LABEL_REGIONGROUP) %>%
        reframe(year=seq(2025, 2100, by=5), cost.2025=splinefun(time, cost.2025)(year)) %>% #predict(smooth.spline(time, cost.2025, spar=.5), year)$y) %>%
        group_by(LABEL_REGIONGROUP, year) %>%
        summarize(cost.2025=sum(cost.2025))

    pdf3 <- pdf2 %>% left_join(subset(pdf2, year == 2025), by='LABEL_REGIONGROUP', suffix=c('', '.t0')) %>%
        mutate(cost.2025=cost.2025 - cost.2025.t0) %>% select(!cost.2025.t0)

    df3 <- rbind(df.base, subset(df2, IDSCENARIOS == scenario & IDYEARS > 2025)) %>%
        left_join(pdf3, by=c('LABEL_REGIONGROUP', 'IDYEARS'='year')) %>%
        mutate(GDP_USD2025_PPP_PERCAP.post=(GDP_USD2017_PPP_PERCAP * POPULATION * 128.97 / 100 - cost.2025) / POPULATION) %>% select(!cost.2025) %>%
        group_by(LABEL_REGIONGROUP) %>%
        ## mutate(GDP_USD2025_PPP_PERCAP.post=(GDP_USD2025_PPP_PERCAP.post + c(GDP_USD2025_PPP_PERCAP.post[1], GDP_USD2025_PPP_PERCAP.post[-n()])) / 2)
        ## mutate(GDP_USD2025_PPP_PERCAP.post=predict(loess(y ~ x, data.frame(x=IDYEARS, y=GDP_USD2025_PPP_PERCAP.post), span=.5), data.frame(x=IDYEARS)))
        mutate(GDP_USD2025_PPP_PERCAP.post=predict(smooth.spline(IDYEARS, GDP_USD2025_PPP_PERCAP.post, spar=.5), IDYEARS)$y,
               eco2pc=predict(smooth.spline(IDYEARS, eco2pc, spar=.5), IDYEARS)$y,
               PM25_ANTHROP=predict(smooth.spline(IDYEARS, PM25_ANTHROP, spar=.5), IDYEARS)$y)

    df3
}

df3 <- get.timeseries("Integrated Action")

ggplot(df3, aes(GDP_USD2025_PPP_PERCAP.post, pmax(.103, eco2pc), colour=LABEL_REGIONGROUP)) +
    coord_cartesian(ylim=c(.1, 50)) +
    geom_line(linewidth=1) +
    geom_point(data=subset(df3, IDYEARS %in% c(2035, 2050, 2100)), aes(shape=factor(IDYEARS)), colour='black') +
    scale_colour_discrete(name="Region:") +
    scale_shape_manual(name="Year:", breaks=factor(c(2035, 2050, 2100)), values=c(1, 2, 3)) +
    scale_x_log10("GDP per capita (PPP, $2025 USD)") + scale_y_log10("CO2 Emissions per capita (t / person year)", expand=c(0, 0)) + theme_bw()
ggsave("arrowplot-co2-v2.pdf", width=8.5, height=6)

ggplot(df3, aes(GDP_USD2025_PPP_PERCAP.post, PM25_ANTHROP, colour=LABEL_REGIONGROUP)) +
    geom_line(linewidth=1) +
    geom_point(data=subset(df3, IDYEARS %in% c(2035, 2050, 2100)), aes(shape=factor(IDYEARS)), colour='black') +
    scale_colour_discrete(name="Region:") +
    scale_shape_manual(name="Year:", breaks=factor(c(2035, 2050, 2100)), values=c(1, 2, 3)) +
    scale_x_log10("GDP per capita (PPP, $2017 USD)") + scale_y_log10("Anthropogenic PM2.5 concentrations") + theme_bw()
ggsave("arrowplot-pm-v2.pdf", width=8.5, height=6)

df3.base <- get.timeseries("Baseline")

df4 <- df3 %>% left_join(df3.base, by=c('LABEL_REGIONGROUP', 'IDYEARS'), suffix=c('', '.base'))

ggplot(df4, aes(GDP_USD2025_PPP_PERCAP.post, eco2pc - eco2pc.base, colour=LABEL_REGIONGROUP)) +
    geom_line(linewidth=1) +
    geom_point(data=subset(df4, IDYEARS %in% c(2035, 2050, 2100)), aes(shape=factor(IDYEARS)), colour='black') +
    scale_colour_discrete(name="Region:") +
    scale_shape_manual(name="Year:", breaks=factor(c(2035, 2050, 2100)), values=c(1, 2, 3)) +
    xlab("GDP per capita (PPP, $2025 USD)") + ylab("CO2 Emissions (Mt / year)") + theme_bw()
ggsave("arrowplot-co2-v2-diff.pdf", width=8.5, height=6)

ggplot(df4, aes(GDP_USD2025_PPP_PERCAP.post, PM25_ANTHROP - PM25_ANTHROP.base, colour=LABEL_REGIONGROUP)) +
    geom_line(linewidth=1) +
    geom_point(data=subset(df4, IDYEARS %in% c(2035, 2050, 2100)), aes(shape=factor(IDYEARS)), colour='black') +
    scale_colour_discrete(name="Region:") +
    scale_shape_manual(name="Year:", breaks=factor(c(2035, 2050, 2100)), values=c(1, 2, 3)) +
    xlab("GDP per capita (PPP, $2017 USD)") + ylab("Anthropogenic PM2.5 concentrations") + theme_bw()
ggsave("arrowplot-pm-v2-diff.pdf", width=8.5, height=6)

df4 <- rbind(cbind(panel="Integrated Action", df3), cbind(panel="Baseline", df3.base))

ggplot(df4, aes(GDP_USD2025_PPP_PERCAP.post, pmax(.103, eco2pc), colour=LABEL_REGIONGROUP)) +
    facet_wrap(~ panel) +
    coord_cartesian(ylim=c(.2, 15)) +
    geom_line(linewidth=1) +
    geom_point(data=subset(df4, IDYEARS %in% c(2035, 2050, 2100)), aes(shape=factor(IDYEARS)), colour='black') +
    scale_colour_discrete(name="Region:") +
    scale_shape_manual(name="Year:", breaks=factor(c(2035, 2050, 2100)), values=c(1, 2, 3)) +
    scale_x_log10("GDP per capita (PPP, $2025 USD)") + scale_y_log10("CO2 Emissions per capita (t / person year)", expand=c(0, 0)) + theme_bw()
myggsave("arrowplot-co2-v2-pair", width=8.5, height=6)

ggplot(df4, aes(GDP_USD2025_PPP_PERCAP.post, PM25_ANTHROP, colour=LABEL_REGIONGROUP)) +
    facet_wrap(~ panel) +
    geom_line(linewidth=1) +
    geom_point(data=subset(df4, IDYEARS %in% c(2035, 2050, 2100)), aes(shape=factor(IDYEARS)), colour='black') +
    scale_colour_discrete(name="Region:") +
    scale_shape_manual(name="Year:", breaks=factor(c(2035, 2050, 2100)), values=c(1, 2, 3)) +
    scale_x_log10("GDP per capita (PPP, $2017 USD)") + scale_y_log10("Anthropogenic PM2.5 concentrations") + theme_bw()
myggsave("arrowplot-pm-v2-pair", width=8.5, height=6)
