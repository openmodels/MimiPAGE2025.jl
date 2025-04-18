setwd("~/research/iamup2/MimiPAGE2020.jl/analysis/national")

library(ggplot2)
library(dplyr)

get.ts <- function(filename) {
    df <- read.csv(file.path("output", filename))
    names(df)[2] <- 'var'
    df %>% group_by(time) %>% summarize(mu=mean(var),
                                        ci25=quantile(var, .25),
                                        ci75=quantile(var, .75))
}

plot.ts <- function(filename, ylabel) {
    df2 <- get.ts(filename)

    ggplot(df2, aes(time, mu)) +
        geom_line() + geom_ribbon(aes(ymin=ci25, ymax=ci75), alpha=.5) +
        theme_bw() + scale_x_continuous(NULL, expand=c(0, 0)) + ylab(ylabel) +
        theme(plot.margin = unit(c(0.1, 0.2, 0.1, 0.1), "inches"))
}

plot.ts("GlobalTemperature_rt_g_globaltemperature.csv", "Change from preindustrial (° C)")
plot.ts("SeaLevelRise_s_sealevel.csv", "Mean sea level rise (m)")
ggsave("../../output/figures/sealevel.pdf", width=6.5, height=4)

get.kts <- function(filename, mufunc=mean) {
    df <- read.csv(file.path("output", filename))
    names(df)[3] <- 'var'
    df %>% filter(country == 'KOR') %>% group_by(time) %>%
        summarize(mu=mufunc(var),
                  ci25=quantile(var, .25),
                  ci75=quantile(var, .75))
}

plot.kts <- function(filename) {
    df2 <- get.kts(filename)
    ggplot(df2, aes(time, mu)) +
        geom_line() + geom_ribbon(aes(ymin=ci25, ymax=ci75), alpha=.5)
}

plot.kts("GDP_cons_percap_consumption.csv")
plot.kts("RegionTemperature_rtl_realizedtemperature_absolute.csv")
plot.kts("CountryLevelNPV_act_percap_adaptationcosts.csv")
plot.kts("CountryLevelNPV_tct_percap_totalcosts_total.csv")
plot.kts("CountryLevelNPV_wit_percap_equityweightedimpact.csv")
plot.kts("MarketDamagesBurke_i1log_impactlogchange.csv")
plot.kts("NonMarketDamages_isat_per_cap_ImpactperCapinclSaturationandAdaptation.csv")
plot.kts("SLRDamages_d_percap_slr.csv")

df1 <- get.kts("CountryLevelNPV_wit_percap_equityweightedimpact.csv")
df2 <- get.kts("GDP_cons_percap_consumption.csv")
##df3 <- get.kts("../output-ssp126/CountryLevelNPV_wit_percap_equityweightedimpact.csv")
df3 <- get.kts("../wit_percap_equityweightedimpact-ssp126.csv")
df4 <- get.kts("../wit_percap_equityweightedimpact-1p5.csv")
df5 <- get.kts("../wit_percap_equityweightedimpact-2p0.csv")
df6 <- get.kts("../wit_percap_equityweightedimpact-2p5.csv")

pdf <- df1 %>% left_join(df2, by='time', suffix=c('.wit', '')) %>%
    left_join(df3, by='time', suffix=c('', '.126')) %>%
    left_join(df4, by='time', suffix=c('', '.1p5')) %>%
    left_join(df5, by='time', suffix=c('', '.2p0')) %>%
    left_join(df6, by='time', suffix=c('.gdp', '.2p5'))

ggplot(pdf, aes(time)) +
    geom_line(aes(y=mu.wit / mu.gdp, colour='RFF-SPs')) +
    ## geom_line(aes(y=mu.126 / mu.gdp, colour='SSP1-2.6')) +
    geom_line(aes(y=mu.1p5 / mu.gdp, colour='1.5 deg. target')) +
    geom_line(aes(y=mu.2p0 / mu.gdp, colour='2 deg. target')) +
    geom_line(aes(y=mu.2p5 / mu.gdp, colour='2.5 deg. target')) +
    scale_x_continuous(NULL, expand=c(0, 0)) + scale_y_continuous("Total damages (% GDP)", labels=scales::percent) +
    scale_colour_discrete(NULL) +
    theme_bw()
ggsave("../../output/figures/damages-xscen-kor.pdf", width=6.5, height=4)

df1 <- get.kts("MarketDamagesBurke_isat_per_cap_ImpactperCapinclSaturationandAdaptation.csv")
df2 <- get.kts("NonMarketDamages_isat_per_cap_ImpactperCapinclSaturationandAdaptation.csv")
df3 <- get.kts("SLRDamages_d_percap_slr.csv")
df4 <- get.kts("Discontinuity_isat_per_cap_DiscImpactperCapinclSaturation.csv", median)

pdf <- rbind(cbind(df1, group='Market Damages'),
             cbind(df2, group='Non-market Damages'),
             cbind(df3, group='SLR Damages'),
             cbind(df4, group='Discontinuity Damages'))

ggplot(pdf, aes(as.numeric(time), mu)) +
    geom_ribbon(aes(ymin=ci25, ymax=ci75, group=group), alpha=.5) +
    geom_line(aes(colour=group)) +
    theme_bw() + scale_x_continuous(NULL, expand=c(0, 0)) + ylab("Damages ($ / person)") +
    scale_colour_manual(NULL, breaks=c('Non-market Damages', 'Discontinuity Damages', 'SLR Damages', 'Market Damages'),
                        values=c('#1b9e77', '#e7298a', '#7570b3', '#d95f02'))
ggsave("../../output/figures/damages-xtype-kor.pdf", width=6.5, height=4)

get.xts <- function(filename, mufunc=mean) {
    df <- read.csv(file.path("output", filename))
    names(df)[3] <- 'var'
    df %>% filter(country %in% c('IND', 'CHN', 'USA', 'IDN', 'PAK', 'NGA', 'BRA', 'DEU', 'RUS')) %>% group_by(country, time) %>%
        summarize(mu=mufunc(var),
                  ci25=quantile(var, .25),
                  ci75=quantile(var, .75))
}

df1 <- get.xts("MarketDamagesBurke_isat_per_cap_ImpactperCapinclSaturationandAdaptation.csv")
df2 <- get.xts("NonMarketDamages_isat_per_cap_ImpactperCapinclSaturationandAdaptation.csv")
df3 <- get.xts("SLRDamages_d_percap_slr.csv")
df4 <- get.xts("Discontinuity_isat_per_cap_DiscImpactperCapinclSaturation.csv")
df4$time <- as.numeric(df4$time)

pdf <- rbind(cbind(df1, group='Market Damages'),
             cbind(df2, group='Non-market Damages'),
             cbind(df3, group='SLR Damages'),
             cbind(df4, group='Discontinuity Damages'))

ggplot(pdf, aes(as.numeric(time), mu)) +
    facet_wrap(~ country, scales="free_y", nrow=3, ncol=3) +
    geom_ribbon(aes(ymin=ci25, ymax=ci75, group=group), alpha=.5) +
    geom_line(aes(colour=group)) +
    theme_bw() + scale_x_continuous(NULL, expand=c(0, 0)) + ylab("Damages ($ / person)") +
    scale_colour_manual(NULL, breaks=c('Non-market Damages', 'Discontinuity Damages', 'SLR Damages', 'Market Damages'),
                        values=c('#1b9e77', '#e7298a', '#7570b3', '#d95f02'))
ggsave("../../output/figures/damages-xtype-9.pdf", width=9, height=6.5)

## XXX: I need to divide each by its associated GDP. Current calcs don't work.

df1 <- get.xts("CountryLevelNPV_wit_percap_equityweightedimpact.csv")
df2 <- get.xts("GDP_cons_percap_consumption.csv")
df3 <- get.xts("../wit_percap_equityweightedimpact-ssp126.csv")
df4 <- get.xts("../wit_percap_equityweightedimpact-1p5.csv")
df5 <- get.xts("../wit_percap_equityweightedimpact-2p0.csv")
df6 <- get.xts("../wit_percap_equityweightedimpact-2p5.csv")

pdf <- df1 %>% left_join(df2, by=c('country', 'time'), suffix=c('.wit', '')) %>%
    left_join(df3, by=c('country', 'time'), suffix=c('', '.126')) %>%
    left_join(df4, by=c('country', 'time'), suffix=c('', '.1p5')) %>%
    left_join(df5, by=c('country', 'time'), suffix=c('', '.2p0')) %>%
    left_join(df6, by=c('country', 'time'), suffix=c('.gdp', '.2p5'))

ggplot(pdf, aes(time)) +
    facet_wrap(~ country, scales="free_y", nrow=3, ncol=3) +
    geom_line(aes(y=mu.wit / mu.gdp, colour='RFF-SPs')) +
    geom_line(aes(y=mu.1p5 / mu.gdp, colour='1.5 deg. target')) +
    geom_line(aes(y=mu.2p0 / mu.gdp, colour='2 deg. target')) +
    geom_line(aes(y=mu.2p5 / mu.gdp, colour='2.5 deg. target')) +
    scale_x_continuous(NULL, expand=c(0, 0)) + scale_y_continuous("Total damages (% GDP)", labels=scales::percent) +
    scale_colour_discrete(NULL) +
    theme_bw()
ggsave("../../output/figures/damages-xscen-9.pdf", width=9, height=6.5)

pdf$rfffrac <- pdf$mu.wit / pdf$mu.gdp
pdf$p15frac <- pdf$mu.1p5 / pdf$mu.gdp
pdf$p20frac <- pdf$mu.2p0 / pdf$mu.gdp
pdf$p25frac <- pdf$mu.2p5 / pdf$mu.gdp
subset(pdf, time == 2100)

library(PBSmapping)

byisos <- read.csv("../../data/bycountry.csv")
byaggs <- read.csv("../../data/aggregates.csv")

shp <- importShapefile("~/data/political/ne_10m_admin_0_countries_lakes/ne_10m_admin_0_countries_lakes.shp")
polydata <- attr(shp, 'PolyData')
polydata$code <- as.character(polydata$ADM0_A3)
polydata$code[polydata$ADMIN == "South Sudan"] <- "SSD"
polydata$code[polydata$ADMIN == "Palestine"] <- "PSE"
polydata$code[polydata$ADMIN == "Kosovo"] <- "XKX"
polydata$code[polydata$ADMIN == "Western Sahara"] <- "ESH"
## We also distinguish Tokelau, but it is in the New Zealand polygons
polydata$code[polydata$ADMIN == "Somaliland"] <- "SOM"

polydata2 <- polydata %>% left_join(byaggs, by=c('code'='ISO')) %>% left_join(byisos, by=c('code'='ISO3'))
polydata2$code[!is.na(polydata2$Aggregate)] <- polydata2$Aggregate[!is.na(polydata2$Aggregate)]

get.allts <- function(filename) {
    df <- read.csv(file.path("output", filename))
    names(df)[3] <- 'var'
    df %>% group_by(time, country) %>%
        summarize(mu=mean(var, na.rm=T),
                  ci25=quantile(var, .25, na.rm=T),
                  ci75=quantile(var, .75, na.rm=T))
}

df <- get.allts("SLRDamages_d_percap_slr.csv")
df.gdp <- get.allts("GDP_cons_percap_consumption.csv")

df2 <- df %>% left_join(df.gdp, by=c('time', 'country'), suffix=c('.wit', '.gdp'))
df2$mu.frac <- df2$mu.wit / df2$mu.gdp
polydata3 <- polydata2 %>% left_join(subset(df2, time == 2100), by=c('code'='country'))
shp2 <- shp %>% left_join(polydata3[, c('PID', 'mu.frac')])
ggplot(shp2, aes(X, Y, group=paste(PID, SID))) +
    geom_polygon(aes(fill=ifelse(mu.frac == 0, 0, pmax(1e-6, mu.frac)))) +
    theme_bw() + scale_x_continuous(NULL, expand=c(0, 0)) + scale_y_continuous(NULL, expand=c(0, 0)) +
    scale_fill_distiller("Damages (% Cons.)", palette="YlGnBu", direction=1, trans='log10', breaks=c(1e-6, 1e-5, 1e-4, 1e-3, 1e-2, 1e-1), labels=scales::percent)
ggsave("../../output/figures/d_cons_slr.png", width=9.5, height=5)

df <- get.allts("MarketDamagesBurke_isat_per_cap_ImpactperCapinclSaturationandAdaptation.csv")
df2 <- df %>% left_join(df.gdp, by=c('time', 'country'), suffix=c('.wit', '.gdp'))
df2$mu.frac <- df2$mu.wit / df2$mu.gdp
polydata3 <- polydata2 %>% left_join(subset(df2, time == 2100), by=c('code'='country'))
shp2 <- shp %>% left_join(polydata3[, c('PID', 'mu.frac')])
ggplot(shp2, aes(X, Y, group=paste(PID, SID))) +
    geom_polygon(aes(fill=mu.frac)) +
    theme_bw() + scale_x_continuous(NULL, expand=c(0, 0)) + scale_y_continuous(NULL, expand=c(0, 0)) +
    scale_fill_distiller("Damages (% Cons.)", type='div', palette="RdYlGn", limits=c(-1, 1)*max(abs(df2$mu.frac)), labels=scales::percent)
ggsave("../../output/figures/d_cons_mkt.png", width=9.5, height=5)

df <- get.allts("NonMarketDamages_isat_per_cap_ImpactperCapinclSaturationandAdaptation.csv")
df2 <- df %>% left_join(df.gdp, by=c('time', 'country'), suffix=c('.wit', '.gdp'))
df2$mu.frac <- df2$mu.wit / df2$mu.gdp
polydata3 <- polydata2 %>% left_join(subset(df2, time == 2100), by=c('code'='country'))
shp2 <- shp %>% left_join(polydata3[, c('PID', 'mu.frac')])
ggplot(shp2, aes(X, Y, group=paste(PID, SID))) +
    geom_polygon(aes(fill=mu.frac)) +
    theme_bw() + scale_x_continuous(NULL, expand=c(0, 0)) + scale_y_continuous(NULL, expand=c(0, 0)) +
    scale_fill_distiller("Damages (% Cons.)", type='seq', direction=1, palette="YlOrRd", limits=c(0, max(df2$mu.frac[df2$time == 2100])), labels=scales::percent)
ggsave("../../output/figures/d_cons_non.png", width=9.5, height=5)

df <- get.allts("MarketDamagesBurke_i1log_impactlogchange.csv")
polydata3 <- polydata2 %>% left_join(subset(df, time == 2300), by=c('code'='country'))

shp2 <- shp %>% left_join(polydata3[, c('PID', 'mu')])

ggplot(shp2, aes(X, Y, group=paste(PID, SID))) +
    geom_polygon(aes(fill=mu)) +
    theme_bw() + scale_x_continuous(NULL, expand=c(0, 0)) + scale_y_continuous(NULL, expand=c(0, 0)) +
    scale_fill_gradient2()

plot.kts("SLRDamages_d_percap_slr.csv")

df <- get.allts("RegionTemperature_rtl_realizedtemperature_absolute.csv")
byisos$baseline <- (byisos$Temp1980 + byisos$Temp2010) / 2
df2 <- df %>% left_join(byisos, by=c('country'='ISO3'))

gdf <- get.ts("GlobalTemperature_rt_g_globaltemperature.csv")
gdf$baseline <- 0.85 # 1980 - 2012 under AR5 / 1.5 report

ggplot(df2, aes(time, mu - baseline)) +
    geom_line(aes(colour="Other", group=country)) +
    ## geom_ribbon(data=subset(df2, country == 'KOR'), aes(ymin=ci25 - baseline, ymax=ci75 - baseline), alpha=.5, fill='#beaed4') +
    ## geom_line(data=subset(df2, country == 'KOR'), aes(colour="South Korea"), linewidth=2) +
    geom_line(data=gdf, aes(colour="Global Mean"), linewidth=2) +
    theme_bw() + scale_x_continuous(NULL, expand=c(0, 0)) + ylab("Change from 1980 - 2010 (° C)") +
    scale_colour_manual(NULL, breaks=c('Global Mean', 'Other', 'South Korea'), values=c('#1b9e77', '#80808080', '#7570b3'))
ggsave("../../output/figures/t_global_nokor.png", width=6.5, height=4)
