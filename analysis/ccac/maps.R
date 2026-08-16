setwd("~/research/iamup2/MimiPAGE2025.jl/analysis/ccac")

library(ggplot2)
library(dplyr)
source("~/projects/research-common/R/myPBSmapping.R")

get.difference <- function(prefix, name) {
    df <- read.csv(paste0(prefix, "-RCP4.5 & SSP2.csv")) %>%
        left_join(read.csv(paste0(prefix, "-RCP2.6 & SSP2.csv")), by=c('time', 'country'), suffix=c('.45', '.26'))
    df[, paste0(name, '.df')] <- df[, paste0(name, '.45')] - df[, paste0(name, '.26')]
    df
}

df <- get.difference("cromar-excess-deaths", "excess_deaths") %>%
    left_join(read.csv("../national/pop_1000s.csv")) %>%
    mutate(excess_deathrate=excess_deaths.df / (pop_population * 1e6))

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

polydata3 <- polydata2 %>% left_join(subset(df, time == 2100), by=c('code'='country'))
shp2 <- shp %>% left_join(polydata3[, c('PID', 'excess_deathrate')])

ggplot(shp2, aes(X, Y, group=paste(PID, SID))) +
    geom_polygon(aes(fill=excess_deathrate * 1e5)) +
    theme_bw() + scale_x_continuous(NULL, expand=c(0, 0)) + scale_y_continuous(NULL, expand=c(0, 0)) +
    scale_fill_gradient("Additional excess\ndeaths / 100k", low="yellow", high=scales::muted("red"))

df <- get.difference("d_market", "isat_per_cap_ImpactperCapinclSaturationandAdaptation")

polydata3 <- polydata2 %>% left_join(subset(df, time == 2100), by=c('code'='country'))
shp2 <- shp %>% left_join(polydata3[, c('PID', 'isat_per_cap_ImpactperCapinclSaturationandAdaptation.df')])

ggplot(shp2, aes(X, Y, group=paste(PID, SID))) +
    geom_polygon(aes(fill=pmin(5000, pmax(-5000, isat_per_cap_ImpactperCapinclSaturationandAdaptation.df)))) +
    theme_bw() + scale_x_continuous(NULL, expand=c(0, 0)) + scale_y_continuous(NULL, expand=c(0, 0)) +
    scale_fill_gradient2("Additional per capita\nmarket damages\n($/person)", low="blue",
                         mid="yellow", high=scales::muted("red"))

df <- get.difference("d_nonmarket", "isat_per_cap_ImpactperCapinclSaturationandAdaptation")

polydata3 <- polydata2 %>% left_join(subset(df, time == 2100), by=c('code'='country'))
shp2 <- shp %>% left_join(polydata3[, c('PID', 'isat_per_cap_ImpactperCapinclSaturationandAdaptation.df')])

ggplot(shp2, aes(X, Y, group=paste(PID, SID))) +
    geom_polygon(aes(fill=pmin(10000, isat_per_cap_ImpactperCapinclSaturationandAdaptation.df))) +
    theme_bw() + scale_x_continuous(NULL, expand=c(0, 0)) + scale_y_continuous(NULL, expand=c(0, 0)) +
    scale_fill_gradient2("Additional per capita\nnon-market damages\n($/person)", low="blue",
                         mid="yellow", high=scales::muted("red"))

df <- get.difference("d_slr", "d_slr") %>%
    left_join(read.csv("../national/pop_1000s.csv")) %>%
    mutate(d_slr.pc=d_slr.df / pop_population)
df$d_slr.pc[df$d_slr.pc == 0] <- NA

polydata3 <- polydata2 %>% left_join(subset(df, time == 2100), by=c('code'='country'))
shp2 <- shp %>% left_join(polydata3[, c('PID', 'd_slr.pc')])

ggplot(shp2, aes(X, Y, group=paste(PID, SID))) +
    geom_polygon(aes(fill=pmax(1e-6, d_slr.pc))) +
    theme_bw() + scale_x_continuous(NULL, expand=c(0, 0)) + scale_y_continuous(NULL, expand=c(0, 0)) +
    scale_fill_gradient("SLR damages\nper capita\n($/person)", low="yellow", high=scales::muted("red"),
                        trans='log10')

df <- get.difference("abateco2-tc", "tc_totalcost_national") %>%
    left_join(read.csv("../national/pop_1000s.csv")) %>%
    mutate(tc_totalcost_national.pc=tc_totalcost_national.df / pop_population)

polydata3 <- polydata2 %>% left_join(subset(df, time == 2030), by=c('code'='country'))
shp2 <- shp %>% left_join(polydata3[, c('PID', 'tc_totalcost_national.pc')])

ggplot(shp2, aes(X, Y, group=paste(PID, SID))) +
    geom_polygon(aes(fill=pmax(-4000, tc_totalcost_national.pc))) +
    theme_bw() + scale_x_continuous(NULL, expand=c(0, 0)) + scale_y_continuous(NULL, expand=c(0, 0)) +
    scale_fill_gradient("CO2 Abatement Costs\nper capita\n($/person)", low="blue",
                        high="yellow")

df <- get.difference("pmmarket-dlngdp", "dlngdp")

