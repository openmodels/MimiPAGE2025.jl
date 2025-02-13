library(dplyr)
library(countrycode)
source("loadutils.R")

df.gdp3 <- load.gdp3()

read.iw <- function(filepath, value.name) {
    df.pro <- read.csv(filepath)
    df.pro$ISO <- countryname(df.pro$Country, 'iso3c')
    df.pro2 <- melt(df.pro[, -1:-2], 'ISO', variable.name='XYear', value.name=value.name)
    df.pro2$Year <- sapply(df.pro2$XYear, function(ss) as.numeric(substring(ss, 2, 5)))
    df.pro2
}

load.solowdata <- function() {
    df.gdp2 <- read.wb("data/API_NY.GDP.MKTP.KD_DS2_en_excel_v2_5871893.xls", 'GDP.2015')
    df.gdp2$GDP.2005 <- df.gdp2$GDP.2015 * 83.6 / 100
    df.lab2 <- read.wb("data/API_SL.TLF.TOTL.IN_DS2_en_excel_v2_5871833.xls", 'Labor')
    df.pop2 <- read.wb("data/API_SP.POP.TOTL_DS2_en_excel_v2_5871620.xls", 'Population')
    df.pop2.last <- subset(df.pop2, Year == 2022)
    df.pop2.last$Year <- 2023
    df.pop2 <- rbind(df.pop2, df.pop2.last)
    df.sav2 <- read.wb("data/API_NY.GNS.ICTR.ZS_DS2_en_excel_v2_5871648.xls", 'SavingRate')
    df.nat2 <- read.wb("data/API_NV.AGR.TOTL.ZS_DS2_en_excel_v2_5871737.xls", 'NaturalGDP')

    df.pro <- read.csv("data/tabula-C-produced.csv")
    df.pro$ISO <- factor(countryname(df.pro$Country, 'iso3c'))
    df.pro2 <- read.iw("data/tabula-C-produced.csv", 'Produced Capital')
    df.hum2 <- read.iw("data/tabula-B-human.csv", 'Human Capital')
    df.non2 <- read.iw("data/tabula-A1-nonrenewable.csv", 'Nonrenewable Capital')
    df.ren2 <- read.iw("data/tabula-A2-renewable.csv", 'Renewable Capital')

    df <- expand.grid(ISO=levels(df.pro$ISO), Year=1990:2020) %>%
        left_join(df.gdp2, by=c('Year', 'ISO'='Country Code')) %>%
        left_join(df.pro2[, c('ISO', 'Year', 'Produced Capital')], by=c('Year', 'ISO')) %>%
        left_join(df.hum2[, c('ISO', 'Year', 'Human Capital')], by=c('Year', 'ISO')) %>%
        left_join(df.non2[, c('ISO', 'Year', 'Nonrenewable Capital')], by=c('Year', 'ISO')) %>%
        left_join(df.ren2[, c('ISO', 'Year', 'Renewable Capital')], by=c('Year', 'ISO')) %>%
        left_join(df.lab2, by=c('Year', 'ISO'='Country Code')) %>%
        left_join(df.pop2, by=c('Year', 'ISO'='Country Code')) %>%
        left_join(df.sav2, by=c('Year', 'ISO'='Country Code')) %>%
        left_join(df.nat2, by=c('Year', 'ISO'='Country Code'))
    df$ISO <- factor(df$ISO, levels=levels(df.pro$ISO))

    df2 <- subset(df, !is.na(ISO)) %>% group_by(ISO) %>%
        reframe(Year=Year, denom=min(Population, na.rm=T), GDP.2005=GDP.2005 / denom,
                `Produced Capital`=1e9 * `Produced Capital` / denom,
                `Per Person Human Capital`=1e9 * `Human Capital` / Population, # Note: Per person
                `Nonrenewable Capital`=1e9 * `Nonrenewable Capital` / denom,
                `Renewable Capital`=1e9 * `Renewable Capital` / denom,
                Labor=Labor / denom, Population=Population / denom,
                SavingRate=SavingRate / 100, NaturalGDP=NaturalGDP / 100)

    assign("df", df, envir = .GlobalEnv)
    assign("df2", df2, envir = .GlobalEnv)
}

make.stan.data <- function(iso) {
    status.1990 <- df2[df2$Year == 1990 & df2$ISO == iso,]

    stan.data <- list(T=diff(range(df2$Year)) + 1,
                      pop=df2$Population[df2$ISO == iso],

                      maxrencap0=2 * status.1990$`Renewable Capital` + 1,
                      maxprocap0=status.1990$`Produced Capital` + 1,
                      maxhumcap0=status.1990$`Per Person Human Capital` + 1,

                      N1=sum(!is.na(df2$GDP.2005) & df2$ISO == iso),
                      gdp=df2$GDP.2005[!is.na(df2$GDP.2005) & df2$ISO == iso],
                      gdp_year=df2$Year[!is.na(df2$GDP.2005) & df2$ISO == iso] - 1989,

                      N2=sum(!is.na(df2$`Produced Capital`) & df2$ISO == iso),
                      rencap=df2$`Renewable Capital`[!is.na(df2$`Produced Capital`) & df2$ISO == iso],
                      procap=df2$`Produced Capital`[!is.na(df2$`Produced Capital`) & df2$ISO == iso],
                      humcap=df2$`Per Person Human Capital`[!is.na(df2$`Produced Capital`) & df2$ISO == iso],
                      cap_year=df2$Year[!is.na(df2$`Produced Capital`) & df2$ISO == iso] - 1989,

                      N3=sum(!is.na(df2$NaturalGDP) & df2$ISO == iso),
                      natgdp=df2$NaturalGDP[!is.na(df2$NaturalGDP) & df2$ISO == iso],
                      natgdp_year=df2$Year[!is.na(df2$NaturalGDP) & df2$ISO == iso] - 1989,

                      deprrate_prior=0.05)

    stan.data$rencap[is.na(stan.data$rencap) | stan.data$rencap == 0] <- 0.1

    if (sum(!is.na(df2$SavingRate) & df2$ISO == iso) == 0) {
        rows <- df2[sample(which(!is.na(df2$SavingRate)), 10),]
        stan.data$N4 <- 10
        stan.data$sav <- rows$SavingRate
        stan.data$sav_year <- rows$Year - 1989
    } else {
        stan.data$N4 <- sum(!is.na(df2$SavingRate) & df2$ISO == iso)
        stan.data$sav <- df2$SavingRate[!is.na(df2$SavingRate) & df2$ISO == iso]
        stan.data$sav_year <- df2$Year[!is.na(df2$SavingRate) & df2$ISO == iso] - 1989
    }

    stan.data
}

model.solow.prodonly <- function(la, stan.data) {
    product <- matrix(0, 4000, stan.data$T-1)
    procap_model <- matrix(NA, 4000, stan.data$T)

    procap_model[, 1] = la$procap0part * stan.data$maxprocap0

    for (tt in 2:stan.data$T) {
        product[, tt-1] = (la$tfp + la$dtfpdt * (tt-1)) * (procap_model[, tt-1]^(la$shares0[, 1] + (tt-2) * (la$sharesT[, 1] - la$shares0[, 1]) / (stan.data$T-2))) * (stan.data$pop[tt-1]^(la$shares0[, 2] + (tt-2) * (la$sharesT[, 2] - la$shares0[, 2]) / (stan.data$T-2)))
        procap_model[, tt] = procap_model[, tt-1] + (la$saverate0 + la$dsaveratedt * (tt-2)) * product[, tt-1] - la$deprrate * procap_model[, tt-1]
        if (any(is.na(product[, tt-1])))
            print(c("Product", tt))
        if (any(is.na(procap_model[, tt])))
            print(c("ProCap", tt))
    }

    list("product"=product, "procap_model"=procap_model)
}
