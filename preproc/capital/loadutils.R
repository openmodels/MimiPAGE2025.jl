library(readxl)
library(reshape2)

read.wb <- function(filepath, value.name) {
    df <- read_xls(filepath, skip=3)
    df2 <- melt(df[, c(-1, -3, -4)], 'Country Code', variable.name='Year', value.name=value.name)
    df2$Year <- as.numeric(as.character(df2$Year))
    df2
}

load.gdp3 <- function() {
    df.gdp2 <- read.wb("data/API_NY.GDP.MKTP.KD_DS2_en_excel_v2_5871893.xls", 'GDP.2015')
    df.gdp3 <- subset(df.gdp2, `Country Code` %in% unique(df.gdp2$`Country Code`[!is.na(df.gdp2$GDP.2015)]) & !(`Country Code` %in% c("LIE", 'NCL'))) %>% group_by(`Country Code`) %>%
        reframe(Year=Year, GDP.2015.est=approx(Year, GDP.2015, Year, rule=2)$y)
    df.gdp3$GDP.2019.est <- df.gdp3$GDP.2015.est * 106.87654 / 100

    df.gdp3.last <- subset(df.gdp3, Year == 2022)
    df.gdp3.last$Year <- 2023
    df.gdp3 <- rbind(df.gdp3, df.gdp3.last)

    df.gdp3
}
