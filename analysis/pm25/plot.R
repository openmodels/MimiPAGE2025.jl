setwd("~/research/iamup2/MimiPAGE2025.jl/analysis/pm25")

library(readxl)
library(ggplot2)
library(dplyr)

df = read.csv("gainssim.csv")
df$label <- ifelse(df$REGION_4LETTER_LABEL %in% c('United States of America', 'China', 'Austria', 'Russian Federation', 'Central America', 'Remaining Southern Africa'),
                   df$REGION_4LETTER_LABEL, "Other")
df$label <- factor(df$label, c('United States of America', 'China', 'Austria', 'Russian Federation', 'Central America', 'Remaining Southern Africa', 'Other'))

ggplot(subset(df, IDSCENARIOS == "Baseline"), aes(IDYEARS, PM25_ANTHROP, group=REGION_4LETTER, colour=label)) +
    geom_line(data=subset(df, IDSCENARIOS == "Baseline" & label == 'Other')) +
    geom_line(data=subset(df, IDSCENARIOS == "Baseline" & label != 'Other')) +
    scale_colour_manual(NULL, breaks=c('United States of America', 'China', 'Austria', 'Russian Federation', 'Central America', 'Remaining Southern Africa', 'Other'),
                        values=c('#1b9e77', '#d95f02', '#7570b3', '#e7298a', '#66a61e', '#e6ab02', '#80808030')) +
    theme_bw() + scale_y_log10("Anthropogenic PM2.5 (um/m3)") + xlab(NULL)

mapping <- read.csv("../../data/pollution/GAINS_4letter_regions_mapping.csv")

for (scenario in c('Decarb', 'Baseline')) {
    for (useekc in c(F, T)) {
        pagedf <- read.csv(paste0("pmtotal_", scenario, "_", useekc, ".csv"))
        pagedf2 <- pagedf %>% left_join(mapping, by=c('country'='ISO3')) %>% group_by(REGION_4LETTER, REGION_4LETTER_LABEL, time) %>%
            summarize(pm_total=mean(pm_total))
        pagedf2$label <- ifelse(pagedf2$REGION_4LETTER_LABEL %in% c('United States of America', 'China', 'Austria', 'Russian Federation', 'Central America', 'Remaining Southern Africa'),
                           pagedf2$REGION_4LETTER_LABEL, "Other")
        pagedf2$label <- factor(pagedf2$label, c('United States of America', 'China', 'Austria', 'Russian Federation', 'Central America', 'Remaining Southern Africa', 'Other'))

        ggplot(subset(pagedf2, time <= 2050), aes(time, pm_total, group=REGION_4LETTER, colour=label)) +
            geom_line(data=subset(pagedf2, time <= 2050 & label == 'Other')) +
            geom_line(data=subset(pagedf2, time <= 2050 & label != 'Other')) +
            scale_colour_manual(NULL, breaks=c('United States of America', 'China', 'Austria', 'Russian Federation', 'Central America', 'Remaining Southern Africa', 'Other'),
                        values=c('#1b9e77', '#d95f02', '#7570b3', '#e7298a', '#66a61e', '#e6ab02', '#80808030')) +
            theme_bw() + scale_y_log10("Anthropogenic PM2.5 (um/m3)") + xlab(NULL)

    }
}

scendf <- rbind(subset(df, IDSCENARIOS == "Baseline" & IDYEARS < 2030), subset(df, IDSCENARIOS == "Decarb" & IDYEARS >= 2030))
ggplot(scendf, aes(IDYEARS, PM25_ANTHROP, group=REGION_4LETTER, colour=label)) +
    geom_line(data=subset(scendf, label == 'Other')) +
    geom_line(data=subset(scendf, label != 'Other')) +
    scale_colour_manual(NULL, breaks=c('United States of America', 'China', 'Austria', 'Russian Federation', 'Central America', 'Remaining Southern Africa', 'Other'),
                        values=c('#1b9e77', '#d95f02', '#7570b3', '#e7298a', '#66a61e', '#e6ab02', '#80808030')) +
    theme_bw() + scale_y_log10("Anthropogenic PM2.5 (um/m3)") + xlab(NULL)
