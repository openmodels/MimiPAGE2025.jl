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

pagedf <- read.csv("output/PM25Pollution_pm_total.csv")
popdf <- read.csv("output/Population_pop_population.csv")

pagedf2 <- pagedf %>% left_join(popdf, by=c('time', 'country', 'trialnum')) %>% group_by(time, trialnum) %>%
    summarize(pm_total=sum(pm_total * pop_population, na.rm=T) / sum(pop_population[!is.na(pm_total)], na.rm=T)) %>% group_by(time) %>%
    summarize(mu=mean(pm_total, na.rm=T), ci25=quantile(pm_total, .25, na.rm=T), ci75=quantile(pm_total, .75, na.rm=T))

ggplot(subset(pagedf2, time <= 2050), aes(time, mu)) +
    geom_line() + geom_ribbon(aes(ymin=ci25, ymax=ci75, alpha=.5)) +
    theme_bw() + scale_y_log10("Anthropogenic PM2.5 (um/m3)") + xlab(NULL)

pagedf2 <- pagedf %>% group_by(country, trialnum) %>% mutate(pm_ratio=pm_total / first(pm_total)) %>%
    left_join(mapping, by=c('country'='ISO3')) %>% group_by(REGION_4LETTER, REGION_4LETTER_LABEL, time, trialnum) %>%
    summarize(pm_ratio=mean(pm_ratio, na.rm=T)) %>% group_by(REGION_4LETTER, REGION_4LETTER_LABEL, time) %>%
    summarize(mu=mean(pm_ratio, na.rm=T), ci25=quantile(pm_ratio, .25, na.rm=T), ci75=quantile(pm_ratio, .75, na.rm=T))

pagedf2$label <- ifelse(pagedf2$REGION_4LETTER_LABEL %in% c('United States of America', 'China', 'Austria', 'Russian Federation', 'Central America', 'Remaining Southern Africa'),
                        pagedf2$REGION_4LETTER_LABEL, "Other")
pagedf2$label <- factor(pagedf2$label, c('United States of America', 'China', 'Austria', 'Russian Federation', 'Central America', 'Remaining Southern Africa', 'Other'))

ggplot(subset(pagedf2, time <= 2050), aes(time, mu, group=REGION_4LETTER)) +
    geom_line(data=subset(pagedf2, time <= 2050 & label == 'Other'), aes(colour=label)) +
    geom_line(data=subset(pagedf2, time <= 2050 & label != 'Other'), aes(colour=label)) +
    geom_ribbon(data=subset(pagedf2, time <= 2050 & label != 'Other'), aes(ymin=ci25, ymax=ci75), alpha=.5) +
    scale_colour_manual(NULL, breaks=c('United States of America', 'China', 'Austria', 'Russian Federation', 'Central America', 'Remaining Southern Africa', 'Other'),
                        values=c('#1b9e77', '#d95f02', '#7570b3', '#e7298a', '#66a61e', '#e6ab02', '#80808030')) +
    theme_bw() + scale_y_continuous("Ratio in Anthropogenic PM2.5 vs. 2020") + xlab(NULL)


pattern <- read.csv("../../data/pollution/export_pattern.csv")

ggplot(pattern, aes(Source, REGION_4LETTER, fill=PM25_Contribution)) +
    geom_raster() +
    scale_fill_distiller(palette="Reds", direction=1, trans='log10') +
    xlab("Source") + ylab("Recipient") +
    theme_bw() +
    theme(text=element_text(size=4), axis.text.x=element_text(angle=90, vjust=0.5, hjust=1))
ggsave("export_pattern.png", width=6, height=5)
