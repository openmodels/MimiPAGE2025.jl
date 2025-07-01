setwd("~/research/iamup2/MimiPAGE2025.jl/analysis/pm25")

library(readxl)
library(ggplot2)
library(dplyr)

df = read.csv("/Users/admin/Library/CloudStorage/GoogleDrive-jrising@udel.edu/My Drive/Research/CCAC/analysis/gainssim.csv")
df$label <- ifelse(df$REGION_4LETTER_LABEL %in% c('United States of America', 'China', 'Austria', 'Russian Federation', 'Brazil', 'Northern Africa (excl Egypt)'),
                   df$REGION_4LETTER_LABEL, "Other")
df$label <- factor(df$label, c('United States of America', 'China', 'Austria', 'Russian Federation', 'Brazil', 'Northern Africa (excl Egypt)', 'Other'))

ggplot(subset(df, IDSCENARIOS == "Baseline_CLE"), aes(IDYEARS, PM25_ANTHROP, group=REGION_4LETTER, colour=label)) +
    geom_line(data=subset(df, IDSCENARIOS == "Baseline_CLE" & label == 'Other')) +
    geom_line(data=subset(df, IDSCENARIOS == "Baseline_CLE" & label != 'Other')) +
    scale_colour_manual(NULL, breaks=c('United States of America', 'China', 'Austria', 'Russian Federation', 'Brazil', 'Northern Africa (excl Egypt)', 'Other'),
                        values=c('#1b9e77', '#d95f02', '#7570b3', '#e7298a', '#66a61e', '#e6ab02', '#80808030')) +
    theme_bw() + scale_y_log10("Anthropogenic PM2.5 (um/m3)") + xlab(NULL)

mapping <- read.csv("../../data/pollution/GAINS_4letter_regions_mapping.csv")

for (scenario in c('Decarb_MTFR', 'Baseline_CLE')) {
    pdf <- cbind(rbind(subset(df, IDSCENARIOS == "Baseline_CLE" & IDYEARS < 2030), subset(df, IDSCENARIOS == scenario & IDYEARS >= 2030))[, c('REGION_4LETTER', 'REGION_4LETTER_LABEL', 'IDYEARS', 'PM25_ANTHROP', 'label')], Model="GAINS")
    names(pdf)[3:4] <- c('time', 'pm_total')
    for (useekc in c(F, T)) {
        for (ssp in c('SSP2', 'SSP3')) {
            if (!useekc && ssp == 'SSP3')
                next
            pagedf <- read.csv(paste0("pmtotal_", ssp, "_", scenario, "_", ifelse(useekc, "true", "false"), ".csv"))
            pagedf2 <- pagedf %>% left_join(mapping, by=c('country'='ISO3')) %>% group_by(REGION_4LETTER, REGION_4LETTER_LABEL, time) %>%
                summarize(pm_total=mean(pm_total))
            pagedf2$label <- ifelse(pagedf2$REGION_4LETTER_LABEL %in% c('United States of America', 'China', 'Austria', 'Russian Federation', 'Brazil', 'Northern Africa (excl Egypt)'),
                                    pagedf2$REGION_4LETTER_LABEL, "Other")
            pagedf2$label <- factor(pagedf2$label, c('United States of America', 'China', 'Austria', 'Russian Federation', 'Brazil', 'Northern Africa (excl Egypt)', 'Other'))

            pdf <- rbind(pdf, cbind(pagedf2, Model=paste("PAGE", ifelse(useekc, paste(ssp, "With EKC"), "No EKC"))))
        }
    }

    ggplot(subset(pdf, time <= 2050), aes(time, pm_total, group=REGION_4LETTER, colour=label)) +
        facet_wrap(~ Model) +
        geom_line(data=subset(pdf, time <= 2050 & label == 'Other')) +
        geom_line(data=subset(pdf, time <= 2050 & label != 'Other')) +
        scale_colour_manual(NULL, breaks=c('United States of America', 'China', 'Austria', 'Russian Federation', 'Brazil', 'Northern Africa (excl Egypt)', 'Other'),
                            values=c('#1b9e77', '#d95f02', '#7570b3', '#e7298a', '#66a61e', '#e6ab02', '#80808030')) +
        theme_bw() + scale_y_log10("Anthropogenic PM2.5 (um/m3)") + xlab(NULL)
}

scendf <- cbind(rbind(subset(df, IDSCENARIOS == "Baseline_CLE" & IDYEARS < 2030), subset(df, IDSCENARIOS == scenario & IDYEARS >= 2030))[, c('REGION_4LETTER', 'REGION_4LETTER_LABEL', 'IDYEARS', 'PM25_ANTHROP', 'label')], Model="GAINS")
ggplot(subset(scendf, IDYEARS <= 2050), aes(IDYEARS, PM25_ANTHROP, group=REGION_4LETTER, colour=label)) +
    facet_wrap(~ Model) +
    geom_line(data=subset(scendf, IDYEARS <= 2050 & label == 'Other')) +
    geom_line(data=subset(scendf, IDYEARS <= 2050 & label != 'Other')) +
    scale_colour_manual(NULL, breaks=c('United States of America', 'China', 'Austria', 'Russian Federation', 'Brazil', 'Northern Africa (excl Egypt)', 'Other'),
                        values=c('#1b9e77', '#d95f02', '#7570b3', '#e7298a', '#66a61e', '#e6ab02', '#80808030')) +
    theme_bw() + scale_y_log10("Anthropogenic PM2.5 (um/m3)") + xlab(NULL)

## scendf <- rbind(subset(df, IDSCENARIOS == "Baseline_CLE" & IDYEARS < 2030), subset(df, IDSCENARIOS == "Decarb_MTFR" & IDYEARS >= 2030))
ggplot(scendf, aes(IDYEARS, PM25_ANTHROP, group=REGION_4LETTER, colour=label)) +
    geom_line(data=subset(scendf, label == 'Other')) +
    geom_line(data=subset(scendf, label != 'Other')) +
    scale_colour_manual(NULL, breaks=c('United States of America', 'China', 'Austria', 'Russian Federation', 'Brazil', 'Northern Africa (excl Egypt)', 'Other'),
                        values=c('#1b9e77', '#d95f02', '#7570b3', '#e7298a', '#66a61e', '#e6ab02', '#80808030')) +
    theme_bw() + scale_y_log10("Anthropogenic PM2.5 (um/m3)") + xlab(NULL)

pagedf <- read.csv("output_SSP2_Baseline_CLE_false/PM25Pollution_pm_total.csv")
popdf <- read.csv("output_SSP2_Baseline_CLE_false/Population_pop_population.csv")

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

pagedf2$label <- ifelse(pagedf2$REGION_4LETTER_LABEL %in% c('United States of America', 'China', 'Austria', 'Russian Federation', 'Brazil', 'Northern Africa (excl Egypt)'),
                        pagedf2$REGION_4LETTER_LABEL, "Other")
pagedf2$label <- factor(pagedf2$label, c('United States of America', 'China', 'Austria', 'Russian Federation', 'Brazil', 'Northern Africa (excl Egypt)', 'Other'))

ggplot(subset(pagedf2, time <= 2050), aes(time, mu, group=REGION_4LETTER)) +
    geom_line(data=subset(pagedf2, time <= 2050 & label == 'Other'), aes(colour=label)) +
    geom_line(data=subset(pagedf2, time <= 2050 & label != 'Other'), aes(colour=label)) +
    geom_ribbon(data=subset(pagedf2, time <= 2050 & label != 'Other'), aes(ymin=ci25, ymax=ci75), alpha=.5) +
    scale_colour_manual(NULL, breaks=c('United States of America', 'China', 'Austria', 'Russian Federation', 'Brazil', 'Northern Africa (excl Egypt)', 'Other'),
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
