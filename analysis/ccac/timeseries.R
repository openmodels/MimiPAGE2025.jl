setwd("~/research/iamup2/MimiPAGE2025.jl/analysis/ccac")

library(dplyr)
library(tidyr)
library(ggplot2)
library(ggforce)
library(ggpattern)
library(readxl)

do.global <- T
source("loader.R")
source("helpers.R")

gdps <- rbind(cbind(name="Baseline", read.csv("mainruns/gdp-Baseline.csv")),
              cbind(name="IntegratedAction", read.csv("mainruns/gdp-IntegratedAction.csv")),
              cbind(name="LTCAction", read.csv("mainruns/gdp-LTCAction.csv"))) %>%
    group_by(name, time) %>% summarize(gdp=sum(gdp))

## Get all years
pdf2 <- scen.diff.fracgdp("IntegratedAction", "Baseline")

ggplot(pdf2 %>% filter(time <= 2100, panel == 'Aggregate'), aes(time, diff, fill=label)) +
    geom_col_pattern(aes(pattern=channel), colour='black', linewidth=.05, pattern_density=.5,
                     pattern_spacing=.02, pattern_fill='black') +
    scale_pattern_manual(values=c('none', 'stripe', 'circle')) +
    theme_bw() + scale_y_continuous(labels=scales::percent)

## Spline interpolation for each
pdf3 <- pdf2 %>% filter(panel == 'Aggregate') %>%
    mutate(label=ifelse(prefix %in% c("d_market", "d_slr", "adaptall-acpc", "discontinuity-percap"), "Aggregate Market Damages", label),
           prefix=ifelse(label == "Aggregate Market Damages", 'd_market', prefix)) %>%
    group_by(name, label, channel, prefix, time) %>% summarize(diff=sum(diff)) %>%
    group_by(name, label, channel, prefix) %>% arrange(time) %>% reframe(year=seq(2025, 2100, by=5), diff=splinefun(c(2025, time), c(0, diff), method='monoH.FC')(year))
pdf3$label2 <- ifelse(pdf3$label == "Aggregate Market Damages", "Market Damages",
               ifelse(pdf3$label == "Aggregate Non-Market Damages", "Non-Market Damages", pdf3$label))
pdf3$label2 <- factor(pdf3$label2, levels=c('Market Damages', 'Non-Market Damages', 'Feedback', 'Costs'))

ggplot(pdf3, aes(year, diff, fill=label2)) +
    geom_col_pattern(aes(pattern=channel), colour='black', linewidth=.05, pattern_density=.5,
                     pattern_spacing=.02, pattern_fill='black') +
    scale_fill_discrete(name="Cost/benefit category:") +
    scale_pattern_manual(name="Impact pathway:", breaks=c('climate', 'combined', 'pollution'), labels=c('Climate', 'Feedback', 'Pollution'), values=c('none', 'stripe', 'circle')) +
    theme_bw() + scale_y_continuous("Potential avoided damages (All Measures)", labels=scales::percent) +
    xlab(NULL)
ggsave("timeseries-bycost.pdf", width=6.5, height=4)

## Market-only

pdf2 <- rbind(scen.diff.fracgdp("LTCAction", "Baseline") %>% mutate(name="Long-term climate solutions"),
              scen.diff.fracgdp("IntegratedAction", "LTCAction") %>% mutate(name="Clear air and\nnear-term climate solutions"))
pdf3 <- pdf2 %>% filter(panel == 'Aggregate') %>%
    mutate(label=ifelse(prefix %in% c("d_market", "d_slr", "adaptall-acpc", "discontinuity-percap"), "Aggregate Market Damages", label),
           prefix=ifelse(label == "Aggregate Market Damages", 'd_market', prefix)) %>%
    group_by(name, label, channel, prefix, time) %>% summarize(diff=sum(diff)) %>%
    group_by(name, label, channel, prefix) %>% arrange(time) %>% reframe(year=seq(2025, 2100, by=5), diff=splinefun(c(2025, time), c(0, diff), method='monoH.FC')(year))
pdf3$label2 <- ifelse(pdf3$label == "Aggregate Market Damages", "Market Damages",
               ifelse(pdf3$label == "Aggregate Non-Market Damages", "Non-Market Damages", pdf3$label))
pdf3$label2 <- factor(pdf3$label2, levels=c('Market Damages', 'Non-Market Damages', 'Feedback', 'Costs'))

pdf4 <- pdf3 %>% filter(label2 != "Non-Market Damages") %>%
    mutate(channel=ifelse(label2 == "Costs", "costs",
                   ifelse(channel == 'combined', 'climate', channel))) %>%
    group_by(name, channel, year) %>%
    summarize(diff=sum(diff))
ggplot(pdf4, aes(year, diff, fill=name)) +
    geom_col_pattern(aes(pattern=channel), colour='black', linewidth=.05, pattern_density=.5,
                     pattern_spacing=.02, pattern_fill='black') +
    scale_fill_discrete(name="Cost/benefit category:") +
    scale_pattern_manual(name="Impact pathway:", breaks=c('climate', 'pollution', 'costs'), labels=c('Climate', 'Pollution', 'Costs'), values=c('none', 'circle', 'none')) +
    theme_bw() + scale_y_continuous("Potential avoided damages (All Measures)", labels=scales::percent) +
    xlab(NULL)
ggsave("timeseries-byscenchan-market.pdf", width=6.5, height=4)

pdf4 <- pdf3 %>%
    mutate(channel=ifelse(label2 == "Costs", "costs",
                   ifelse(channel == 'combined', 'climate', channel))) %>%
    group_by(name, channel, year) %>%
    summarize(diff=sum(diff))
ggplot(pdf4, aes(year, diff, fill=name)) +
    geom_col_pattern(aes(pattern=channel), colour='black', linewidth=.05, pattern_density=.5,
                     pattern_spacing=.02, pattern_fill='black') +
    scale_fill_discrete(name="Cost/benefit category:") +
    scale_pattern_manual(name="Impact pathway:", breaks=c('climate', 'pollution', 'costs'), labels=c('Climate', 'Pollution', 'Costs'), values=c('none', 'circle', 'none')) +
    theme_bw() + scale_y_continuous("Potential avoided damages (All Measures)", labels=scales::percent) +
    xlab(NULL)
ggsave("timeseries-byscenchan-alldmg.pdf", width=6.5, height=4)

## Simplified

pdf4 <- pdf3 %>% filter(label2 != "Non-Market Damages") %>%
    mutate(channel=ifelse(label2 == "Costs", "costs",
                   ifelse(channel == 'combined', 'climate', channel))) %>%
    group_by(channel, year) %>%
    summarize(diff=sum(diff))
ggplot(pdf4, aes(year, diff, fill=channel)) +
    geom_col_pattern(aes(pattern=channel), colour='black', linewidth=.05, pattern_density=.5,
                     pattern_spacing=.02, pattern_fill='black') +
    scale_fill_manual(name="Costs and Benefits:", breaks=c('climate', 'pollution', 'costs'), labels=c('Climate', 'Pollution', 'Costs'), values=c('#e41a1c', '#4daf4a', '#377eb8')) +
    scale_pattern_manual(name="Costs and Benefits:", breaks=c('climate', 'pollution', 'costs'), labels=c('Climate', 'Pollution', 'Costs'), values=c('none', 'circle', 'none')) +
    theme_bw() + scale_y_continuous("Potential avoided damages (All Measures)", limits=c(-0.005, .04), labels=scales::percent) +
    xlab(NULL)
ggsave("timeseries-byscenchan-market-simple.pdf", width=6.5, height=4)

pdf4 <- pdf3 %>%
    mutate(channel=ifelse(label2 == "Costs", "costs",
                   ifelse(channel == 'combined', 'climate', channel))) %>%
    group_by(channel, year) %>%
    summarize(diff=sum(diff))
ggplot(pdf4, aes(year, diff, fill=channel)) +
    geom_col_pattern(aes(pattern=channel), colour='black', linewidth=.05, pattern_density=.5,
                     pattern_spacing=.02, pattern_fill='black') +
    scale_fill_manual(name="Costs and Benefits:", breaks=c('climate', 'pollution', 'costs'), labels=c('Climate', 'Pollution', 'Costs'), values=c('#e41a1c', '#4daf4a', '#377eb8')) +
    scale_pattern_manual(name="Costs and Benefits:", breaks=c('climate', 'pollution', 'costs'), labels=c('Climate', 'Pollution', 'Costs'), values=c('none', 'circle', 'none')) +
    theme_bw() + scale_y_continuous("Potential avoided damages (All Measures)", labels=scales::percent) +
    xlab(NULL)
ggsave("timeseries-byscenchan-alldmg-simple.pdf", width=6.5, height=4)

## Split by policy action

pdf2 <- rbind(scen.diff.fracgdp("LTCAction", "Baseline") %>% mutate(name="Long-term climate solutions"),
              scen.diff.fracgdp("IntegratedAction", "LTCAction") %>% mutate(name="Clear air and\nnear-term climate solutions"))

pdf3 <- pdf2 %>% filter(panel == 'Aggregate') %>%
    mutate(label=ifelse(label %in% c("Costs", "Feedback"), label, "Damages")) %>%
    group_by(name, channel, label, time) %>% summarize(diff=sum(diff)) %>%
    group_by(name, channel, label) %>% arrange(time) %>%
    reframe(year=seq(2025, 2100, by=5), diff=splinefun(c(2025, time), c(0, diff), method='monoH.FC')(year)) %>%
    mutate(fulllabel=ifelse(label %in% c('Costs', 'Feedback'), label, name)) %>%
    group_by(fulllabel, channel, year) %>% summarize(diff=sum(diff))
pdf3$fulllabel <- factor(pdf3$fulllabel, levels=c('Long-term climate solutions', 'Clear air and\nnear-term climate solutions', 'Feedback', 'Costs'))

ggplot(pdf3, aes(year, diff, fill=fulllabel)) +
    geom_col_pattern(aes(pattern=channel), colour='black', linewidth=.02, pattern_density=.25,
                     pattern_spacing=.02, pattern_fill='black') +
    scale_fill_discrete(name="Policy Category:") +
    scale_pattern_manual(name="Impact pathway:", breaks=c('climate', 'combined', 'pollution'), labels=c('Climate', 'Feedback', 'Pollution'), values=c('none', 'stripe', 'circle')) +
    theme_bw() + scale_y_continuous("Potential avoided damages (All Measures)", labels=scales::percent) +
    xlab(NULL) + theme(plot.margin=unit(c(5.5, 30, 5.5, 5.5), "pt"))
ggsave("timeseries-bypolicy.pdf", width=6.5, height=4)

## Simplified version-- total costs, total benefits

pdf3 <- rbind(scen.diff.fracgdp("LTCAction", "Baseline") %>% filter(panel == 'Aggregate') %>% mutate(name="Long-term climate solutions"),
              scen.diff.fracgdp("IntegratedAction", "LTCAction") %>% filter(panel == 'Aggregate') %>% mutate(name="Clean air and\nnear-term climate solutions")) %>%
    mutate(label=ifelse(label == "Costs", label, name)) %>%
    group_by(name, label, time) %>% summarize(diff=sum(diff)) %>%
    group_by(name, label) %>% arrange(time) %>%
    reframe(year=seq(2025, 2100, by=5), diff=splinefun(c(2025, time), c(0, diff), method='monoH.FC')(year))
pdf3$label <- factor(pdf3$label, levels=c("Costs", "Long-term climate solutions", "Clean air and\nnear-term climate solutions"))
pdf3 <- pdf3 %>% arrange(label)

## Numbers for text:
subset(pdf3, year %in% c(2050, 2100))

ggplot(pdf3, aes(year, diff, fill=label)) +
    geom_col() +
    scale_fill_manual(name="Avoided Losses:", breaks=c("Costs",
                                                       "Long-term climate solutions",
                                                       "Clean air and\nnear-term climate solutions"),
                      labels=c("Policy Costs",
                               "Long-term climate solutions",
                               "Clean air and\nnear-term climate solutions"),
                      values=c('#e31a1c', '#33a02c', '#1f78b4')) +
    theme_bw() + scale_y_continuous("Potential avoided damages (% Global GDP)", labels=scales::percent) +
    xlab(NULL)
ggsave("timeseries-total-simple.pdf", width=6.5, height=4)

## Construct BCA
pdf3 <- rbind(scen.diff.fracgdp("LTCAction", "Baseline") %>% filter(panel == 'Aggregate' & time <= 2100) %>% mutate(name="Due to Long-term Climate Action"),
              scen.diff.fracgdp("IntegratedAction", "Baseline") %>% filter(panel == 'Aggregate' & time <= 2100) %>% mutate(name="Due to Integrated Action")) %>%
    mutate(label=ifelse(label == 'Costs', 'Costs', 'Benefits')) %>%
    group_by(name, label, time) %>% summarize(diff=sum(diff)) %>%
    group_by(name, label) %>% arrange(time) %>%
    reframe(year=2025:2100, diff=splinefun(time, diff, method='monoH.FC')(year))

npvres <- data.frame()
for (name in unique(pdf3$name)) {
    subgdps <- subset(gdps, name == "Baseline")
    subgdps2 <- approx(c(2025, subgdps$time), c(0, subgdps$gdp), 2025:2100)

    for (label in c('Costs', 'Benefits')) {
        subpdf3 <- pdf3[pdf3$name == name & pdf3$label == label,]
        subpdf4 <- approx(subpdf3$year, subpdf3$diff, 2025:2100)
        npv <- sum(subgdps2$y * subpdf4$y * (1 / (1 + 0.03))^(subpdf4$x - 2025))

        npvres <- rbind(npvres, data.frame(name, label, npv))
    }
}

npvres %>% group_by(name) %>% summarize(benefits=npv[label != "Costs"], costs=-npv[label == "Costs"], bcr=benefits / costs)

pdf3 <- scen.diff.fracgdp("IntegratedAction", "Baseline") %>% filter(panel == 'Aggregate') %>%
    mutate(label=ifelse(label == "Costs", label,
                 ifelse(label == "Aggregate Non-Market Damages", "Non-Market Damages", "Market Damages"))) %>%
    group_by(name, label, time) %>% summarize(diff=sum(diff)) %>%
    group_by(name, label) %>% arrange(time) %>%
    reframe(year=seq(2025, 2100, by=5), diff=splinefun(c(2025, time), c(0, diff), method='monoH.FC')(year))
pdf3$label <- factor(pdf3$label, levels=c("Market Damages", "Market Feedbacks", "Non-Market Damages", "Costs"))

## Numbers for report:
subset(pdf3, year %in% c(2050, 2100))

ggplot(pdf3, aes(year, diff, fill=label)) +
    geom_col() +
    scale_fill_discrete(name="Avoided Losses:") +
    theme_bw() + scale_y_continuous("Potential avoided damages (% Global GDP)", labels=scales::percent) +
    xlab(NULL)
ggsave("timeseries-total-split.pdf", width=6.5, height=4)

pdf3 <- rbind(scen.diff.fracgdp("LTCAction", "Baseline") %>% filter(panel == 'Aggregate') %>% mutate(name="Due to decarbonization"),
              scen.diff.fracgdp("IntegratedAction", "LTCAction") %>% filter(panel == 'Aggregate') %>% mutate(name="Due to super\npollutant measures")) %>%
    mutate(label=ifelse(label == "Costs", label, name)) %>%
    group_by(name, label, time) %>% summarize(diff=sum(diff)) %>%
    group_by(name, label) %>% arrange(time) %>%
    reframe(year=seq(2025, 2100, by=5), diff=splinefun(c(2025, time), c(0, diff), method='monoH.FC')(year))

ggplot(pdf3, aes(year, diff, fill=label)) +
    geom_col() +
    scale_fill_discrete(name="Avoided Losses:") +
    theme_bw() + scale_y_continuous("Potential avoided damages (% Global GDP)", labels=scales::percent) +
    xlab(NULL)

for (compare in c('slcp-only', 'climate-only', 'total')) {
if (compare == 'slcp-only') {
    pdf2 <- scen.diff.fracgdp("IntegratedAction", "LTCAction")
} else if (compare == 'total') {
    pdf2 <- scen.diff.fracgdp("IntegratedAction", "Baseline")
} else {
    pdf2 <- scen.diff.fracgdp("LTCAction", "Baseline")
}

pdf3 <- pdf2 %>% filter(panel == 'Aggregate') %>%
    mutate(label=ifelse(label %in% c("Costs", "Feedback"), label, "Damages")) %>%
    group_by(name, channel, label, time) %>% summarize(diff=sum(diff)) %>%
    group_by(name, channel, label) %>% arrange(time) %>%
    reframe(year=seq(2025, 2100, by=5), diff=splinefun(c(2025, time), c(0, diff), method='monoH.FC')(year)) %>%
    mutate(fulllabel=ifelse(label == 'Costs', label,
                     ifelse(channel == 'climate', "Climate damages",
                     ifelse(channel == 'pollution', "Pollution damages", "other"))))
pdf3$fulllabel <- factor(pdf3$fulllabel, levels=c('Climate damages', 'Pollution damages', 'Costs'))

## Numbers for text:
subset(pdf3, year %in% c(2050, 2100))

gp <- ggplot(pdf3 %>% filter(fulllabel != 'other'), aes(year, diff, fill=fulllabel)) +
    geom_col() +
    scale_fill_manual(name="Avoided Losses:", breaks=c('Climate damages', 'Pollution damages', 'Costs'), values=c('#a6761d', '#7570b3', '#e7298a')) +
    theme_bw() +
    xlab(NULL)
if (compare == 'slcp-only') {
    ggsave("timeseries-slcp-only.pdf",
           gp + scale_y_continuous("Potential avoided damages (% Global GDP)", limits=c(-.0025, .03), labels=scales::percent),
           width=6.5, height=4)
} else if (compare == 'total') {
    ggsave("timeseries-total-bydmg.pdf",
           gp + scale_y_continuous("Potential avoided damages (% Global GDP)", labels=scales::percent),
           width=6.5, height=4)
} else {
    ggsave("timeseries-decarb-only.pdf",
           gp + scale_y_continuous("Potential avoided damages (% Global GDP)", labels=scales::percent),
           width=6.5, height=4)
}

## Also include market, non-market split

pdf3 <- pdf2 %>% filter(panel == 'Aggregate') %>%
    mutate(label=ifelse(label %in% c("Costs", "Feedback", "Aggregate Non-Market Damages"), label, "Aggregate Market Damages")) %>%
    group_by(name, channel, label, time) %>% summarize(diff=sum(diff)) %>%
    group_by(name, channel, label) %>% arrange(time) %>%
    reframe(year=seq(2025, 2100, by=5), diff=splinefun(c(2025, time), c(0, diff), method='monoH.FC')(year)) %>%
    mutate(fulllabel=ifelse(label == 'Costs', label,
                     ifelse(channel == 'climate', "Climate damages",
                     ifelse(channel == 'pollution', "Pollution damages", "other"))))
pdf3$fulllabel <- factor(pdf3$fulllabel, levels=c('Climate damages', 'Pollution damages', 'Costs'))

ggplot(pdf3 %>% filter(fulllabel != 'other'), aes(year, diff)) +
    geom_col_pattern(aes(pattern=channel, fill=label), colour='black', linewidth=.05, pattern_density=.5,
                     pattern_spacing=.02, pattern_fill='black') +
    scale_pattern_manual(name="Impact pathway:", values=c('none', 'circle'), label=c("Climate", "Pollution")) +
    scale_fill_discrete(name="Avoided Losses:") +
    theme_bw() + scale_y_continuous("Potential avoided damages (% Global GDP)", labels=scales::percent) +
    xlab(NULL)
if (compare == 'slcp-only') {
    ggsave("timeseries-slcp-only-split.pdf", width=6.5, height=4)
} else if (compare == 'total') {
    ggsave("timeseries-total-split2.pdf", width=6.5, height=4)
} else {
    ggsave("timeseries-decarb-only-split.pdf", width=6.5, height=4)
}
}

## Look at the effects of delays

gdps <- rbind(cbind(name="IntegratedAction", read.csv("mainruns/gdp-IntegratedAction.csv")),
              cbind(name="IntegratedAction_delay", read.csv("mainruns/gdp-IntegratedAction_delay.csv"))) %>%
    group_by(name, time) %>% summarize(gdp=sum(gdp))

pdf2 <- scen.diff.fracgdp("IntegratedAction_delay", "IntegratedAction")

ggplot(pdf2 %>% filter(time <= 2100, panel == 'Aggregate'), aes(time, diff, fill=label)) +
    geom_col_pattern(aes(pattern=channel), colour='black', linewidth=.05, pattern_density=.5,
                     pattern_spacing=.02, pattern_fill='black') +
    scale_pattern_manual(values=c('none', 'stripe', 'circle')) +
    theme_bw() + scale_y_continuous(labels=scales::percent)

pdf3 <- pdf2 %>% filter(panel == 'Aggregate' & label != "Costs") %>%
    mutate(label=ifelse(prefix %in% c("d_market", "d_slr", "adaptall-acpc", "discontinuity-percap"), "Aggregate Market Damages", label),
           prefix=ifelse(label == "Aggregate Market Damages", 'd_market', prefix)) %>%
    group_by(name, label, channel, prefix, time) %>% summarize(diff=sum(diff)) %>%
    group_by(name, label, channel, prefix) %>% reframe(year=seq(2025, 2100, by=5), diff=approxfun(time, diff)(year))
pdf3$label2 <- ifelse(pdf3$label == "Aggregate Market Damages", "Market Damages",
               ifelse(pdf3$label == "Aggregate Non-Market Damages", "Non-Market Damages", pdf3$label))
pdf3$label2 <- factor(pdf3$label2, levels=c('Market Damages', 'Non-Market Damages', 'Feedback', 'Costs'))

ggplot(subset(pdf3, year <= 2100), aes(year, -diff, fill=label2)) +
    geom_col_pattern(aes(pattern=channel), colour='black', linewidth=.05, pattern_density=.5,
                     pattern_spacing=.02, pattern_fill='black') +
    scale_fill_discrete(name="Cost/benefit category:") +
    scale_pattern_manual(name="Impact pathway:", breaks=c('climate', 'combined', 'pollution'), labels=c('Climate', 'Feedback', 'Pollution'), values=c('none', 'stripe', 'circle')) +
    theme_bw() + scale_y_continuous("Potential avoided damages (Delay - No-delay)", labels=scales::percent) +
    xlab(NULL)
ggsave("timeseries-delays.pdf", width=6.5, height=4)

## Generate damages for each bottom-up result

for (prfx in c('pmmarket-percchg', 'd_slr', unique(pdf$prefix[pdf$panel == 'Bottom-up']))) {
    pdf2 <- rbind(scen.diff.fracgdp("LTCAction", "Baseline") %>% filter(prefix == prfx) %>% mutate(name="Decarbonization"),
                  scen.diff.fracgdp("IntegratedAction", "LTCAction") %>% filter(prefix == prfx) %>% mutate(name="Pollution Control"))
    pdf3 <- pdf2 %>% group_by(name) %>% reframe(year=seq(2025, 2100, by=5), diff=approxfun(time, diff)(year))

    ggplot(pdf3, aes(year, diff, fill=name)) +
        geom_col() +
        scale_fill_discrete(name="Policy Category:") +
        theme_bw() + scale_y_continuous("Potential avoided damages (CLE - APC)", labels=scales::percent) +
        xlab(NULL) + ggtitle(paste(pdf2$label[1], "from", pdf2$channel[1]))
    ggsave(paste0("timeseries-", prfx, ".pdf"), width=6.5, height=4)
}

