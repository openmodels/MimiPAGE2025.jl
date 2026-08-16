setwd("~/research/iamup2/MimiPAGE2025.jl/analysis/ccac")

library(dplyr)
library(tidyr)
library(ggplot2)
library(ggforce)
library(ggpattern)
library(readxl)

do.global <- F
source("loader.R")
source("helpers.R")

wbregions <- read_excel("regiongroups_WB_global_flagship_2024.xlsx")

pdf2 <- pdf %>% filter(time == 2050) %>% left_join(gainsregions, by=c('country'='ISO3')) %>%
    left_join(wbregions, by='REGION_4LETTER') %>% left_join(gdps, by=c('time', 'country', 'name')) %>%
    group_by(name, prefix, LABEL_REGIONGROUP, scenario_emt) %>%
    summarize(cost=sum(cost) / sum(1e6 * gdp)) %>%
    filter(name %in% c("Baseline", "IntegratedAction")) %>%
    mutate(name="Cost") %>%
    pivot_wider(names_from=c(scenario_emt), values_from=cost) %>%
    mutate(diff=Baseline - `Integrated Action`) %>% left_join(infomap, by='prefix')
pdf2$panel <- ifelse(pdf2$prefix %in% c("abateco2-tc", "d_market", "d_nonmarket", "pmmarket-percchg", 'pm-nonmarket', 'apcosts', 'WBRC-infrastructure_cost'),
                     'Aggregate', 'Bottom-up')

ggplot(pdf2, aes(diff, LABEL_REGIONGROUP, fill=label)) +
    facet_wrap(~ panel) +
    geom_col_pattern(aes(pattern=channel), colour='black', linewidth=.05) +
    scale_pattern_manual(values=c('none', 'stripe', 'circle')) +
    theme_bw() + scale_x_continuous(labels=scales::percent)

## Get 2035, 2050 and 2100
pdf2 <- pdf %>% filter(name %in% c("Baseline", "IntegratedAction") & time %in% c(2035, 2050, 2100)) %>%
    left_join(gdps, by=c('time', 'name', 'country')) %>%
    left_join(gainsregions, by=c('country'='ISO3')) %>%
    left_join(wbregions, by='REGION_4LETTER') %>%
    group_by(scenario_emt, time, prefix, LABEL_REGIONGROUP) %>%
    summarize(name="Cost", cost=sum(cost) / sum(gdp * 1e6)) %>%
    pivot_wider(names_from=c(scenario_emt), values_from=cost) %>%
    mutate(diff=Baseline - `Integrated Action`) %>% left_join(infomap, by='prefix')
pdf2$panel <- ifelse(pdf2$prefix %in% c("abateco2-tc", "pmmarket-percchg"), "Other",
              ifelse(pdf2$prefix %in% c("abateall-tcpc", "adaptall-acpc", "discontinuity-percap", "d_market", "d_nonmarket", 'pm-nonmarket', 'pm-market', 'apcosts', "d_slr", 'capitalloss', 'WBRC-infrastructure_cost'),
                     'Aggregate', 'Bottom-up'))

pdf3 <- pdf2 %>% filter(panel == 'Aggregate') %>%
    mutate(label=ifelse(prefix %in% c("d_market", "d_slr", "adaptall-acpc", "discontinuity-percap"), "Aggregate Market Damages", label),
           prefix=ifelse(label == "Aggregate Market Damages", 'd_market', prefix)) %>%
    group_by(prefix, LABEL_REGIONGROUP, label, channel, time) %>% summarize(diff=sum(diff))
pdf3$label2 <- ifelse(pdf3$label == "Aggregate Market Damages", "Market Damages",
               ifelse(pdf3$label == "Aggregate Non-Market Damages", "Non-Market Damages", pdf3$label))
pdf3$label2 <- factor(pdf3$label2, levels=c('Market Damages', 'Non-Market Damages', 'Feedback', 'Costs'))

ggplot(pdf3 %>% filter(!is.na(LABEL_REGIONGROUP)), aes(diff, LABEL_REGIONGROUP, fill=label2)) +
    facet_wrap(~ time, space='free_x', scales='free_x') +
    geom_col_pattern(aes(pattern=channel), colour='black', linewidth=.02, pattern_density=.25,
                     pattern_spacing=.02, pattern_fill='black') +
    scale_fill_discrete(name="Cost/benefit category:") +
    scale_pattern_manual(name="Impact pathway:", breaks=c('climate', 'combined', 'pollution'), labels=c('Climate', 'Feedback', 'Pollution'), values=c('none', 'stripe', 'circle')) +
    theme_bw() + scale_x_continuous("Potential avoided damages (CLE - APC)", labels=scales::percent) +
    ylab(NULL)
ggsave("regional-bycost.pdf", width=6.5, height=4)

## Part-Simplifed
pdf4 <- pdf3 %>% mutate(channel=ifelse(label2 == 'Costs', 'combined', ifelse(label2 == "Feedback", "climate", channel)),
                        label3=ifelse(label2 == "Feedback", "Market Damages", as.character(label2)),
                        stackgroup=ifelse(label3 == "Costs", "Costs", "Other"),
                        LABEL_REGIONGROUP=factor(LABEL_REGIONGROUP, levels=unique(LABEL_REGIONGROUP))) %>%
    arrange(label3, desc(channel)) %>% group_by(stackgroup, LABEL_REGIONGROUP, sign(diff), time) %>%
    mutate(xmin=c(0, cumsum(diff[-length(diff)])), xmax=cumsum(diff))

ggplot(pdf4 %>% filter(!is.na(LABEL_REGIONGROUP)), aes(y=LABEL_REGIONGROUP, xmin=xmin, xmax=xmax, ymin=as.numeric(LABEL_REGIONGROUP) - .225,
                                                       ymax=as.numeric(LABEL_REGIONGROUP) + .225, fill=label3)) +
    facet_wrap(~ time, space='free_x', scales='free_x') +

    geom_rect_pattern(data=subset(pdf4, stackgroup == 'Costs' & !is.na(LABEL_REGIONGROUP)),
                      aes(xmin=-xmin, xmax=-xmax, pattern='climate'), position=position_nudge(y=.225),
                      colour='black', linewidth=.02, pattern_density=.25,
                      pattern_spacing=.05, pattern_fill='black', pattern_units="in") +

    geom_rect_pattern(data=subset(pdf4, stackgroup == 'Other' & !is.na(LABEL_REGIONGROUP)),
                      aes(pattern=channel), position=position_nudge(y=-.225),
                      colour='black', linewidth=.02, pattern_density=.25,
                      pattern_spacing=.05, pattern_fill='black', pattern_units="in") +

    scale_fill_manual(name="Cost/benefit category:", breaks=c("Market Damages", "Non-Market Damages", "Feedback", "Costs"),
                      labels=c("Avoided market damages", "Avoided non-market damages", "Avoided market damages", "Costs"),
                      values=c('#e6ab02', '#66a61e', '#e6ab02', '#7570b3')) +
    scale_pattern_manual(name="Impact pathway:", breaks=c('climate', 'combined', 'pollution'), labels=c('Climate', 'Feedback', 'Pollution'), values=c('none', 'stripe', 'circle')) +
    theme_bw() + scale_x_continuous("Costs and potential avoided damages (% Regional GDP)", labels=scales::percent, breaks=c(c(0, .02, seq(0.05, .25, by=.05))),
                                    expand=expansion(add=c(0, .005))) +
    ylab(NULL) + theme(panel.spacing=unit(0.8, "lines")) +
    guides(fill=guide_legend(override.aes=list(pattern="none")))
myggsave("Figure ‎3.26B Regional costs and benefits", width=7.5, height=4)

## Simplified
pdf4 <- pdf3 %>% mutate(category=ifelse(label2 %in% c("Feedback", "Market Damages", "Non-Market Damages"),
                                        ifelse(channel == 'combined', 'climate', channel), "costs")) %>%
    group_by(category, LABEL_REGIONGROUP, time) %>% summarize(diff=sum(diff))

ggplot(pdf4 %>% filter(!is.na(LABEL_REGIONGROUP)), aes(diff, LABEL_REGIONGROUP, fill=category)) +
    facet_wrap(~ time, space='free_x', scales='free_x') +
    geom_col_pattern(aes(pattern=category), colour='black', linewidth=.02, pattern_density=.25,
                     pattern_spacing=.05, pattern_fill='black', pattern_units="in") +
    scale_fill_manual(name="Benefits and Costs:", breaks=c('climate', 'pollution', 'costs'), labels=c('Climate', 'Pollution', 'Costs'), values=c('#e41a1c', '#4daf4a', '#7570b3')) +
    scale_pattern_manual(name="Benefits and Costs:", breaks=c('climate', 'pollution', 'costs'), labels=c('Climate', 'Pollution', 'Costs'), values=c('none', 'circle', 'none')) +
    theme_bw() + scale_x_continuous("Potential avoided damages (% Regional GDP)", labels=scales::percent, breaks=c(c(0, .02, seq(0.05, .25, by=.05))),
                                    expand=expansion(add=c(0, .005))) +
    ylab(NULL) + theme(panel.spacing=unit(0.8, "lines"))
ggsave("regional-bycost-simple.pdf", width=7.5, height=4)

## Split by policy action

pdf2 <- rbind(pdf %>% filter(name %in% c("Baseline", "LTCAction") & time %in% c(2035, 2050, 2100)) %>%
              left_join(gdps, by=c('time', 'name', 'country')) %>%
              left_join(gainsregions, by=c('country'='ISO3')) %>%
              left_join(wbregions, by='REGION_4LETTER') %>%
              group_by(scenario_emt, time, prefix, LABEL_REGIONGROUP) %>%
              summarize(name="Long-term\nclimate solutions", cost=sum(cost) / sum(gdp * 1e6)) %>%
              pivot_wider(names_from=c(scenario_emt), values_from=cost) %>%
              mutate(diff=Baseline - `Long-term Climate Action`) %>% left_join(infomap, by='prefix') %>% select(!c(Baseline, `Long-term Climate Action`)),
              pdf %>% filter(name %in% c("LTCAction", "IntegratedAction") & time %in% c(2050, 2100)) %>%
              left_join(gdps, by=c('time', 'name', 'country')) %>%
              left_join(gainsregions, by=c('country'='ISO3')) %>%
              left_join(wbregions, by='REGION_4LETTER') %>%
              group_by(scenario_emt, time, prefix, LABEL_REGIONGROUP) %>%
              summarize(name="Clean air and\nnear-term solutions", cost=sum(cost) / sum(gdp * 1e6)) %>%
              pivot_wider(names_from=c(scenario_emt), values_from=cost) %>%
              mutate(diff=`Long-term Climate Action` - `Integrated Action`) %>% left_join(infomap, by='prefix') %>% select(!c(`Long-term Climate Action`, `Integrated Action`)))
pdf2$panel <- ifelse(pdf2$prefix %in% c("abateco2-tc", "pmmarket-percchg"), "Other",
              ifelse(pdf2$prefix %in% c("abateall-tcpc", "adaptall-acpc", "discontinuity-percap", "d_market", "d_nonmarket", 'pm-nonmarket', 'pm-market', 'apcosts', "d_slr", 'capitalloss', 'WBRC-infrastructure_cost'),
                     'Aggregate', 'Bottom-up'))

pdf3 <- pdf2 %>% filter(panel == 'Aggregate') %>%
    mutate(fulllabel=ifelse(label %in% c('Costs', 'Feedback'), label, name)) %>%
    group_by(LABEL_REGIONGROUP, fulllabel, channel, time) %>% summarize(diff=sum(diff))
pdf3$fulllabel <- factor(pdf3$fulllabel, levels=c('Long-term\nclimate solutions', 'Clean air and\nnear-term solutions', 'Feedback', 'Costs'))

ggplot(pdf3 %>% filter(!is.na(LABEL_REGIONGROUP)), aes(diff, LABEL_REGIONGROUP, fill=fulllabel)) +
    facet_wrap(~ time, space='free_x', scales='free_x') +
    geom_col_pattern(aes(pattern=channel), colour='black', linewidth=.02, pattern_density=.25,
                     pattern_spacing=.05, pattern_fill='black', pattern_units="in") +
    scale_fill_discrete(name="Policy Category:") +
    scale_pattern_manual(name="Impact pathway:", breaks=c('climate', 'combined', 'pollution'), labels=c('Climate', 'Feedback', 'Pollution'), values=c('none', 'stripe', 'circle')) +
    theme_bw() + scale_x_continuous("Potential avoided damages (% Regional GDP)", labels=scales::percent,
                                    breaks=c(c(0, .02, seq(0.05, .25, by=.05))), expand=expansion(add=c(0, .01))) +
    ylab(NULL) + theme(panel.spacing=unit(0.8, "lines")) #+ theme(plot.margin=unit(c(5.5, 30, 5.5, 5.5), "pt"))
ggsave("regional-bypolicy.pdf", width=6.5, height=4)

## Simplified version-- total costs, total benefits

pdf3 <- rbind(scen.diff.fracgdp("LTCAction", "Baseline") %>% filter(panel == 'Aggregate' & time %in% c(2035, 2050, 2100)) %>% mutate(name="Long-term climate solutions"),
              scen.diff.fracgdp("IntegratedAction", "LTCAction") %>% filter(panel == 'Aggregate' & time %in% c(2035, 2050, 2100)) %>% mutate(name="Clean air and\nnear-term climate solutions")) %>%
    filter(label != "Costs") %>%
    group_by(LABEL_REGIONGROUP, name, time) %>% summarize(diff=sum(diff))

ggplot(pdf3 %>% filter(!is.na(LABEL_REGIONGROUP)), aes(diff, LABEL_REGIONGROUP, fill=name)) +
    facet_wrap(~ time, space='free_x', scales='free_x') +
    geom_col() +
    scale_fill_discrete(name="Avoided Losses:") +
    theme_bw() + scale_x_continuous("Potential avoided damages (% Regional GDP)", labels=scales::percent,
                                    breaks=c(c(0, .02, seq(0.05, .25, by=.05))), expand=expansion(add=c(0, .01))) +
    ylab(NULL) + theme(panel.spacing=unit(0.8, "lines"))
ggsave("regional-total-simple.pdf", width=7.5, height=4)

## Construct BCA
costtype <- 'Welfare' # 'Market'

pdf3 <- rbind(scen.diff.fracgdp("LTCAction", "Baseline") %>% filter(panel == 'Aggregate' & time <= 2100) %>% mutate(name="Due to Long-term Climate Action"),
              scen.diff.fracgdp("IntegratedAction", "Baseline") %>% filter(panel == 'Aggregate' & time <= 2100) %>% mutate(name="Due to Integrated Action"))

if (costtype == 'Market') {
    pdf4 <- pdf3 %>% filter(!(prefix %in% c('d_nonmarket', 'pm-nonmarket')))
} else {
    pdf4 <- pdf3
}
pdf5 <- pdf4 %>%
    mutate(label=ifelse(label == 'Costs', 'Costs', 'Benefits')) %>%
    group_by(LABEL_REGIONGROUP, name, label, time) %>% summarize(diff=sum(diff)) %>%
    group_by(LABEL_REGIONGROUP, name, label) %>% arrange(time) %>%
    reframe(year=2025:2100, diff=splinefun(time, diff, method='monoH.FC')(year))

npvres <- data.frame()
for (name in unique(pdf5$name)) {
    if (name == "Due to decarbonization") {
        subgdps <- subset(gdps, name == "Baseline")
    } else {
        subgdps <- subset(gdps, name == "LTCAction")
    }
    subgdps2 <- subgdps %>% left_join(gainsregions, by=c('country'='ISO3')) %>%
        left_join(wbregions, by='REGION_4LETTER') %>%
        group_by(LABEL_REGIONGROUP, time) %>% summarize(gdp=sum(gdp)) %>%
        group_by(LABEL_REGIONGROUP) %>% arrange(time) %>%
        reframe(year=2025:2100, gdp=approx(time, gdp, year)$y)

    for (label in c('Costs', 'Benefits')) {
        subpdf5 <- pdf5[pdf5$name == name & pdf5$label == label,]
        subpdf6 <- subpdf5 %>% left_join(subgdps2, by=c('LABEL_REGIONGROUP', 'year'))
        npv <- subpdf6 %>% group_by(name, label, LABEL_REGIONGROUP) %>%
            summarize(npv=sum(gdp * diff * (1 / (1 + 0.03))^(year - 2025)))

        npvres <- rbind(npvres, npv)
    }

    ## IRR
    subpdf5 <- pdf5[pdf5$name == name,]
    subpdf6 <- subpdf5 %>% left_join(subgdps2, by=c('LABEL_REGIONGROUP', 'year'))
    subpdf7 <- subpdf6 %>% group_by(name, LABEL_REGIONGROUP, year) %>%
        summarize(val=sum(gdp * diff))

    irrvalid <- subpdf7 %>% group_by(name, LABEL_REGIONGROUP) %>%
        summarize(nodis=sum(val), infdis=val[1])
    irrres <- subpdf7 %>% group_by(name, LABEL_REGIONGROUP) %>%
        filter(LABEL_REGIONGROUP %in% irrvalid$LABEL_REGIONGROUP[sign(irrvalid$nodis) != sign(irrvalid$infdis)]) %>%
        summarize(irr=uniroot(function(dr) {
            sum(val * (1 / (1 + dr))^(year - 2025))
        }, c(0, 10))$root)

    npvres <- rbind(npvres, irrres %>% mutate(label="IRR") %>% rename(npv=irr),
                    irrvalid %>% filter(infdis > 0) %>% mutate(label="IRR") %>% select(!c(nodis, infdis)) %>% mutate(npv=Inf))
}

npvres2 <- npvres %>% filter(!is.na(LABEL_REGIONGROUP)) %>% group_by(name, LABEL_REGIONGROUP) %>%
    summarize(benefits=npv[label == "Benefits"], costs=-npv[label == "Costs"], bcr=benefits / costs,
              irr=ifelse('IRR' %in% label, npv[label == 'IRR'], NA)) %>%
    select(!c(benefits, costs)) %>% pivot_wider(names_from=name, values_from=c(bcr, irr))

library(clipr)
write_clip(npvres2)

## library(xtable)
## print(xtable(npvres2[, c('LABEL_REGIONGROUP', 'Due to Long-term Climate Action', 'Due to Integrated Action')], digits=0), type='html')

for (do.slcp.only in c(F, T)) {
if (do.slcp.only) {
    pdf2 <- scen.diff.fracgdp("IntegratedAction", "LTCAction")
} else {
    pdf2 <- scen.diff.fracgdp("LTCAction", "Baseline")
}
pdf3 <- pdf2 %>% filter(panel == 'Aggregate' & time %in% c(2035, 2050, 2100)) %>%
    mutate(label=ifelse(label %in% c("Costs", "Feedback"), label, "Damages")) %>%
    group_by(LABEL_REGIONGROUP, channel, label, time) %>% summarize(diff=sum(diff)) %>%
    mutate(diff=ifelse(label != 'Costs' | channel != "pollution" | time <= 2050, diff, 0)) %>%
    mutate(fulllabel=ifelse(label == 'Costs', "other",
                     ifelse(channel == 'climate', "Climate damages",
                     ifelse(channel == 'pollution', "Pollution damages", "other"))))
pdf3$fulllabel <- factor(pdf3$fulllabel, levels=c('Climate damages', 'Pollution damages', 'Costs'))

if (do.slcp.only) {
    ggplot(pdf3 %>% filter(fulllabel != 'other' & !is.na(LABEL_REGIONGROUP)), aes(diff, LABEL_REGIONGROUP, fill=fulllabel)) +
        facet_wrap(~ time, space='free_x', scales='free_x') +
        geom_col() +
        scale_fill_manual(name="Avoided Losses:", breaks=c('Climate damages', 'Pollution damages', 'Costs'), values=c('#a6761d', '#7570b3', '#e7298a')) +
        theme_bw() + scale_x_continuous("Potential avoided damages (% Regional GDP)", labels=scales::percent,
                                        breaks=c(seq(0, .03, by=.01)), expand=expansion(add=c(0, .005))) +
        ylab(NULL) + theme(panel.spacing=unit(0.8, "lines"))
    myggsave("Figure ‎3.27 Regional avoided damages - pollution", width=7.5, height=4)
} else {
    ggplot(pdf3 %>% filter(fulllabel != 'other' & !is.na(LABEL_REGIONGROUP)), aes(diff, LABEL_REGIONGROUP, fill=fulllabel)) +
        facet_wrap(~ time, space='free_x', scales='free_x') +
        geom_col() +
        scale_fill_manual(name="Avoided Losses:", breaks=c('Climate damages', 'Pollution damages', 'Costs'), values=c('#a6761d', '#7570b3', '#e7298a')) +
        theme_bw() + scale_x_continuous("Potential avoided damages (% Regional GDP)", labels=scales::percent,
                                        breaks=c(c(0, .02, seq(0.05, .25, by=.05))), expand=expansion(add=c(0, .015))) +
        ylab(NULL) + theme(panel.spacing=unit(0.8, "lines"))
    myggsave("Figure ‎3.29 Regional avoided damages - climate", width=7.5, height=4)
}
}

## Generate damages for each bottom-up result

titles <- list('pmmarket-percchg'="Aggregate Market Damages from PM2.5",
               'd_slr'="Avoided Sea-Level Rise and Storm Surge Damages\nfrom Integrated Action",
               "cromar-mortality"="Avoided Excess All-Cause Mortality\ndue to Climate from Integrated Action",
               "dasgupta-labor-dmg"="Avoided Labour Productivity Losses\ndue to Climate from Integrated Action",
               "WBRC-morb_disutility_new"="Disutility from Increased PM2.5 Morbidity",
               "WBRC-morb_healthcare_new"="Health Care Cost from Increased PM2.5 Morbidity",
               "WBRC-morb_productivity_new"="Avoided Labour Productivity Losses\ndue to PM2.5 from Integrated Action",
               "WBRC-mort_disutility_new"="Avoided Increased PM2.5 Mortality Risk from Integrated Action",
               "WBRC-mort_productivity_new"="Labour Productivity Losses from Increased PM2.5 Mortality Risk",
               "WBRC-mort-combo"="Avoided Increased PM2.5 Mortality Risk from Integrated Action",
               'MarketDamageAQ_CropLoss'="Avoided Agricultural Productivity Loss from Integrated Action")
xbreaks <- list("dasgupta-labor-dmg"=seq(0, 10, by=1) / 100,
                "WBRC-morb_productivity_new"=c(0, 0.004, seq(0.01, 0.025, by=0.005)),
                "cromar-mortality"=seq(0, 0.016, by=0.003),
                "WBRC-mort_disutility_new"=seq(0, 0.1, by=0.02),
                "WBRC-mort-combo"=seq(0, 0.1, by=0.02),
                'd_slr'=c(0, 0.001, seq(0.002, 0.006, by=0.002)),
                'MarketDamageAQ_CropLoss'=seq(0, 0.0006, by=0.0003))
xexpands <- list("dasgupta-labor-dmg"=expansion(add=c(0.001, .005)),
                 "WBRC-morb_productivity_new"=expansion(add=c(0, .004)),
                 "cromar-mortality"=expansion(add=c(0, .0028)),
                 "WBRC-mort_disutility_new"=expansion(add=c(0, .01)),
                 "WBRC-mort-combo"=expansion(add=c(0, .01)),
                 'd_slr'=expansion(add=c(0, .0014)),
                 'MarketDamageAQ_CropLoss'=expansion(add=c(0, .00014)))
is.nonmarket <- c("WBRC-morb_disutility_new", "WBRC-mort_disutility_new")
doc.filenames <- list("dasgupta-labor-dmg"="Figure ‎3.18 Regional warming productivity",
                      "WBRC-morb_productivity_new"="Figure 3.19 Regional pollution productivity",
                      "cromar-mortality"="Figure 3.20 Regional warming mortality",
                      "WBRC-mort-combo"="Figure 3.21 Regional pollution mortality",
                      "d_slr"="Figure 3.22 Regional SLR damages",
                      'MarketDamageAQ_CropLoss'="Figure 3.23 Agricultural loss")

for (prfx in c('pmmarket-percchg', 'd_slr', "WBRC-mort-combo", unique(pdf$prefix[pdf$panel == 'Bottom-up']))) {
    if (prfx == "WBRC-mort-combo")
        prfxes <- c("WBRC-mort_disutility_new", "WBRC-mort_productivity_new")
    else
        prfxes <- prfx
    pdf2 <- rbind(scen.diff.fracgdp("LTCAction", "Baseline") %>% filter(prefix %in% prfxes) %>% mutate(name="Long-term climate solutions"),
                  scen.diff.fracgdp("IntegratedAction", "LTCAction") %>% filter(prefix %in% prfxes) %>% mutate(name="Clean air and\nnear-term climate solutions")) %>% group_by(time, LABEL_REGIONGROUP, channel, label, panel, name) %>% summarize(diff=sum(diff))
    title <- ifelse(prfx %in% names(titles), titles[[prfx]], paste(pdf2$label[1], "from", pdf2$channel[1]))
    pdf2$name <- factor(pdf2$name, levels=rev(c("Long-term climate solutions", "Clean air and\nnear-term climate solutions")))
    pdf2$LABEL_REGIONGROUP <- factor(pdf2$LABEL_REGIONGROUP, levels=rev(unique(pdf2$LABEL_REGIONGROUP)))

    gp <- ggplot(pdf2 %>% filter(time %in% c(2035, 2050, 2100) & !is.na(LABEL_REGIONGROUP)), aes(diff, LABEL_REGIONGROUP, fill=name)) +
        facet_wrap(~ time, space='free_x', scales='free_x') +
        geom_col() +
        scale_fill_manual(name="Policy Category:", breaks=c("Long-term climate solutions", "Clean air and\nnear-term climate solutions"),
                          labels=c("Long-term climate solutions", "Clean air and\nnear-term climate solutions"),
                          values=c('#33a02c', '#1f78b4')) +
        theme_bw() +
        ylab(NULL) + theme(panel.spacing=unit(1, "lines")) + ggtitle(title)
    xtitle <- ifelse(prfx %in% is.nonmarket, "Potential avoided damages (equivalent % of Regional GDP, relative to Baseline)",
                     "Potential avoided damages (% of Regional GDP, relative to Baseline)")
    if (prfx %in% names(xbreaks)) {
        gp <- gp + scale_x_continuous(xtitle, labels=scales::percent,
                                      breaks=xbreaks[[prfx]], expand=xexpands[[prfx]])
    } else {
        gp <- gp + scale_x_continuous(xtitle, labels=scales::percent,
                                      expand=expansion(mult=c(0, .1)))
    }
    if (prfx %in% names(doc.filenames)) {
        myggsave(doc.filenames[[prfx]], gp, width=7.5, height=4)
    } else {
        ggsave(paste0("regional-", prfx, ".pdf"), gp, width=7.5, height=4)
    }
}

pdf2 <- scen.diff.fracgdp("IntegratedAction", "Baseline") %>% filter(prefix %in% c("WBRC-morb_productivity_new", "WBRC-morb_disutility_new", "WBRC-morb_healthcare_new"))
pdf2$prefix <- factor(pdf2$prefix, levels=rev(c("WBRC-morb_healthcare_new", "WBRC-morb_disutility_new", "WBRC-morb_productivity_new")))
pdf2$LABEL_REGIONGROUP <- factor(pdf2$LABEL_REGIONGROUP, levels=rev(unique(pdf2$LABEL_REGIONGROUP)))

ggplot(pdf2 %>% filter(time %in% c(2035, 2050, 2100) & !is.na(LABEL_REGIONGROUP)), aes(diff, LABEL_REGIONGROUP, fill=prefix)) +
    facet_wrap(~ time, space='free_x', scales='free_x') +
    geom_col() +
    scale_fill_manual(name="Morbidity Cost:", breaks=c("WBRC-morb_healthcare_new", "WBRC-morb_disutility_new", "WBRC-morb_productivity_new"),
                      labels=c("Healthcare Costs", "Welfare Loss", "Productivity Loss"),
                      values=c('#7570b3', '#d95f02', '#1b9e7780')) +
    theme_bw() + scale_x_continuous("Potential avoided damages (% of GDP, relative to Baseline)", labels=scales::percent,
                                        breaks=c(0, 0.01, seq(0.02, 0.1, by=0.02)), expand=expansion(add=c(0, .005))) +
    ylab(NULL) + theme(panel.spacing=unit(0.8, "lines"))
ggsave(paste0("regional-Morbidity.pdf"), width=7.5, height=4)

