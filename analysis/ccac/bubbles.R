setwd("~/research/iamup2/MimiPAGE2025.jl/analysis/ccac")

library(dplyr)
library(tidyr)
library(ggplot2)
library(ggforce)
library(ggpattern)

do.global <- T
source("loader.R")

pdf2 <- pdf %>% filter(time == 2050) %>% left_join(infomap, by='prefix')
pdf2$name <- factor(pdf2$name, levels=rev(runspecs$name))

ggplot(pdf2, aes(cost / 1e12, name, fill=label)) +
    facet_wrap(~ panel) +
    geom_col_pattern(aes(pattern=channel), colour='black', linewidth=.05) +
    scale_pattern_manual(values=c('none', 'stripe', 'circle')) +
    theme_bw() + xlab("Total Cost (Trillion 2015 USD)")

pdf2 <- pdf %>% filter(time == 2050) %>% filter(!(name %in% c("Decarb_CLE", "Decarb_CLE_SSP3"))) %>%
    mutate(name=gsub("Decarb_APC", "Cost", gsub("Baseline_CLE", "Cost", name))) %>%
    filter(name %in% c("Cost", "Cost_SSP3")) %>%
    pivot_wider(names_from=c(scenario_ghg, scenario_pol), values_from=cost) %>%
    mutate(diff=Baseline_CLE - Decarb_APC) %>% left_join(infomap, by='prefix')
pdf$panel <- ifelse(pdf$prefix == "pmmarket-percchg", "Other",
             ifelse(pdf$prefix %in% c("abateco2-tc", "d_market", "d_nonmarket", 'pm-nonmarket', 'pm-market', 'apcosts'),
                    'Aggregate', 'Bottom-up'))

ggplot(pdf2, aes(diff / 1e12, name, fill=label)) +
    facet_wrap(~ panel) +
    geom_col_pattern(aes(pattern=channel), colour='black', linewidth=.05) +
    scale_pattern_manual(values=c('none', 'stripe', 'circle')) +
    theme_bw() + xlab("Total Benefits (Trillion 2015 USD)")

pdf2 <- pdf %>% filter(time == 2100) %>% filter(!(name %in% c("Decarb_CLE", "Decarb_CLE_SSP3"))) %>%
    mutate(name=gsub("Decarb_APC", "Cost", gsub("Baseline_CLE", "Cost", name))) %>%
    filter(name %in% c("Cost", "Cost_SSP3")) %>%
    pivot_wider(names_from=c(scenario_ghg, scenario_pol), values_from=cost) %>%
    mutate(diff=Baseline_CLE - Decarb_APC) %>% left_join(infomap, by='prefix')
pdf$panel <- ifelse(pdf$prefix == "pmmarket-percchg", "Other",
             ifelse(pdf$prefix %in% c("abateco2-tc", "d_market", "d_nonmarket", 'pm-nonmarket', 'pm-market', 'apcosts'),
                    'Aggregate', 'Bottom-up'))

ggplot(pdf2, aes(diff / 1e12, name, fill=label)) +
    facet_wrap(~ panel) +
    geom_col_pattern(aes(pattern=channel), colour='black', linewidth=.05) +
    scale_pattern_manual(values=c('none', 'stripe', 'circle')) +
    theme_bw() + xlab("Total Benefits (Trillion 2015 USD)")




pdf2 <- pdf %>% filter(name %in% c("Baseline_CLE", "Decarb_APC")) %>%
    mutate(name=gsub("Decarb_APC", "Cost", gsub("Baseline_CLE", "Cost", name))) %>%
    filter(!(name %in% c("Cost_pmhalfcost", "Cost_pmdblcost", "Cost_ghhalfcost",
                         "Cost_ghdblcost", "Cost_halfcost", "Cost_dblcost"))) %>%
    pivot_wider(names_from=c(scenario_ghg, scenario_pol), values_from=cost) %>%
    mutate(diff=Baseline_CLE - Decarb_APC) %>% left_join(infomap, by='prefix')
pdf$panel <- ifelse(pdf$prefix == "pmmarket-percchg", "Other",
             ifelse(pdf$prefix %in% c("abateco2-tc", "d_market", "d_nonmarket", 'pm-nonmarket', 'pm-market', 'apcosts'),
                    'Aggregate', 'Bottom-up'))
pdf2$diff[pdf2$label == 'Costs'] <- -pdf2$diff[pdf2$label == 'Costs']

pdf3 <- pdf2 %>% group_by(label) %>%
    mutate(total=sum(diff),
           start_angle=lag(cumsum(diff), default=0) / total * 2 * pi,
           end_angle=cumsum(diff) / total * 2 * pi) %>%
    ungroup()

ggplot(pdf3, aes(x0=0, y0=0, r0=0, r=sqrt(total), fill=channel)) +
    geom_arc_bar(aes(
        start=start_angle,
        end=end_angle,
        r0=0,
        r=sqrt(total)  # Adjust radius accordingly
    ), position="identity") +  # You may adjust position for layout
    coord_fixed() +
    facet_wrap(~label, ncol=4) +
    theme_void() +
    scale_fill_brewer(palette="Set3")

ggplot(subset(pdf3, label %in% c('Aggregate Market Damages', 'Aggregate Non-Market Damages')), aes(x0=0, y0=0, r0=0, r=sqrt(total), fill=channel)) +
    geom_arc_bar(aes(
        start=start_angle,
        end=end_angle,
        r0=0,
        r=sqrt(total)  # Adjust radius accordingly
    ), position="identity") +  # You may adjust position for layout
    coord_fixed() +
    facet_wrap(~label) +
    theme_void() +
    scale_fill_brewer(palette="Set3")

ggplot(subset(pdf3, !(label %in% c('Aggregate Market Damages', 'Aggregate Non-Market Damages'))), aes(x0=0, y0=0, r0=0, r=sqrt(total), fill=channel)) +
    geom_arc_bar(aes(
        start=start_angle,
        end=end_angle,
        r0=0,
        r=log(total)  # Adjust radius accordingly
    ), position="identity") +  # You may adjust position for layout
    geom_arc_bar(aes(
        colour=channel,
        start=start_angle,
        end=end_angle,
        r0=0,
        r=log(total) * 0.9  # Adjust radius accordingly
    ), position="identity") +  # You may adjust position for layout
    coord_fixed() +
    facet_wrap(~label) +
    theme_void() +
    scale_fill_brewer(palette="Set3") +
    scale_colour_brewer(palette="Set3")
