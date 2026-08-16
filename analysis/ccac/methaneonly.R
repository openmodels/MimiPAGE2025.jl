setwd("~/research/iamup2/MimiPAGE2025.jl/analysis/ccac")

library(dplyr)
library(tidyr)
library(ggplot2)
library(ggforce)
library(ggpattern)
library(readxl)

do.global <- F
source("loader.R")

pdf2 <- pdf %>% filter(name %in% c("Baseline_CLE", "methaneonly") & time %in% c(2050, 2100)) %>%
    left_join(gdps, by=c('time', 'name', 'country')) %>%
    mutate(fraccost=cost / (gdp * 1e6)) %>% select(!c(gdp, cost)) %>%
    pivot_wider(names_from=name, values_from=fraccost) %>%
    mutate(diff=Baseline_CLE - methaneonly) %>% left_join(infomap, by='prefix')
pdf2$panel <- ifelse(pdf2$prefix %in% c("abateco2-tc", "pmmarket-percchg"), "Other",
              ifelse(pdf2$prefix %in% c("abateall-tcpc", "adaptall-acpc", "discontinuity-percap", "d_market", "d_nonmarket", 'pm-nonmarket', 'pm-market', 'apcosts', "d_slr", 'capitalloss'),
                     'Aggregate', 'Bottom-up'))

sids <- c("Antigua and Barbuda", "Haiti", "St. Kitts and Nevis",
          "Bahamas", "Jamaica", "St. Lucia", "Barbados", "Kiribati",
          "St. Vincent and the Grenadines", "Belize", "Maldives",
          "Seychelles", "Cabo Verde", "Marshall Islands",
          "Solomon Islands", "Comoros",
          "Micronesia (Federated States of)", "Suriname",
          "Cook Islands", "Mauritius", "Timor-Leste", "Cuba", "Nauru",
          "Tonga", "Dominica", "Niue", "Trinidad and Tobago",
          "Dominican Republic", "Palau", "Tuvalu", "Fiji",
          "Papua New Guinea", "Vanuatu", "Grenada", "Samoa",
          "Guinea-Bissau", "São Tomé and Príncipe", "Guyana",
          "Singapore", "American Samoa", "Curacao", "Puerto Rico",
          "Anguilla", "French Polynesia", "Sint Maarten", "Aruba",
          "Guadeloupe", "Turks and Caicos Islands", "Bermuda", "Guam",
          "U.S. Virgin Islands", "British Virgin Islands",
          "Martinique", "Cayman Islands", "Montserrat",
          "Commonwealth of Northern Marianas", "New Caledonia")
library(countrycode)
sids.iso <- countrycode(sids, 'country.name', 'iso3c')
aggregates <- read.csv("../../data/aggregates.csv")

countries <- unique(sapply(sids.iso, function(iso) ifelse(iso %in% pdf2$country, iso, ifelse(iso %in% aggregates$ISO, aggregates$Aggregate[aggregates$ISO == iso], NA))))

unique(pdf2$prefix[pdf2$diff != 0])

pdf3 <- pdf2 %>% filter(country %in% countries & prefix == "d_slr")
pdf3$country.name <- countrycode(pdf3$country, 'iso3c', 'country.name',
                                 custom_match = c("SIS-GONDWANA"="Other Gondwana SIS",
                                                  "SIS-NAMER"="Other Caribbean SIS",
                                                  "SIS-OCEANIA"="Other Pacific SIS"))
pdf3$country.name <- factor(pdf3$country.name, levels=rev(c(sort(unique(pdf3$country.name[!(pdf3$country %in% c('SIS-Gondwana', 'SIS-NAmer', 'SIS-Oceania'))])), "Other Pacific SIS", "Other Caribbean SIS", "Other Gondwana SIS")))

ggplot(pdf3, aes(diff, country.name)) +
    facet_wrap(~ time, scales='free_x') +
    geom_col() +
    scale_x_continuous("Benefit specific to methane reduction (% GDP)", labels=scales::percent) +
    ylab(NULL) +
    theme_bw()
ggsave("methaneonly-slr.pdf", width=6, height=4.25)
