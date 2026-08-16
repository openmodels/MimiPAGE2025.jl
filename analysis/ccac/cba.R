setwd("~/research/iamup2/MimiPAGE2025.jl/analysis/ccac")

library(clipr)
library(EnvStats)

do.global <- T
source("loader-mc.R")

gdps2 <- subset(gdps, name == "Baseline") %>% group_by(time, trialnum) %>% summarize(gdp=sum(gdp))

results <- data.frame()
for (pert in c("LTCAction", "IntegratedAction")) {
    for (costtype in c('Market', 'Welfare')) {
        pdf2 <- scen.diff.fracgdp(pert, "Baseline")
        if (costtype == 'Market') {
            pdf2 <- pdf2 %>% filter(label != "Aggregate Non-Market Damages")
        }
        pdf3.costs <- pdf2 %>% filter(panel == "Aggregate" & label == "Costs") %>%
            group_by(trialnum, time) %>% summarize(diff=sum(diff)) %>%
            left_join(gdps2, by=c('trialnum', 'time')) %>%
            group_by(trialnum) %>% mutate(diff=rtri(1, .6, 1.25, 1.15) * diff) %>%
            ## reframe(year=2025:2100, diff=splinefun(c(2025, time), c(0, diff), method='monoH.FC')(year),
            reframe(year=2025:2100, diff=tryCatch({
                splinefun(time, diff, method='monoH.FC')(year)}, error=function(e) {
                    NA
                }),
                gdp=splinefun(time, gdp, method='monoH.FC')(year))
        pdf3.costs2 <- pdf3.costs %>%
            group_by(trialnum) %>% summarize(npv=sum(gdp * diff * (1 / (1 + 0.03))^(year - 2025)))
        pdf3.benefits <- pdf2 %>% filter(panel == "Aggregate" & label != "Costs") %>%
            group_by(trialnum, prefix) %>%
            mutate(sigmaap=approx(c(2035, 2050, 2100), c(1.23, 1.25, 1.33), rule=2, time)$y,
                   diff=ifelse(prefix %in% c('pm-market', 'pm-nonmarket'),
                               ## mu st. exp(mu + sigmaap^2/2) = diff => log(diff) - sigmaap^2/2
                               diff * rlnorm(1, -log(sigmaap)^2/2, log(sigmaap)),
                               diff)) %>%
            group_by(trialnum, time) %>% summarize(diff=sum(diff)) %>%
            left_join(gdps2, by=c('trialnum', 'time')) %>%
            group_by(trialnum) %>%
            reframe(year=2025:2100, diff=splinefun(c(2025, time), c(0, diff), method='monoH.FC')(year),
                gdp=splinefun(time, gdp, method='monoH.FC')(year))
        pdf3.benefits2 <- pdf3.benefits %>%
            group_by(trialnum) %>% summarize(npv=sum(gdp * diff * (1 / (1 + 0.03))^(year - 2025)))

        pdf4 <- pdf3.costs2 %>% left_join(pdf3.benefits2, by='trialnum', suffix=c('.c', '.b')) %>%
            summarize(valid=mean(!is.na(npv.b)),
                      mu=mean(npv.b / -npv.c, na.rm=T), med=median(npv.b / -npv.c, na.rm=T),
                      ci25=quantile(npv.b / -npv.c, .25, na.rm=T), ci75=quantile(npv.b / -npv.c, .75, na.rm=T))

        pdf3.irr <- rbind(pdf3.costs, pdf3.benefits) %>%
            group_by(trialnum, year) %>% summarize(diff=sum(diff), gdp=gdp[1]) %>%
            group_by(trialnum) %>% summarize(irr=tryCatch({
                uniroot(function(dr) {
                    sum(diff * gdp * (1 / (1 + dr))^(year - 2025))
                }, c(0, 1))$root * 100
            }, error=function(e) {
                NA
            }))

        pdf4.irr <- pdf3.irr %>% ungroup() %>%
            summarize(valid=mean(!is.na(irr)),
                      mu=mean(irr, na.rm=T), med=median(irr, na.rm=T),
                      ci25=quantile(irr, .25, na.rm=T), ci75=quantile(irr, .75, na.rm=T))

        results <- rbind(results, cbind(policy=pert, costtype=costtype, metric="BCR", pdf4),
                         cbind(policy=pert, costtype=costtype, metric="IRR", pdf4.irr))
    }
}

write_clip(results)

