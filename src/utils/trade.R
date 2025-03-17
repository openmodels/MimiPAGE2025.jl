calc.domar.change <- function(TT, labels, isos, dimpact) {
    ## Match up known impacts to IO countries
    isos[isos == 'SDN'] <- 'SUD'
    isos[isos == 'PSX'] <- 'PSE'
    labels2 <- labels %>% left_join(data.frame(V1=isos, dimpact=dimpact), by='V1')
    labels2$dimpact[labels2$V1 == 'ANT'] <- labels2$dimpact[labels2$V1 == 'NLD']

    ## Calculate Domar weights
    total.sales <- rowSums(TT) + labels2$FD
    labels2$gdp <- labels2$FD + labels2$VA
    global.gdp <- sum(labels2$gdp)
    weights <- total.sales / global.gdp

    ## Calculate global GDP loss
    dimpact.level <- exp(labels2$dimpact) - 1
    total.change <- sum(weights * ifelse(is.na(dimpact.level), 0, dimpact.level))

    ## Extract out the additional
    total.trade.effect <- total.change - sum(dimpact.level * labels2$gdp, na.rm=T) / global.gdp

    total.trade.effect
}

calc.domar.distribute.method1 <- function(TT, labels, isos, dimpact) {
    domar.change <- calc.domar.change(TT, labels, isos, dimpact)

    ## Distribute domar loss
    dirimpacts <- data.frame(ISO=isos, dimpact)
    totimpacts <- data.frame()
    for (iso in isos) {
        comtrade.iso <- subset(comtrade, ReporterISO == iso & PartnerISO != 'W00')
        if (nrow(comtrade.iso) == 0)
            next
        results2.iso <- subset(results2, ISO == iso)

        maxgrow <- max(0, dimpact[isos == iso])

        calcdf <- comtrade.iso %>% left_join(dirimpacts, by=c('PartnerISO'='ISO'))

        ## Limit any growth to growth of country
        calcdf$cif.lost <- calcdf$Cifvalue * pmax(-calcdf$dimpact, -maxgrow)
        calcdf$fob.lost <- calcdf$Fobvalue * pmax(-calcdf$dimpact, -maxgrow)

        ## Fill in NAs, with preference based on direction
        calcdf$fob.lost[is.na(calcdf$fob.lost) & calcdf$FlowDesc == 'Export'] <- calcdf$cif.lost[is.na(calcdf$fob.lost) & calcdf$FlowDesc == 'Export']
        calcdf$Fobvalue[is.na(calcdf$fob.lost) & calcdf$FlowDesc == 'Export'] <- calcdf$Cifvalue[is.na(calcdf$fob.lost) & calcdf$FlowDesc == 'Export']
        calcdf$cif.lost[is.na(calcdf$cif.lost) & calcdf$FlowDesc == 'Import'] <- calcdf$fob.lost[is.na(calcdf$cif.lost) & calcdf$FlowDesc == 'Import']
        calcdf$Cifvalue[is.na(calcdf$cif.lost) & calcdf$FlowDesc == 'Import'] <- calcdf$Fobvalue[is.na(calcdf$cif.lost) & calcdf$FlowDesc == 'Import']

        fracloss.import <- sum(calcdf$cif.lost[calcdf$FlowDesc == 'Import'], na.rm=T) / sum(calcdf$Cifvalue[calcdf$FlowDesc == 'Import'], na.rm=T)
        fracloss.export <- sum(calcdf$fob.lost[calcdf$FlowDesc == 'Export'], na.rm=T) / sum(calcdf$Fobvalue[calcdf$FlowDesc == 'Export'], na.rm=T)

        totimpacts <- rbind(totimpacts, data.frame(ISO=iso, fracloss.import, fracloss.export))
    }

    totimpacts2 <- totimpacts %>% left_join(df.gdp3, by=c('ISO'='Country Code'))

    list(global=data.frame(domar.change, global.gdp=sum(totimpacts2$GDP.2019.est, na.rm=T), global.loss=sum(ifelse(is.na(totimpacts2$fracloss.export), 0, totimpacts2$fracloss.export) * totimpacts2$GDP.2019.est, na.rm=T)),
         totimpacts2=totimpacts2)
}

calc.domar.distribute.method2 <- function(scaleby, isos, totimpacts2) {
    totimpacts2$tradeloss <- totimpacts2$fracloss.export * scaleby

    domar.loss2 <- data.frame(ISO=isos) %>% left_join(totimpacts2)
    domar.loss2$tradeloss
}

logscalebys <- c(0)
for (year in unique(results2$Year)) {
    results2.year <- subset(results2, Year == year)
    dimpact <- results2.year$dimpact

    output <- calc.domar.distribute.method1(year, results2.year$ISO, dimpact)

    thisglobal <- output$global
    thisglobal$yy <- thisglobal$domar.change * thisglobal$global.gdp
    if (thisglobal$yy > 0) {
        logscaleby <- log(thisglobal$yy) - log(thisglobal$global.loss)
        if (logscaleby >= 0)
            logscalebys <- c(logscalebys, logscaleby)
    }

    mod <- lm(scalebys ~ 1)
    smoothscalebys <- exp(predict(mod, data.frame(years=unique(results2$Year)))) * exp(var(mod$resid) / 2)

    losses <- calc.domar.distribute.method2(smoothscalebys[ii], results2.year$ISO, output$thisyear2)

    PROCESS_LOSSES(losses)
}
