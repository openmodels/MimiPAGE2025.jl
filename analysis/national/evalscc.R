setwd("~/research/iamup2/MimiPAGE2020.jl")

library(dplyr)
library(ggplot2)
library(reshape2)
library(cowplot)
library(scales)

baseline <- read.csv("data/bycountry.csv")

get.result.row <- function(df) {
    df2 <- df %>% filter(country == 'global')
    df3 <- df %>% filter(country != 'global') %>% group_by(country) %>% summarize(scc=median(scc, na.rm=T)) %>%
        left_join(baseline, by=c('country'='ISO3'))

    ## plot(density(df3$scc / df3$Pop2015))
    ## mean(df3$scc / df3$Pop2015) / sd(df3$scc / df3$Pop2015)
    ## plot(density(df3$scc / df3$GDP2015))
    ## mean(df3$scc / df3$GDP2015) / sd(df3$scc / df3$GDP2015)

    df3$GDPpc2015 <- df3$GDP2015 / df3$Pop2015

    df3$logscc <- log(df3$scc)
    df3$logscc[!is.finite(df3$logscc)] <- NA
    mod <- lm(logscc ~ log(Pop2015) + log(GDPpc2015), data=df3)
    ## summary(mod) # R2 = 0.9737

    if (is.null(mod$na.action)) {
        df3$expresid <- exp(mod$resid)
    } else {
        df3$expresid <- NA
        df3$expresid[-mod$na.action] <- exp(mod$resid)
    }
    mod2 <- lm(expresid ~ Temp2010, data=df3)
    ## summary(mod2)

    ## Model:
    ## SCC = (alpha0 + alpha1 T) (Pop^beta) (GDPpc^gamma)

    soln <- optim(c(1, mod2$coeff[2], mod$coeff[-1]), function(par) {
        scchat <- exp(mod$coeff[1]) * (par[1] + par[2] * df3$Temp2010) * df3$Pop2015^par[3] * df3$GDPpc2015^par[4] * exp(var(mod$resid) / 2)
        sccobs <- sign(df3$scc) * log(abs(df3$scc) + 1)
        sccprd <- sign(scchat) * log(abs(scchat) + 1)
        sum((sccobs - sccprd)^2, na.rm=T)
    }, hessian=T)

    ## Calculate pars for each mc
    df4 <- df %>% filter(country != 'global')
    df4$mc <- rep(1:(nrow(df4) / 183), each=183)
    pardf <- data.frame()
    for (mcii in unique(df4$mc)) {
        df5 <- df4 %>% filter(mc == mcii & country != 'global') %>% left_join(baseline, by=c('country'='ISO3'))
        if (sum(!is.na(df5$scc) & df5$scc > 0) < 3) {
            pardf <- rbind(pardf, data.frame(median=NA, mean=NA, stddev=NA,
                                             alpha1=NA, alpha1.se=NA, betam1=NA,
                                             beta.se=NA, gammam1=NA, gamma.se=NA, rsqr=NA))
            next
        }

        df5$GDPpc2015 <- df5$GDP2015 / df5$Pop2015

        df5$logscc <- log(df5$scc)
        df5$logscc[!is.finite(df5$logscc)] <- NA
        mod.mc <- lm(logscc ~ log(Pop2015) + log(GDPpc2015), data=df5)

        soln.mc <- optim(soln$par, function(par) {
            scchat <- exp(mod.mc$coeff[1]) * (par[1] + par[2] * df5$Temp2010) * df5$Pop2015^par[3] * df5$GDPpc2015^par[4] * exp(var(mod.mc$resid) / 2)
            sccobs <- sign(df5$scc) * log(abs(df5$scc) + 1)
            sccprd <- sign(scchat) * log(abs(scchat) + 1)
            sum((sccobs - sccprd)^2, na.rm=T)
        }, hessian=F)

        par.mc <- soln.mc$par
        scchat <- exp(mod$coeff[1]) * (par.mc[1] + par.mc[2] * df5$Temp2010) * df5$Pop2015^par.mc[3] * df5$GDPpc2015^par.mc[4] * exp(var(mod$resid) / 2)
        if (sum(scchat > 0 & !is.na(df5$scc) & df5$scc > 0) > 2)
            rsqr <- summary(lm(logscc ~ 0 + log(scchat), data=df5))$r.squared
        else
            rsqr <- NA

        pardf <- rbind(pardf, data.frame(median=NA, mean=NA, stddev=NA,
                                         alpha1=par.mc[2], alpha1.se=NA, betam1=par.mc[3] - 1,
                                         beta.se=NA, gammam1=par.mc[4] - 1, gamma.se=NA, rsqr=rsqr))
    }

    pardf$median <- df$scc[df$country == 'global']
    pardf$mean <- df$scc[df$country == 'global']

    par <- soln$par
    df3$scchat <- exp(mod$coeff[1]) * (par[1] + par[2] * df3$Temp2010) * df3$Pop2015^par[3] * df3$GDPpc2015^par[4] * exp(var(mod$resid) / 2)

    rsqr <- summary(lm(logscc ~ 0 + log(scchat), data=df3))$r.squared

    vcv <- solve(soln$hessian)
    stderr <- sqrt(diag(vcv))

    rbind(data.frame(median=median(df2$scc, na.rm=T), mean=mean(df2$scc, na.rm=T), stddev=sd(df2$scc, na.rm=T),
                     alpha1=par[2], alpha1.se=stderr[2],
                     betam1=par[3] - 1, beta.se=stderr[3], gammam1=par[4] - 1, gamma.se=stderr[4], rsqr),
          pardf)
}

gp.saved <- NULL
get.displays <- function(alts, levels) {
    pdf1.raw <- data.frame()
    pdf2 <- data.frame()
    pdf2.mc <- data.frame()
    pdf2.diff <- data.frame()
    basedf <- NULL
    for (ii in 1:length(alts)) {
        print(levels[ii])
        altdf <- read.csv(alts[ii])
        pdf1.raw <- rbind(pdf1.raw, cbind(subset(altdf, country == 'global'), group=levels[ii]))
        rows <- get.result.row(altdf)
        pdf2 <- rbind(pdf2, cbind(rows[1,], group=levels[ii]))
        if (is.null(basedf)) {
            basedf <- rows[-1,]
            pdf2.mc <- rbind(pdf2.mc, cbind(rows[-1,], group=levels[ii]))
        } else {
            pdf2.mc <- rbind(pdf2.mc, cbind(rows[-1,], group=levels[ii]))
            alpha1.diff <- rows$alpha1[-1] - basedf$alpha1
            beta.diff <- rows$betam1[-1] - basedf$betam1
            gamma.diff <- rows$gammam1[-1] - basedf$gammam1
            pdf2.diff <- rbind(pdf2.diff, data.frame(alpha1=alpha1.diff, alpha1.se=NA,
                                                     betam1=beta.diff, beta.se=NA,
                                                     gammam1=gamma.diff, gamma.se=NA, group=levels[ii]))
        }
    }

    pdf1 <- pdf1.raw %>%
        group_by(group) %>% summarize(mu=mean(scc, na.rm=T), p025=quantile(scc, 0.025, na.rm=T),
                                      p05=quantile(scc, 0.05, na.rm=T),
                                      p25=quantile(scc, 0.25, na.rm=T), p50=quantile(scc, 0.5, na.rm=T),
                                      p75=quantile(scc, 0.75, na.rm=T), p95=quantile(scc, 0.95, na.rm=T),
                                      p975=quantile(scc, 0.975, na.rm=T))
    pdf1$group <- factor(pdf1$group, levels=rev(levels))
    gp1a <- ggplot(pdf1, aes(group)) +
        coord_flip(ylim=c(max(min(pdf1$p025, na.rm=T), -500), 5e3 - 1)) + scale_y_continuous(expand=c(0, 0)) +
        geom_boxplot(aes(min=p05, lower=p25, middle=p50, upper=p75, max=p95), stat="identity") +
        geom_point(aes(y=mu)) +
        geom_segment(aes(xend=group, y=p025, yend=p05), lty=2, lwd=0.4) +
        geom_segment(aes(xend=group, y=p975, yend=p95), lty=2, lwd=0.4) +
        theme_bw() + ylab("Global SCC ($2015/tCO2)") + xlab(NULL)
    gp1a

    gp1b <- ggplot(pdf1, aes(group)) +
        coord_flip(ylim=c(5e3, min(max(pdf1$p975, na.rm=T), 1e4, na.rm=T))) + scale_y_continuous(expand=c(0, 0)) +
        geom_boxplot(aes(min=p05, lower=p25, middle=p50, upper=p75, max=p95), stat="identity") +
        geom_point(aes(y=mu)) +
        geom_segment(aes(xend=group, y=p025, yend=p05), lty=2, lwd=0.4) +
        geom_segment(aes(xend=group, y=p975, yend=p95), lty=2, lwd=0.4) +
        theme_bw() + ylab("")
    gp1b

    pdf2.long <- cbind(melt(pdf2.mc[, c('group', 'alpha1', 'betam1', 'gammam1')], id='group'),
                       se=melt(pdf2.mc[, c('group', 'alpha1.se', 'beta.se', 'gamma.se')], id='group')$value)
    pdf2.long$group <- factor(pdf2.long$group, levels=rev(levels))

    pdf2.diff.long <- cbind(melt(pdf2.diff[, c('group', 'alpha1', 'betam1', 'gammam1')], id='group'),
                            se=melt(pdf2.diff[, c('group', 'alpha1.se', 'beta.se', 'gamma.se')], id='group')$value)
    pdf2.diff.long$group <- factor(pdf2.diff.long$group, levels=rev(levels))

    pdf3 <- rbind(cbind(pdf2.long, calc="raw"),
                  cbind(pdf2.diff.long, calc="diff")) %>% group_by(calc, group, variable) %>%
        summarize(mu=mean(value), med=median(value, na.rm=T),
                  ci25=quantile(value, .25, na.rm=T), ci75=quantile(value, .75, na.rm=T))
    pdf3$label <- factor(ifelse(pdf3$variable == "alpha1", "alpha_1", ifelse(pdf3$variable == "betam1", "beta - 1", "gamma - 1")),
                         levels=c("alpha_1", "beta - 1", "gamma - 1"))

    gp2 <- ggplot(pdf3, aes(group, group=paste(group, calc))) +
        facet_wrap(~ label, ncol=3, scales='free_x') +
        coord_flip() +
        geom_errorbar(aes(ymin=ci25, ymax=ci75, colour=calc), position="dodge") +
        geom_point(aes(y=med, colour=calc), position=position_dodge(width=.9)) +
        geom_hline(data=data.frame(label=c('alpha_1', 'beta - 1', 'gamma - 1'), value=c(0, 0, 0)), aes(yintercept=value), linetype='dashed') +
        scale_colour_manual(breaks=c('raw', 'diff'), values=c('#000000', '#800000')) +
        scale_y_continuous(breaks=breaks_extended(3)) + # + scale_x_discrete(limits=rev(levels[c(1, 5, 2, 3, 4)])) + <-- XXX: when plotting partial damages
        theme_bw() + theme(panel.spacing=unit(.7, "lines")) + guides(colour='none') + ylab("Coefficient values")
    gp2

    drop.y.labels <- theme(
        axis.title.y = element_blank(),  # Remove y-axis title
        axis.text.y = element_blank(),    # Remove y-axis text
        axis.ticks.y = element_blank()     # Remove y-axis ticks
    )
    gp <- plot_grid(gp1a + facet_wrap(~ "Distribution") + theme(plot.margin=margin(r=0)),
                    gp1b + drop.y.labels + facet_wrap(~ "Distribution") + theme(plot.margin=margin(l=0)),
                    gp2 + drop.y.labels + theme(plot.margin=margin(0, 1, 0, 6)),
                    rel_widths=c(1, 0.5, 1), nrow=1, ncol=3)
    gp.saved <<- gp
    print(gp)
    pdf1 %>% left_join(pdf2, by='group')
}

df <- read.csv("output/allscc.csv")
df %>% filter(country == 'global') %>% summarize(mu=median(scc, na.rm=T), ci25=quantile(scc, .25, na.rm=T), ci75=quantile(scc, .75, na.rm=T))
df %>% filter(country == 'KOR') %>% summarize(mu=median(scc, na.rm=T), ci25=quantile(scc, .25, na.rm=T), ci75=quantile(scc, .75, na.rm=T))

df <- read.csv("output/allscc-nodrupp.csv")
df %>% filter(country == 'KOR') %>% summarize(mu=median(scc, na.rm=T), ci25=quantile(scc, .25, na.rm=T), ci75=quantile(scc, .75, na.rm=T))

disp.year <- get.displays(c("output/allscc.csv", "output/allscc-2050.csv", "output/allscc-2100.csv"),
                          c('2020', '2050', '2100'))
ggsave("sccfig-year.pdf", width=8, height=2)

disp.scen <- get.displays(c("output/allscc.csv", "output/allscc-ssp126.csv", "output/allscc-ssp245.csv", "output/allscc-ssp585.csv"),
                          c('RFFSP', 'SSP1-2.6', 'SSP2-4.5', 'SSP5-8.5'))
ggsave("sccfig-scen.pdf", width=8, height=2)

disp.mktd <- get.displays(c("output/allscc.csv", "output/allscc-marketdmg-pageice.csv", "output/allscc-marketdmg-nooffset.csv", "output/allscc-marketdmg-constoffset.csv"),
                          c('Adaptive', 'PAGE-ICE', 'No offset', 'Constant offset'))
ggsave("sccfig-mktd.pdf", width=8, height=2)

disp.othd <- get.displays(c("output/allscc.csv", "output/allscc-otherdmg-pageice.csv", "output/allscc-otherdmg-pinational.csv", "output/allscc-otherdmg-pinonmarket.csv", "output/allscc-otherdmg-pislr.csv"),
                          c('Adaptive', 'PAGE-ICE', 'National PAGE-ICE', 'PAGE-ICE Non-Market', 'PAGE-ICE SLR'))
ggsave("sccfig-othd.pdf", width=8, height=2)

disp.macu <- get.displays(c("output/allscc.csv", "output/allscc-mac-pageice.csv"),
             c('Updated', 'PAGE-ICE'))
ggsave("sccfig-macu.pdf", width=8, height=1.5)

disp.down <- get.displays(c("output/allscc.csv", "output/allscc-downscale-pageice.csv"),
                          c('Updated', 'PAGE-ICE'))
ggsave("sccfig-down.pdf", width=8, height=1.5)

disp.subn <- get.displays(c("output/allscc.csv", "output/allscc-subnational.csv"),
                          c('National', 'Subnational'))
ggsave("sccfig-subn.pdf", width=8, height=1.5)

disp.part <- get.displays(c("output/allscc.csv", "output/allscc-onlydmg-nonmarket.csv",
                            "output/allscc-onlydmg-slr.csv"), #"output/allscc-onlydmg-market.csv", "output/allscc-onlydmg-discont.csv"),
                          c('Combined', 'Non-market-only', 'SLR-only'))#, 'Market-only', 'Discontinuity-only'))
## Note: After ran function by hand, added back both so I could get the tables
ggsave("output/figures/sccfig-part.pdf", width=8, height=2)

disp.capx <- get.displays(c("output/allscc.csv", "output/allscc-capital-constant.csv",
                            "output/allscc-capital-inferred.csv", "output/allscc-capital-full.csv"),
                          c('Old', 'Constant', 'Inferred', 'Full'))
ggsave("sccfig-capx.pdf", width=8, height=2)

write.csv(rbind(disp.year, disp.scen, disp.mktd, disp.othd, disp.macu, disp.down), "scc-options.csv", row.names=F)

prevtbl <- read.csv("output/scc-options.csv")
write.csv(rbind(disp.year, prevtbl[4:nrow(prevtbl),]), "output/scc-options.csv", row.names=F)

prevtbl <- read.csv("output/scc-options.csv")
write.csv(rbind(prevtbl, disp.part), "output/scc-options.csv", row.names=F)

for (filepath in c("output/allscc.csv", "output/allscc-2050.csv", "output/allscc-2100.csv")) {
    df <- read.csv(filepath)
    df2 <- df %>% filter(country == 'global')
    print(c(filepath, mean(df2$scc == 0)))
}

prevtbl <- read.csv("output/scc-options.csv")

disptbl <- prevtbl[c(-2:-4, -8, -12, -15, -18, -20:-21),]
disptbl$group[1] <- "Default Updated"
library(xtable)
print(xtable(disptbl[, c(1, 3:9, 11)]), include.rownames=F)
print(xtable(disptbl[, c(1, 13:19)]), include.rownames=F)
