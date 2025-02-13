setwd("~/research/iamup2/MimiPAGE2020.jl/preproc/capital/")

source("utils2.R")
load.solowdata()

allres <- data.frame()
for (filename in list.files("fits")) {
    print(filename)
    load(file.path("fits", filename))

    years <- 1990:2020
    saverate <- la$saverate0 + la$dsaveratedt * which(years == 2015)
    capshare <- la$shares0[, 1] + which(years == 2015) * (la$sharesT[, 1] - la$shares0[, 1]) / length(years)
    procap0 <- la$procap_model[, which(years == 2015)] * df2$denom[df2$ISO == substring(filename, 1, 3)][1]

    isores <- data.frame(ISO=substring(filename, 1, 3), mc=1:4000, procap0, saverate, deprrate=la$deprrate, capshare)
    allres <- rbind(allres, isores)
}

library(dplyr)

sumres <- allres %>% group_by(ISO) %>% summarize(procap0.sd=sd(procap0), procap0=mean(procap0),
                                                 saverate.sd=sd(saverate), saverate=mean(saverate),
                                                 deprrate.sd=sd(deprrate), deprrate=mean(deprrate),
                                                 capshare.sd=sd(capshare), capshare=mean(capshare))

library(ggplot2)
library(reshape2)

mdf <- melt(sumres[, c('ISO', 'procap0', 'saverate', 'deprrate', 'capshare')])
mdf$variable <- sapply(mdf$variable, function(col) list('procap0'="2015 Produced Capital (USD)",
                                                        'saverate'="Savings Rate (portion)",
                                                        'deprrate'="Depreciation Rate (portion)",
                                                        'capshare'="Capital Share (unitless)")[[col]])

ggplot(mdf, aes(value)) +
    facet_wrap(~ variable, scales='free') +
    geom_histogram() + scale_x_log10() + theme_bw()

outres <- subset(allres, mc <= 1000)
outres$procap0 <- round(outres$procap0 / 1e6)
for (cc in 4:6) {
    print(quantile(outres[, cc]))
    outres[, cc] <- round(outres[, cc, drop=T], 6)
}
write.csv(outres, "../../data/other/capital.csv", row.names=F)
