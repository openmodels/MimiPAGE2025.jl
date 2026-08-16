library(tidyr)
library(ggplot2)

pm2lngdp <- -0.00545
lnpm2lngdp <- -0.1175

indiapm <- 41.4 # 2022 from EPIC

df <- tibble(pm=rev(seq(0, 41.4, length.out=100)),
             pm.clip=pmin(pm, 20),
             loglev=c(0, pm2lngdp * diff(pm)),
             loglog=c(0, lnpm2lngdp * diff(log(pm))),
             dlngdp=pmin(loglev, loglog),
             choose=ifelse(dlngdp == loglev, "log-level", "log-log"),
             gdpchange=(exp(cumsum(dlngdp)) - 1),
             loglev.clip=c(0, pm2lngdp * diff(pm.clip)),
             loglog.clip=c(0, lnpm2lngdp * diff(log(pm.clip))),
             dlngdp.clip=pmin(loglev.clip, loglog.clip),
             gdpchange.clip=(exp(cumsum(dlngdp.clip)) - 1))


ggplot(df, aes(pm, dlngdp)) +
    geom_line()

ggplot(df, aes(pm, gdpchange)) +
    geom_line(aes(colour="No clipping")) +
    geom_line(aes(y=gdpchange.clip, colour="Clip above 20 µg/m³")) +
    scale_x_reverse("PM2.5 Level (µg/m³), decreasing from India in 2022", expand=c(0, 0)) +
    scale_y_continuous("Increase in GDP from reductions (%)", labels=scales::percent) +
    scale_colour_discrete("Assumption:") +
    theme_bw()
