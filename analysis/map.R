source("~/projects/research-common/R/myPBSmapping.R")
library(dplyr)

byisos <- read.csv("../data/bycountry.csv")
byaggs <- read.csv("../data/aggregates.csv")

shp <- importShapefile("~/data/political/ne_10m_admin_0_countries_lakes/ne_10m_admin_0_countries_lakes.shp")
polydata <- attr(shp, 'PolyData')
polydata$code <- as.character(polydata$ADM0_A3)
polydata$code[polydata$ADMIN == "South Sudan"] <- "SSD"
polydata$code[polydata$ADMIN == "Palestine"] <- "PSE"
polydata$code[polydata$ADMIN == "Kosovo"] <- "XKX"
polydata$code[polydata$ADMIN == "Western Sahara"] <- "ESH"
## We also distinguish Tokelau, but it is in the New Zealand polygons
polydata$code[polydata$ADMIN == "Somaliland"] <- "SOM"

polydata2 <- polydata %>% left_join(byaggs, by=c('code'='ISO')) %>% left_join(byisos, by=c('code'='ISO3'))
polydata2$code[!is.na(polydata2$Aggregate)] <- polydata2$Aggregate[!is.na(polydata2$Aggregate)]

cents <- calcCentroid(shp, rollup=2)
areas <- calcArea(shp, rollup=2)
centroids <- cents %>% left_join(areas, by=c('PID', 'SID')) %>% group_by(PID) %>%
    dplyr::summarize(X=X[which.max(area)], Y=Y[which.max(area)])
centroids$Y[centroids$PID == which(polydata$SOV_A3 == 'JPN')] <- centroids$Y[centroids$PID == which(polydata$SOV_A3 == 'JPN')] + 4
centroids$X[centroids$PID == which(polydata$SOV_A3 == 'JPN')] <- centroids$X[centroids$PID == which(polydata$SOV_A3 == 'JPN')] + 2

source("~/projects/research-common/R/distance.R")
centroids$show <- F
for (PID in order(polydata$POP_EST, decreasing=T)) {
    dists <- gcd.slc(centroids$X[PID] / 2, centroids$Y[PID], centroids$X[centroids$show] / 2, centroids$Y[centroids$show])
    if (all(dists > 500))
        centroids$show[PID] <- T
}
centroids$show[centroids$X < -176] <- F
centroids$show[centroids$X > 176] <- F
centroids$show[centroids$Y < -50] <- F
centroids$show[centroids$Y > 65] <- F
## Only show largest for each aggregate
aggtokeep <- polydata2 %>% filter(!is.na(Aggregate)) %>% group_by(Aggregate) %>% summarize(PID=PID[which.max(POP_EST)])
centroids$show[!is.na(polydata2$Aggregate) & !(polydata2$PID %in% aggtokeep$PID)] <- F

make.map <- function(df, isocol, valcol, title, palette="YlOrRd", direction=1, trans="log10") {
    polydata3 <- polydata2 %>% left_join(df, by=c('code'=isocol))
    polydata3$scc <- polydata3[, valcol]
    shp2 <- shp %>% left_join(polydata3[, c('PID', 'scc')])

    centroids2 <- centroids %>% left_join(polydata3[, c('PID', 'scc')])

    ggplot(shp2, aes(X, Y)) +
        geom_polygon(aes(fill=scc, group=paste(PID, SID))) +
        geom_label(data=subset(centroids2, show & !is.na(scc) & scc > 0.005), aes(label=format(round(scc, 2), nsmall=2, trim=T)), size=2, label.padding=unit(0.1, "lines")) +
        theme_bw() + scale_x_continuous(NULL, expand=c(0, 0)) + scale_y_continuous(NULL, expand=c(0, 0)) +
        scale_fill_distiller(title, palette=palette, direction=direction, labels=scales::comma, trans=trans)
}
