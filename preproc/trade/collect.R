## setwd("~/Library/CloudStorage/GoogleDrive-jrising@udel.edu/My Drive/Research/Current Losses")

library(raster)
library(dplyr)

load.fd <- function(year) {
    if (year < 1990)
        return(load.fd(1990))
    if (year > 2016)
        return(load.fd(2016))

    yearstr <- as.character(year)

    datapath <- paste0("data/I_O data/Eora26/Eora26_", year, "_bp")
    VA <- as.matrix(read.delim(file.path(datapath, paste0("Eora26_", year, "_bp_VA.txt")), sep='\t', header=F))
    FD <- as.matrix(read.delim(file.path(datapath, paste0("Eora26_", year, "_bp_FD.txt")), sep='\t', header=F))

    FD2 <- matrix(0, nrow(FD), ncol(FD) / 6)
    for (ii in 1:6)
        FD2[,] <- FD2 + FD[, seq(ii, ncol(FD), by=6)]

    labels <- read.delim(file.path(datapath, "labels_T.txt"), sep='\t', header=F)
    labels$V1 <- factor(labels$V1, levels=unique(labels$V1))

    labels$VA <- colSums(VA)

    labels2 <- labels %>% group_by(V1) %>% summarize(VA=sum(VA))

    return(list(FD=FD2, labels=labels2))
}

## Returns list("TT"=matrix, labels=data.frame)
load.io <- function(year) {
    if (year < 1990)
        return(load.io(1990))
    if (year > 2016)
        return(load.io(2016))

    yearstr <- as.character(year)

    fd.data <- load.fd(year)
    labels2 <- fd.data$labels

    datapath <- paste0("data/I_O data/Eora26/Eora26_", year, "_bp")
    TT <- as.matrix(read.delim(file.path(datapath, paste0("Eora26_", year, "_bp_T.txt")), sep='\t', header=F))

    rTT <- raster(TT)
    rTT2 <- aggregate(rTT, 26, sum)
    TT2 <- as.matrix(rTT2)

    labels2$FD <- colSums(fd.data$FD)

    return(list(TT=TT2, labels=labels2))
}

io.data <- load.io(2015)
names(io.data$labels)[1] <- "ISO"
write.csv(io.data$labels, "labels.csv", row.names=F)
write.csv(io.data$TT, "TT.csv")

## Move to data/trade
