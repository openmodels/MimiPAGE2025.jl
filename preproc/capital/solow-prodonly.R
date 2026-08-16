setwd("~/research/iamup2/MimiPAGE2020.jl/preproc/capital/")

library(readxl)
library(dplyr)
library(reshape2)
library(countrycode)
library(rstan)

source("utils2.R")

load.solowdata()

stan.model <- "
data {
  int<lower=0> T;
  vector[T] pop;

  real maxprocap0;

  int<lower=0> N1;
  real gdp[N1];
  int<lower=0, upper=T> gdp_year[N1];

  int<lower=0> N2;
  real procap[N2];
  int<lower=0, upper=T> cap_year[N2];

  int<lower=0> N4;
  real sav[N4];
  int<lower=0, upper=T> sav_year[N4];

  real<lower=0, upper=1> deprrate_prior;
}
parameters {
  real<lower=0> tfp;
  real dtfpdt;

  // Produced Capital
  real<lower=0, upper=1> procap0part;
  real<lower=0, upper=1> saverate0;
  real<lower=-.1, upper=.1> dsaveratedt;
  real<lower=0, upper=1> deprrate;
  real<lower=0> procap_error;
  real<lower=0> sav_error;

  // GDP production
  simplex[2] shares0; // pro, pop
  simplex[2] sharesT; // pro, pop
  real<lower=0> shares_error;
  vector<lower=0, upper=1>[T-1] cumulpart;

  real<lower=0> gdp_error;
}
transformed parameters {
  vector<lower=0>[T-1] product; // calculates year 1 product for year 2 capital
  vector<lower=0>[T] procap_model;

  procap_model[1] = procap0part * maxprocap0;

  for (tt in 2:T) {
    product[tt-1] = (tfp + dtfpdt * (tt-1)) * pow(procap_model[tt-1], (shares0[1] + (tt-2) * (sharesT[1] - shares0[1]) / (T-2))) * pow(pop[tt-1], (shares0[2] + (tt-2) * (sharesT[2] - shares0[2]) / (T-2)));
    procap_model[tt] = procap_model[tt-1] + (saverate0 + dsaveratedt * (tt-2)) * product[tt-1] - deprrate * procap_model[tt-1];
  }
}
model {
  // Match observations
  for (ii in 1:N1) {
    if (gdp_year[ii] > 1)
      gdp[ii] ~ lognormal(log(product[gdp_year[ii]-1]), gdp_error);
  }
  for (ii in 1:N2) {
    procap[ii] ~ lognormal(log(procap_model[cap_year[ii]]), procap_error);
  }
  for (ii in 1:N4) {
    sav[ii] ~ normal(saverate0 + dsaveratedt * (sav_year[ii]-2), sav_error);
  }

  // Model logic
  dsaveratedt ~ normal(0, sav_error);

  // Priors
  deprrate ~ normal(deprrate_prior, .1);
  sharesT[1] - shares0[1] ~ normal(0, shares_error);
  sharesT[2] - shares0[2] ~ normal(0, shares_error);
  shares_error ~ normal(0, 0.1);
}"

mod <- stan_model(model_code=stan.model)

allrows <- lapply(levels(df$ISO), function(iso) {
    print(c(iso))
    stan.data <- make.stan.data(iso)

    fit <- tryCatch({
        sampling(mod, data=stan.data, open_progress=F, chains=4, cores=4)
    }, error=function(ee) {
        NULL
    })
    if (is.null(fit))
        return(data.frame())
    la <- extract(fit, permute=T)
    if (is.null(la))
        return(data.frame())

    ess <- mean(stan_ess(fit)$data$stat)
    lp <- mean(la$lp__)

    row <- data.frame(ISO=iso,
                      product.end.true=mean(la$product[, 30]),
                      procap.end.true=mean(la$procap_model[, 31]),
                      ess, lp)

    save(la, file=paste0("fits/", iso, ".RData"))

    row
})

sumbymc <- data.frame()
for (ii in 1:length(allrows))
    sumbymc <- rbind(sumbymc, allrows[[ii]])

write.csv(sumbymc, "fits.csv", row.names=F)
