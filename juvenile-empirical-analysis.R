# Libraries ----
library(tidyverse)
library(lubridate)
library(reshape)
library(R2jags)

# Data ----
# . Data read ----
# Read data file to reproduce the analysis
juv_haverstraw <- read.csv("data/juv_haverstraw.csv")


# . Capture histories ----
# Capture history of counts by site and year
# Note different order!
caps <- cast(juv_haverstraw, SITE ~ YEAR ~ REP, 
            fun.aggregate = sum,
            value = "NUMBER",
            add.missing = FALSE)

# Here is a capture history for the occupancy process
caps_binom <- caps
caps_binom[caps_binom > 0] <- 1


# Analysis ----
# . Jags data ----
# Format data
jags.data <- list(nSites = length(unique(juv_haverstraw$SITE)),
                  nYears = length(unique(juv_haverstraw$YEAR)),
                  nReps = length(unique(juv_haverstraw$REP)),
                  y = caps)

# . Parameters monitored----
params <- c("N", "p", "lambda", 
            "logit_p_mu", "r", "r_mu",
            "sum_y", "sum_y_rep")

# . Initial values ----
n_init <- apply(caps, 2, max, na.rm = TRUE)
n_init <- n_init + 10
n_init[is.infinite(n_init)] <- 10

# For testing zero-inflation
# z_init <- caps[,,1]
# z_init[!is.na(z_init)] <- 1

inits = function(){
  list(
    N = n_init
  )
}  

# . Compile model ----
fit <- jags(jags.data, inits = inits,
            parameters.to.save = params,
            model.file = "models/n-mixture-jags",
            n.chains = 3, n.iter = 500000,
            n.burnin = 150000, n.thin = 5)

# Print summary
print(fit, digits=3)

# . Save result file ----
# Save results and data used to run it
save(fit, file = "results/juv_posts.rda")