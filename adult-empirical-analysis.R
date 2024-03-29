# Libraries ----
library(tidyverse)
library(lubridate)
library(reshape)
library(R2jags)

# Data ----
# . Data read ----
# Read data file to reproduce the analysis
adult_norrie <- read.csv("data/adult_norrie.csv")


# . Capture histories ----
# Counts
caps <- cast(adult_norrie, SITE ~ YEAR ~ REP, 
            fun.aggregate = sum,
            value = "NUMBER",
            fill = NA,
            drop = FALSE,
            add.missing = TRUE
            )

# Here is a capture history for the occupancy process
caps_binom <- caps
caps_binom[caps_binom > 0] <- 1

# Analysis ----
# . Jags data ----
# Format data
jags.data <- list(nSites = length(unique(adult_norrie$SITE)),
                  nYears = length(unique(adult_norrie$YEAR)),
                  nReps = length(unique(adult_norrie$REP)),
                  y = caps)

# . Parameters monitored----
params <- c("N", "p", "lambda", 
            "logit_p_mu", "r", "r_mu",
            "sum_y", "sum_y_rep", 
            "bayesian_p_mean")

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
            n.chains = 3,
            n.iter = 50,
            n.burnin = 15, 
            n.thin = 5)

# Print summary
print(fit, digits=3)

# . Save result file ----
# Save results and data used to run it
save(fit, file = "results/adult_posts.rda")
