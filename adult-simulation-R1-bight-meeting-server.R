# Libraries ----
library(tidyverse)
library(reshape)
library(R2jags)
library(snowfall)


# Simulation function ----
sim_fun <- function(idx){
  
  
  # . Simulation settings ----
  
  
  # .. Empirical estimates ----
  
  
  # ... Individual detection probability (p) ----
  # Overall mean from Stich et al. (2025)
  p <- 0.068
  
  
  # ... Population abundance ----
  # Overall mean from Stich et al. (2025)
  n <- 30
  
  
  # ... Annual abundance (lambda) ----
  # Overall mean from Stich et al. (2025)
  lambda <- 30
  
  
  # .. Design considerations ----
  n_years <- 30
  n_sites <- 3
  n_reps <- sample(c(1, 5, 10, 15, 20, 25, 30), 1, replace = TRUE)
  

  # .. Parameter values ----
  # Use mean value of each parameter to generate capture histories
  lambda_sim <- lambda
  p_sim <- p
  

  # . Data simulation ----
  # .. Abundance ----
  # True local abundance at each site in each year
  N <- matrix(NA, nrow = n_sites, ncol = n_years)

  # Population change from year 2 through n_years
  for(i in 1:n_sites){
    for(t in 1:n_years) {
      N[i, t] <- rpois(n = 1, lambda = lambda_sim)
    }
  }
  
  # True abundance in the whole study area each year
  n_total_true <- apply(N, 2, sum)
  

  # .. Detection array ----
  # Catch matrix
  y <- array(NA, c(n_sites, n_years, n_reps))
  for(i in 1:n_sites) {
    for(t in 1:n_years) {
      for(j in 1:n_reps) {
        y[i, t, j] <- rbinom(n = 1, size = N[t], prob = p_sim)          
      }
    }
  }
  
  
  # . Analysis of data set ----
  # .. Data for jags ----
  jags.data <- list(nSites = n_sites,
                    nYears = n_years,
                    nReps = n_reps,
                    y = y)
  

  # .. Initial values ----
  n_init <- y[,,1]
  n_init[is.na(n_init)] <- 1
  n_init <- n_init + 100
  n_init[is.infinite(n_init)] <- 100
  
  inits = function(){
    list(
      N = n_init
    )
  }  
  
  
  # .. Parameters monitored----
  params <- c("n_total", "N", "lambdap", "p", "logit_p_mu")
  
  
  # .. Compile model ----
  sim_fit <- jags(jags.data, inits = inits,
             parameters.to.save = params,
             model.file = "models/poisson-itt-jags-bight-sim",
             n.chains = 3, n.iter = 10000,
             n.burnin = 1000, n.thin = 10)
  
  print(sim_fit, digits = 3)

  # .. Parameter estimates ----
  # Estimated posteriors as a list
  posts_est <- sim_fit$BUGSoutput$sims.list
  

  # .. Abundance ----
  n_est <- melt(posts_est$n_total)
  names(n_est) <- c("iteration", "year", "n")

  n_est <- n_est %>%
    group_by(year) %>%
    summarize(n_total_est = median(n),
              sd_est = sd(n))
  

  # Detection ----
  p_est <- melt(posts_est$p)
  names(p_est) <- c("iteration", "site", "year", "rep", "p")
  p_est <- p_est %>%
    group_by(year) %>%
    summarize(p = median(p))
  
  # Lambda
  lambda_est <- median(posts_est$lambdap)
  
  # Difference between estimated and true abundance
  bias <- n_est$n_total_est - n_total_true

  # . Output ----
  out <- data.frame("n_est" = n_est$n_total_est, 
                    "n_sd" = n_est$sd_est,
                    "n_true" = n_total_true, 
                    "bias" = as.vector(bias),
                    "year" = seq(1, n_years, 1),
                    "n_years" = n_years,
                    "n_reps" = n_reps,
                    "n_sites" = n_sites,
                    "p_sim" = mean(p_sim),
                    "p_est" = mean(p_est$p),
                    "lambda_sim" = lambda_sim,
                    "lambda_est" = lambda_est)

  return(out)

} 


# Parallel simulation ----
# . Initialize snowfall ----
# Initialize snowfall using number of virtual threads minus one
# most PCs have 4-8 virtual cores (cpus x 2)
sfInit(parallel = TRUE, cpus = 10, type = "SOCK")


# . Export data ----
# Export data needed for simulation to workers from global env
sfExport(list = c("p", "lambda"))


# . Export libraries ----
# Load R packages on workers
sfLibrary(R2jags)
sfLibrary(reshape)
sfLibrary(tidyverse)


# . Number of iterations ----
niterations <- 1000


# . Run simulation ----
# Store and print start time
start <- Sys.time()
start


# Distribute job to workers
res <- sfLapply(1:niterations, sim_fun)

# Calculate run time, save, and print
total_time <- Sys.time() - start
total_time

# Stop snowfall
sfStop()


# Results ----
# . Extract output ----
results <- do.call(rbind, res)


# . Save to a file ----
# save(results, file = "results/adult_sim_results_bight.rda")
