# Libraries ----
library(tidyverse)
library(reshape)
library(R2jags)
library(snowfall)


# Empirical estimates ----
load("results/adult_posts_poisson_itt.rda")
posts <- fit$BUGSoutput$sims.list
names(posts)


# . Individual detection probability (p) ----
p <- melt(posts$p)
names(p) <- c("iteration", "site", "year", "rep", "value")
p <- p %>% 
  group_by(site, year, rep) %>% 
  summarize(fit = median(value))
p <- reshape::cast(p, site ~ year ~ rep)


# . Population abundance ----
n <- melt(posts$N)
names(n) <- c("iteration", "site", "year", "value")
n <- n %>% 
  group_by(site, year) %>% 
  summarize(fit = median(value))


# . Annual abundance (lambda) ----
lambda <- melt(posts$lambda)
names(lambda) <- c("iteration", "year", "value")
lambda <- lambda %>% 
  group_by(year) %>% 
  summarize(fit = round(median(value)))


# Simulation function ----
sim_fun <- function(idx){
  # . Simulation settings ----
  # .. Design considerations ----
  n_years <- nrow(lambda)
  n_sites <- 3
  n_reps <- 10
  

  # .. Parameter values ----
  # Use mean value of each parameter to generate capture histories
  lambda_sim <- lambda$fit
  p_sim <- p
  

  # . Data simulation ----
  # .. Abundance ----
  # True local abundance at each site in each year
  N <- matrix(NA, nrow = n_sites, ncol = n_years)

  # Population change from year 2 through n_years
  for(i in 1:n_sites){
    for(t in 1:n_years) {
      N[i, t] <- rpois(n = 1, lambda = lambda_sim[t])
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
        y[i, t, j] <- rbinom(n = 1, size = N[t], prob = p_sim[i, t, j])          
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
             model.file = "models/poisson-itt-jags",
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
    summarize(n_total_est = median(n))
  

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
sfInit(parallel=TRUE, cpus=10, type="SOCK")


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
# save(results, file = "results/adult_sim_results_poisson.rda")
# load("results/adult_sim_results_poisson.rda")


# . Descriptive statistics for bias ----
bias_median <- median(results$bias)
bias_quants <- unname(quantile(results$bias, c(0.025, 0.975)))
bias_median
bias_quants


# . Quick plots of bias (Figure S2) ----
hist <- ggplot(results, aes(x = bias)) +
  geom_histogram(bins = 300) +
  geom_vline(xintercept = bias_median, color = "blue") +
  ylab("Frequency") +
  xlab("Bias") +
  annotate("text", x = -160, y = 590, label = "(a)")

bias_dir <- ggplot(results, aes(x = n_true, y = n_est)) + 
  geom_point() + 
  geom_abline(aes(slope = 1, intercept = 0)) +
  xlab(expression(paste(italic("N")[true]))) +
  ylab(expression(paste(italic("N")[estimated]))) +
  annotate("text", x = 0, y = 925, label = "(b)")

check_year <- results %>% 
  group_by(year) %>% 
  summarize(fit = mean(bias),
            lwr = quantile(bias, 0.025),
            upr = quantile(bias, 0.975))

line_year <- ggplot(check_year, aes(x = year, y = fit)) +
  geom_ribbon(aes(xmax = year, ymin = lwr, ymax = upr, color = NULL),
              alpha = 0.1) +
  geom_line() +
  ylab("Bias") +
  xlab("Year") +
  annotate("text", x = 1, y = 240, label = "(c)")

check_n <- results %>% 
  group_by(year) %>% 
  summarize(fit = mean(bias),
            lwr = quantile(bias, 0.025),
            upr = quantile(bias, 0.975))

point_n <- ggplot(results, aes(x = n_true, y = bias)) +
  geom_point() +
  geom_smooth(method = "loess") +
  ylab("Bias") +
  xlab(expression(paste(italic("N")[true])))  +
  annotate("text", x = 5, y = 850, label = "(d)")

jpeg("results/FigureS2.jpg",
     res = 300,
     width = 2000,
     height = 2000)
gridExtra::grid.arrange(hist, bias_dir, line_year, point_n)
dev.off()

# . Parameter bias (Figure S3) ----
results$p_bias <- results$p_est - results$p_sim
results$lambda_bias <- results$lambda_est - results$lambda_sim

lambda_bias_mean <- mean(results$lambda_bias)
lambda_bias_plot <- ggplot(results, aes(x = p_bias, y = lambda_bias)) +
  geom_point(alpha = 0.05) +
  geom_hline(yintercept = lambda_bias_mean, linetype = 2) +
  xlab(expression(paste(italic(p)["Bias"]))) +
  ylab(expression(paste(lambda["Bias"])))

# r_bias_mean <- mean(results$r_bias)
# r_bias_plot <- ggplot(results, aes(x = p_bias, y = r_bias)) +
#   geom_point(alpha = 0.05) +
#   geom_hline(yintercept = r_bias_mean, linetype = 2) +
#   xlab(expression(paste(italic(p)["Bias"]))) +
#   ylab(expression(paste(italic(r)["Bias"])))

jpeg("results/FigureS3.jpg",
     res = 300,
     width = 2000, 
     height = 2400)
 
lambda_bias_plot
 
dev.off()


# Check patterns for n_true and n_est (Figure S4) ----
check_pat <- results %>%
  group_by(year) %>%
  summarize(fit = median(n_est),
            lwr = quantile(n_est, 0.025),
            upr = quantile(n_est, 0.975),
            fit2 = median(n_true),
            lwr2 = quantile(n_true, 0.025),
            upr2 = quantile(n_true, 0.975))

n_est <- ggplot(check_pat, aes(x = year, y = fit)) +
  geom_ribbon(aes(xmax = year, ymin = lwr, ymax = upr, color = NULL),
              alpha = 0.1) +
  geom_line() +
  ylab(expression(paste(italic("N")[estimated]))) +
  xlab("Year")

n_true <- ggplot(check_pat, aes(x = year, y = fit2)) +
  geom_ribbon(aes(xmax = year, ymin = lwr2, ymax = upr2, color = NULL),
              alpha = 0.1) +
  geom_line() +
  ylab(expression(paste(italic("N")[true]))) +
  xlab("Year")

jpeg("results/FigureS4.jpg",
     res = 300,
     width = 2000,
     height = 2000)
gridExtra::grid.arrange(n_est, n_true)
dev.off()

