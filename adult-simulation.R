# Libraries ----
library(tidyverse)
library(reshape)
library(R2jags)
library(snowfall)

# Empirical estimates ----
load("results/adult_posts.rda")
posts <- fit$BUGSoutput$sims.list
names(posts)

# . Individual detection probability (p) ----
p <- melt(posts$p)
names(p) <- c("iteration", "site", "year", "value")
p <- p %>% 
  group_by(site, year) %>% 
  summarize(fit = median(value)) %>% 
  pivot_wider(names_from = year, values_from = fit) 
p <- as.matrix(p[, 2:ncol(p)])
dimnames(p) <- NULL

# . Population growth ----
n <- melt(posts$N)
names(n) <- c("iteration", "year", "value")
n <- n %>% 
  group_by(year) %>% 
  summarize(fit = median(value))


# . Starting abundance (lambda) ----
lambda <- median(posts$lambda)

# . Population growth rate (r) ----
r <- mean(posts$r_mu)


# Simulation function ----
sim_fun <- function(idx){
  # . Simulation settings ----
  # .. Design considerations ----
  n_years <- 17
  n_sites <- 3
  n_reps <- 30
  
  # .. Parameter values ----
  # Use mean value of each parameter to generate capture histories
  lambda_sim <- lambda
  p_sim <- matrix(mean(p), nrow = n_sites, ncol = n_years)
  r_sim <- r

  # . Data simulation ----
  # .. Abundance ----
  # True local abundance at each site in each year
  N <- vector(mode = "numeric", length = n_years)

  # Initial
  N[1] <- rpois(1, lambda_sim)

  # Population change from year 2 through n_years
  for(t in 2:n_years) {
    N[t] <- rpois(1, N[t-1]*exp(r))
  }

  # Round to whole numbers for binomial rng below
  N <- round(N)

  # True abundance in the whole study area each year
  n_true <- N
  
  # .. Detection array ----
  # Catch matrix
  y <- array(NA, c(n_sites, n_years, n_reps))
  for(i in 1:n_sites) {
    for(t in 1:n_years) {
      for(j in 1:n_reps) {
        y[i, t, j] <- rbinom(n = 1, size = N[t], prob = p_sim[i, t])          
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
  z <- apply(y, 2, max, na.rm = TRUE)
  z <- z + 10
  z[is.infinite(z)] <- 10
  
  inits <- function(){
    list(
      N = z
    )
  }
  
  # .. Parameters monitored----
  params <- c("N", "p", "lambda", "logit_p_mu", "r", "r_mu")
  
  # .. Compile model ----
  sim_fit <- jags(jags.data, inits = inits,
             parameters.to.save = params,
             model.file = "models/n-mixture-jags-sim",
             n.chains = 3, n.iter = 1000,
             n.burnin = 500, n.thin = 5)
  
  # .. Parameter estimates ----
  # Estimated posteriors as a list
  posts_est <- sim_fit$BUGSoutput$sims.list
  
  # .. Abundance ----
  n_est <- melt(posts_est$N)
  names(n_est) <- c("iteration", "year", "N")

  n_est <- n_est %>%
    group_by(year) %>%
    summarize(n_obs = median(N))
  
  # Detection ----
  p_est <- melt(posts_est$p)
  names(p_est) <- c("iteration", "site", "year", "p")
  p_est <- p_est %>%
    group_by(year) %>%
    summarize(p = median(p))
  
  # Lambda
  lambda_est <- median(posts_est$lambda)
  
  # Population growth (r)
  r_est <- median(posts_est$r)  
  
  # Difference between estimated and true abundance
  bias <- n_est$n_obs - n_true

  # . Output ----
  out <- data.frame("n_est" = n_est$n_obs, 
                    "n_true" = as.vector(n_true), 
                    "bias" = as.vector(bias),
                    "year" = seq(1, n_years, 1),
                    "n_years" = n_years,
                    "n_reps" = n_reps,
                    "n_sites" = n_sites,
                    "p_sim" = apply(p, 2, mean),
                    "p_est" = p_est$p,
                    "lambda_sim" = lambda,
                    "lambda_est" = lambda_est,
                    "r_sim" = r,
                    "r_est" = r_est)

  return(out)

} 


# Parallel simulation ----
# . Initialize snowfall ----
# Initialize snowfall using number of virtual threads minus one
# most PCs have 4-8 virtual cores (cpus x 2)
sfInit(parallel=TRUE, cpus=10, type="SOCK")

# . Export data ----
# Export data needed for simulation to workers from global env
sfExport(list = c("p", "lambda", "r"))

# . Export libraries ----
# Load R packages on workers
sfLibrary(R2jags)
sfLibrary(reshape)
sfLibrary(tidyverse)


# . Number of iterations ----
niterations <- 10

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
# save(results, file = "results/adult_sim_results.rda")
load("results/adult_sim_results.rda")

# . Descriptive statistics for bias ----
bias_median <- median(results$bias)
bias_quants <- unname(quantile(results$bias, c(0.025, 0.975)))
bias_median
bias_quants

# . Quick plots of bias (Figure S3) ----
hist <- ggplot(results, aes(x = bias)) +
  geom_histogram(bins = 300) +
  geom_vline(xintercept = bias_median, color = "blue") +
  ylab("Frequency") +
  xlab("Bias") +
  annotate("text", x = -1500, y = 500, label = "(a)") +
  scale_y_continuous(limits = c(0,600))
  
bias_dir <- ggplot(results, aes(x = n_true, y = n_est)) + 
  geom_point() + 
  geom_smooth(method = "lm") +
  xlab(expression(paste("N"[true]))) +
  ylab(expression(paste("N"[estimated]))) +
  annotate("text", x = 0, y = 500, label = "(b)") +
  scale_y_continuous(limits = c(0,600))


line_year <- ggplot(check_year, aes(x = year, y = fit)) +
  geom_ribbon(aes(xmax = year, ymin = lwr, ymax = upr, color = NULL),
              alpha = 0.1) +
  geom_line() +
  ylab("Bias") +
  xlab("Year") +
  annotate("text", x = 0, y = -250, label = "(c)") +
  scale_y_continuous(limits = c(-1600, 0))

point_n <- ggplot(check_n, aes(x = n_true, y = fit)) +
  geom_point() +
  geom_smooth() +
  ylab("Bias") +
  xlab(expression(paste("N"[true]))) +
  annotate("text", x = 0, y = -250, label = "(d)") +
  scale_y_continuous(limits = c(-1600, 0))

jpeg("results/FigureS3.jpg",
     res = 300,
     width = 2000, 
     height = 2000)
gridExtra::grid.arrange(hist, bias_dir, line_year, point_n)
dev.off()

# . Parameter bias (Figure S4) ----
results$p_bias <- results$p_est - results$p_sim
results$lambda_bias <- results$lambda_est - results$lambda_sim
results$r_bias <- results$r_est - results$r_sim

lambda_bias_mean <- mean(results$lambda_bias)
r_bias_mean <- mean(results$r_bias)

lambda_bias_plot <- ggplot(results, aes(x = p_bias, y = lambda_bias)) +
  geom_point(alpha = 0.05) +
  geom_hline(yintercept = lambda_bias_mean, linetype = 2) +
  xlab(expression(paste(italic(p)["Bias"]))) +
  ylab(expression(paste(lambda["Bias"])))

r_bias_plot <- ggplot(results, aes(x = p_bias, y = r_bias)) +
  geom_point(alpha = 0.05) +
  geom_hline(yintercept = r_bias_mean, linetype = 2) +
  xlab(expression(paste(italic(p)["Bias"]))) +
  ylab(expression(paste(italic(r)["Bias"])))

jpeg("results/FigureS4.jpg",
     res = 300,
     width = 2000, 
     height = 2400)
gridExtra::grid.arrange(lambda_bias_plot, r_bias_plot)
dev.off()


# Check patterns for n_true and n_est (Figure S5) ----
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
  ylab(expression(paste("N"[estimated]))) +
  xlab("Year")

n_true <- ggplot(check_pat, aes(x = year, y = fit2)) +
  geom_ribbon(aes(xmax = year, ymin = lwr2, ymax = upr2, color = NULL),
              alpha = 0.1) +
  geom_line() +
  ylab(expression(paste("N"[true]))) +
  xlab("Year")

jpeg("results/FigureS5.jpg",
     res = 300,
     width = 2000, 
     height = 2000)
gridExtra::grid.arrange(n_est, n_true)
dev.off()


