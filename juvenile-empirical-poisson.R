# Libraries ----
library(tidyverse)
library(lubridate)
library(reshape)
library(R2jags)


# Data ----
# . Data read ----
# Read data file to reproduce the analysis
juv_haverstraw <- read.csv("data/juv_haverstraw.csv")

juv_haverstraw <- juv_haverstraw %>%
  filter(!is.na(BATCH))

# . Capture histories ----
# Capture history of counts by site and year
caps <- cast(juv_haverstraw, SITE ~ YEAR ~ REP, 
            fun.aggregate = sum,
            value = "NUMBER",
            add.missing = FALSE)

# Analysis ----
# . Jags data ----
# Format data
jags.data <- list(nSites = length(unique(juv_haverstraw$SITE)),
                  nYears = length(unique(juv_haverstraw$YEAR)),
                  nReps = length(unique(juv_haverstraw$REP)),
                  y = caps)

# . Parameters monitored----
params <- c("n_total", "N", "p", "lambdap", "sum_E", "sum_E_rep")

# . Initial values ----
n_init <- caps[,,1]
n_init[is.na(n_init)] <- 1
n_init <- n_init + 100
n_init[is.infinite(n_init)] <- 100

inits = function(){
  list(
    N = n_init
  )
}  

# . Compile model ----
fit <- jags(jags.data, inits = inits,
            parameters.to.save = params,
            model.file = "models/poisson-itit-jags",
            n.chains = 3, n.iter = 250000,
            n.burnin = 100000, n.thin = 10)

# Print summary
print(fit, digits=3)

posts <- fit$BUGSoutput$sims.list

# . Save result file ----
# Save results and data used to run it
save(fit, file = "results/juv_posts_poisson_itit.rda")

# Results ----
# . Goodness of fit ----
p_check <- data.frame(
  E = melt(posts$sum_E)$value,
  E_rep = melt(posts$sum_E_rep)$value)

ggplot(p_check, aes(x = E, y = E_rep)) +
  geom_point(alpha = 0.05) +
  geom_abline(intercept = 0, slope = 1) 

# . Abundance ----
# Extract posterior samples for abundance from fitted model
n_mat <- melt(posts$n_total)
names(n_mat) <- c("iteration", "year", "n")
n_mat$year <- unique(juv_haverstraw$YEAR)[n_mat$year]

# Summarize posteriors by year
n_plotter <- n_mat %>%
  group_by(year) %>%
  summarize(
    fit = mean(n),
    lwr = quantile(n, 0.025),
    Q2 = quantile(n, 0.25),
    Q3 = quantile(n, 0.75),
    upr = quantile(n, 0.975))

# Mean observed catch
observed <- juv_haverstraw %>%
  group_by(YEAR) %>%
  summarize(obs = mean(NUMBER, na.rm = TRUE)) %>% 
  mutate(predicted = n_plotter$fit)
names(observed) <- c("year", "mean", "fit")

ggplot(n_plotter, aes(x = year, y = fit)) +
  geom_line() +
  geom_errorbar(aes(xmax = year, ymin = lwr, ymax = upr), width = 0,
                position = position_dodge(width = 0.1)) +
  geom_ribbon(aes(xmax = year, ymin = Q2, ymax = Q3, color = NULL),
              alpha = 0.25) +
  xlab("Year") +
  ylab("Relative abundance") +
  ggtitle("Poisson fit") +
  theme_bw() +
  theme(axis.title.x = element_text(vjust = -1),
        axis.title.y = element_text(vjust = 3),
        text = element_text(size = 12))

