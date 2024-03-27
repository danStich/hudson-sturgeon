# Libraries ----
library(tidyverse)
library(readxl)
library(lubridate)
library(reshape)
library(R2jags)

# Data read ----
# Maximum number of rows in an Excel file
n_max <- 1048576

# Data exports from the state-wide fishery database
sturg_len <- read_xlsx("data/STURG_LENGTHS.xlsx", guess_max = n_max)
sturg_a <- read_xlsx("data/STURG_A.xlsx", guess_max = n_max)
sturg_b <- read_xlsx("data/STURG_B.xlsx", guess_max = n_max)
sturg_n <- read_xlsx("data/STURG_N.xlsx", guess_max = n_max)


# . Adult surveys ----
# Filter swfdb data to get Adult surveys (program 12) and
# only Atlantic sturgeon (SPEC = 262)
adult_a <- filter(sturg_a, PROG == 12)
adult_b <- filter(sturg_b, PROG == 12 & SPEC == 262)
adult_n <- filter(sturg_n, PROG == 12)

# Data manipulation ----
river_miles <- adult_a[, c("BATCH", "RM", "SHORE", "SET_SAMP", "Habitat")]
net_sets <- adult_n[, c("BATCH", "DATE", "YEAR", "LAT_MIDPT", "LONG_MIDPT")]
fish <- adult_b[, c("BATCH", "NUMBER", "SEX")]

rm_nets <- merge(river_miles, net_sets, by = "BATCH")
adult_c <- merge(rm_nets, fish, by = "BATCH", all.x = TRUE)

# Add 2010 data for empty nets that were missing from SWFDB
# A Higgs sent in seperate file 8/1/2023
adult_2010_empty_nets <- read.csv("data/adult_2010_empty_nets.csv")
adult_2010_empty_nets$DATE <- as.Date(adult_2010_empty_nets$DATE, format = "%m/%d/%Y")

# Combine adult data with empty nets from 2010
adult_c <- rbind(adult_c, adult_2010_empty_nets)

# Add columns for day of year and name of day
adult_c$DOY <- yday(adult_c$DATE)
adult_c$DAY <- wday(adult_c$DATE, label = TRUE)

# TEST SITE IDS
adult_c$SITE <- adult_c$RM #paste(adult_c$RM, adult_c$SHORE)

# Just get data for Hyde Park reaches during adult sampling 
# dates.
adult_norrie <- adult_c %>% 
  filter(RM %in% c(80, 81, 83), DOY >= 150 & DOY <= 180) %>% 
  group_by(SITE, YEAR) %>% 
  mutate(REP = row_number()) %>%
  ungroup()

# Add zeroes to NA nets before completing RM-YEAR-REP combos
adult_norrie$NUMBER[is.na(adult_norrie$NUMBER)] <- 0

adult_norrie <- adult_norrie %>% 
  left_join(expand.grid(SITE = unique(adult_norrie$SITE), 
                        YEAR = unique(adult_norrie$YEAR),
                        REP = unique(adult_norrie$REP)), .) %>% 
  fill(SITE) %>% 
  fill(YEAR) %>% 
  fill(REP)

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
# fit <- jags(jags.data, inits = inits,
#                               parameters.to.save = params,
#                               model.file = "models/n-mixture-jags",
#                               n.chains = 3, n.iter = 500000,
#                               n.burnin = 150000, n.thin = 5)

# # Print summary
# print(fit, digits=3)

# . Save result file ----
# Save results and data used to run it
# save(fit, file = "results/adult_posts.rda")
# save(adult_norrie, file = "results/adult_norrie.rda")

# Results ----
# . Load result file ----
load("results/adult_posts.rda")
load("results/adult_norrie.rda")


# . Extract posteriors ----
posts <- fit$BUGSoutput$sims.list

# . Posterior predicted fit (Figure S2) ----
# Get fitted and predicted values from posteriors
p_check <- data.frame(
  y = melt(posts$sum_y)$value,
  y_rep = melt(posts$sum_y_rep)$value)

# Plot predicted vs fitted with a 1:1 line
jpeg(filename = "results/FigureS2.jpg",
     res = 300,
     height = 1600, width = 1600)
ggplot(p_check, aes(x = y, y = y_rep)) +
  geom_point(alpha = 0.05) +
  geom_abline(intercept = 0, slope = 1) +
  ylab(expression(paste(italic("T(y"^"rep"), italic(")")))) +
  xlab(expression(paste(italic("T(y)")))) +
  # scale_x_continuous(limits = c(3000, 6000)) +
  # scale_y_continuous(limits = c(3000, 6000)) +  
  theme_bw() +
  theme(axis.title.x = element_text(vjust = -1),
        axis.title.y = element_text(vjust = 3))
dev.off()

# Calculate Bayesian p-value
mean(posts$bayesian_p_mean)

# . Detection (Figure 4) ----
# Get posteriors for p
p <- melt(posts$p)
names(p) <- c("iteration", "site", "year", "value")

# Assign text labels from original data to column labels in posts
p$site <- sort(unique(adult_norrie$SITE))[p$site]
p$year <- sort(unique(adult_norrie$YEAR))[p$year]

# Replace river mile with river kilometer for paper
p$site <- gsub(pattern = "80", replacement = "129", p$site)
p$site <- gsub(pattern = "81", replacement = "130", p$site)
p$site <- gsub(pattern = "82", replacement = "132", p$site)
p$site <- gsub(pattern = "83", replacement = "134", p$site)

p_plotter <- p %>%
  group_by(site, year) %>%
  summarize(fit = mean(value),
            lwr = quantile(value, 0.025),
            upr = quantile(value, 0.975))

# Descriptive statistics for results section
# Interannual variability
p %>% 
  group_by(year) %>% 
  summarize(fit = mean(value),
            lwr = quantile(value, 0.025),
            upr = quantile(value, 0.975)) %>% 
  arrange(fit)

# Variability among sites
p %>% 
  group_by(site) %>% 
  summarize(fit = mean(value),
            lwr = quantile(value, 0.025),
            upr = quantile(value, 0.975)) %>% 
  arrange(fit)

jpeg(filename = "results/Figure4.jpg",
     res = 300,
     height = 1800, width = 2400)
ggplot(p_plotter, aes(x = year, y = fit, color = site)) +
  geom_point(position = position_dodge2(width = .5), size = 2) +
  geom_linerange(aes(xmax = year, ymin = lwr, ymax = upr),
                 position = position_dodge2(width = .5)) +
  labs(color = "Site") +
  xlab("Year") +
  ylab(expression(paste(
    "Individual detection probability (", italic(p["i, t"]), ")"))) +
  theme_bw() +
  theme(axis.title.x = element_text(vjust = -1),
        axis.title.y = element_text(vjust = 3),
        text = element_text(size = 14)) +
  scale_color_grey()
dev.off()

# . Relative abundance (Figure 5) ----
# Extract posterior samples for abundance from fitted model
n_mat <- melt(posts$N)
names(n_mat) <- c("iteration", "year", "n")
n_mat$year <- unique(adult_norrie$YEAR)[n_mat$year]

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
observed <- adult_norrie %>%
  group_by(YEAR) %>%
  summarize(obs = mean(NUMBER, na.rm = TRUE)) %>% 
  mutate(predicted = n_plotter$fit)
names(observed) <- c("year", "mean", "fit")

# Kazyak's estimates
kazyak <- data.frame(
  year = 2013.9,
  fit = 466,
  lwr = 310,
  upr = 745
)

# Plot
jpeg(filename = "results/Figure5.jpg",
     res = 300,
     height = 1800, width = 2400)
ggplot(n_plotter, aes(x = year, y = fit)) +
  geom_line() +
  geom_errorbar(aes(xmax = year, ymin = lwr, ymax = upr), width = 0,
                position = position_dodge(width = 0.1)) +
  geom_ribbon(aes(xmax = year, ymin = Q2, ymax = Q3, color = NULL),
              alpha = 0.25) +
  xlab("Year") +
  ylab("Relative abundance") +
  theme_bw() +
  theme(axis.title.x = element_text(vjust = -1),
        axis.title.y = element_text(vjust = 3),
        text = element_text(size = 14)) +
  geom_point(data = kazyak, aes(x = year, y = fit), size = 2) +
  geom_linerange(data = kazyak, aes(xmax = year, ymin = lwr, ymax = upr))
dev.off()


# . Population growth rate ----
# Extract posterior samples for abundance from fitted model
r_mat <- melt(posts$r)
names(r_mat) <- c("iteration", "year", "r")
r_mat$year <- unique(adult_norrie$YEAR)[r_mat$year]

ggplot(r_mat, aes(x = year, y = r, group = year))+
  geom_hline(yintercept = 0, linetype = 2) +
  geom_violin(draw_quantiles = c(0.25, 0.75)) +
  stat_summary(geom = "point", fun = "mean", size = 4) +
  xlab("Year") +
  ylab(expression(paste(italic(r)["t"])))
  