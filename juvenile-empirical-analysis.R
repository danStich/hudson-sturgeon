# Libraries ----
library(tidyverse)
<<<<<<< HEAD
=======
library(readxl)
>>>>>>> b0137a1f65d9f3536ef4e4b6062042d788200ae0
library(lubridate)
library(reshape)
library(R2jags)

<<<<<<< HEAD
# Data ----
# . Data read ----
# Read data file to reproduce the analysis
juv_haverstraw <- read.csv("data/juv_haverstraw.csv")
=======
# Data read ----
# Maximum number of rows in an Excel file
n_max <- 1048576

# Data exports from the state-wide fishery database
sturg_len <- read_xlsx("data/STURG_LENGTHS.xlsx", guess_max = n_max)
sturg_a <- read_xlsx("data/STURG_A.xlsx", guess_max = n_max)
sturg_b <- read_xlsx("data/STURG_B.xlsx", guess_max = n_max)
sturg_n <- read_xlsx("data/STURG_N.xlsx", guess_max = n_max)


# . Juvenile surveys ----
# Filter swfdb data to get juvenile surveys (program 11) and
# only Atlantic sturgeon (SPEC = 262)
juv_a <- filter(sturg_a, PROG == 11)
juv_b <- filter(sturg_b, PROG == 11 & SPEC == 262)
juv_n <- filter(sturg_n, PROG == 11)

# . Data manipulation ----
river_miles <- juv_a[, c("BATCH", "RM", "SHORE", "SET_SAMP")]
net_sets <- juv_n[, c("BATCH", "DATE", "YEAR", "HABITAT", "LAT_MIDPT", "LONG_MIDPT")]
fish <- juv_b[, c("BATCH", "NUMBER", "SEX")]

# Merge the site, net, and fish data sets
rm_nets <- merge(river_miles, net_sets, by = "BATCH")
# Change erroneous habitat values
rm_nets$HABITAT <- gsub("sd", "SD", rm_nets$HABITAT)
juv_c <- merge(rm_nets, fish, by = "BATCH", all.x = TRUE)

# .. Juvenile net sets for maps ----
# Save to a file
juvenile_maps_SD <- juv_c %>% 
  filter(RM %in% c(35:39) & HABITAT == "SD")

write.table(juvenile_maps_SD, "data/juvenile_maps.csv",
            sep = ",", quote = FALSE, row.names = FALSE)

# Remove the LAT/LON MID POINT columns for reproducible data set
# that doesn't include exact locations for ESA status
juv_c <- juvenile_maps_SD[ , -c(8:9)]

# .. Identify unique replicates ----
# Check that there are observations in each of the years
with(juv_c, table(YEAR, NUMBER))
with(juv_c, table(YEAR, RM))

# Add columns for day of year and name of day
juv_c$DOY <- yday(juv_c$DATE)
juv_c$DAY <- wday(juv_c$DATE, label = TRUE)

# Create higher-res sites for expanded design
juv_c$SITE <- paste0(juv_c$RM, "-", juv_c$SHORE)
with(juv_c, table(SITE, YEAR))

# Create higher-res reps for expanded design
juv_c$DOY2 <- paste0(juv_c$DOY, "-", juv_c$SET_SAMP)
with(juv_c, table(DOY2, YEAR))

# Data for consistently sampled area
juv_haverstraw <- juv_c %>% 
  filter(RM %in% c(35:39) & HABITAT == "SD")%>% 
  group_by(SITE, YEAR) %>% 
  mutate(REP = row_number()) %>%
  ungroup()

# Add zeroes to NA nets before completing RM-YEAR-REP combos
juv_haverstraw$NUMBER[is.na(juv_haverstraw$NUMBER)] <- 0

juv_haverstraw <- juv_haverstraw %>% 
  left_join(expand.grid(SITE = unique(juv_haverstraw$SITE), 
                        YEAR = unique(juv_haverstraw$YEAR),
                        REP = unique(juv_haverstraw$REP)), .) %>% 
  fill(SITE) %>% 
  fill(YEAR) %>% 
  fill(REP)

# Write out a data file that can be used to reproduce the analysis
write.table(juv_haverstraw, "data/juv_haverstraw.csv",
            sep = ",", quote = FALSE, row.names = FALSE)
>>>>>>> b0137a1f65d9f3536ef4e4b6062042d788200ae0


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
<<<<<<< HEAD
save(fit, file = "results/juv_posts.rda")
=======
# save(fit, file = "results/juv_posts.rda")
# save(juv_haverstraw, file = "results/juv_haverstraw.rda")


# Results ----
# . Load result file ----
# load("results/juv_posts.rda")
# load("results/juv_haverstraw.rda")


# . Extract posteriors ----
posts <- fit$BUGSoutput$sims.list

# . Model validation
# . Posterior predicted fit (Figure S1) ----
# Get sums of observed and simulated values from posteriors
p_check <- data.frame(
  y = melt(posts$sum_y)$value,
  y_rep = melt(posts$sum_y_rep)$value)

# # Plot predicted vs fitted with a 1:1 line
# jpeg(filename = "results/FigureS1.jpg",
#      res = 300,
#      height = 1600, width = 1600)
# ggplot(p_check, aes(x = y, y = y_rep)) +
#   geom_point(alpha = 0.05) +
#   geom_abline(intercept = 0, slope = 1) +
#   ylab(expression(paste(italic("T(y"^"rep"), italic(")")))) +
#   xlab(expression(paste(italic("T(y)")))) +
#   theme_bw() +
#   theme(axis.title.x = element_text(vjust = -1),
#         axis.title.y = element_text(vjust = 3))
# dev.off()

# Calculate Bayesian p-value
mean(p_check$y_rep > p_check$y)

# . Detection (Figure 2) ----
# Get posteriors for p
p <- melt(posts$p)
names(p) <- c("iteration", "site", "year", "value")

# Assign text labels from original data to column labels in posts
p$site <- sort(unique(juv_haverstraw$SITE))[p$site]
p$year <- sort(unique(juv_haverstraw$YEAR))[p$year]

# Remove dashes and shore designations with cardinal directions
p$site <- gsub(pattern = "-3", replacement = "-W", p$site)
p$site <- gsub(pattern = "-1", replacement = "-E", p$site)

# Replace river mile with river kilometer for paper
p$site <- gsub(pattern = "35-", replacement = "56-", p$site)
p$site <- gsub(pattern = "36-", replacement = "58-", p$site)
p$site <- gsub(pattern = "37-", replacement = "60-", p$site)
p$site <- gsub(pattern = "38-", replacement = "61-", p$site)
p$site <- gsub(pattern = "39-", replacement = "63-", p$site)

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


# jpeg(filename = "results/Figure2.jpg",
#      res = 300,
#      height = 1800, width = 2400)
  ggplot(p_plotter, aes(x = year, y = fit, color = site)) +
    geom_point(position = position_dodge2(width = .5), size = 2) +
    geom_linerange(aes(xmax = year, ymin = lwr, ymax = upr),
                   position = position_dodge2(width = .5)) +
    geom_linerange(aes(xmax = year, ymin = lwr, ymax = upr)) +
    labs(color = "Site") +
    xlab("Year") +
    ylab(expression(paste(
      "Individual detection probability (", italic(p["i, t"]), ")"))) +
    theme_bw() +
    theme(axis.title.x = element_text(vjust = -1),
          axis.title.y = element_text(vjust = 3),
          text = element_text(size = 14)) +
    scale_color_grey()
# dev.off()

# . Relative abundance (Figure 3) ----
# Extract posterior samples for abundance from fitted model
n_mat <- melt(posts$N)
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
    upr = quantile(n, 0.975)) %>% 
  arrange(fit)

mean(n_plotter$fit[n_plotter$year >= 2019])
mean(n_plotter$lwr[n_plotter$year >= 2019])
mean(n_plotter$upr[n_plotter$year >= 2019])


# Mean observed catch
observed <- juv_haverstraw %>%
  group_by(YEAR) %>%
  summarize(obs = mean(NUMBER, na.rm = TRUE)) %>% 
  mutate(predicted = n_plotter$fit)
names(observed) <- c("year", "mean", "fit")

# Plot
# jpeg(filename = "results/Figure3.jpg",
#      res = 300,
#      height = 1800, width = 2400)
  ggplot(n_plotter, aes(x = year, y = fit)) +
    geom_line() +
    geom_errorbar(aes(xmax = year, ymin = lwr, ymax = upr), width = 0,
                  position = position_dodge(width = 0.1)) +
    geom_ribbon(aes(xmax = year, ymin = Q2, ymax = Q3, color = NULL),
                alpha = 0.25) +
    # geom_line(data = observed, aes(y = mean), lty = 2) +
    xlab("Year") +
    ylab("Relative abundance") +
    theme_bw() +
    theme(axis.title.x = element_text(vjust = -1),
          axis.title.y = element_text(vjust = 3),
          text = element_text(size = 14))  
# dev.off()

# . Population growth rate ----
# Extract posterior samples for abundance from fitted model
r_mat <- melt(posts$r)
names(r_mat) <- c("iteration", "year", "r")
r_mat$year <- unique(juv_haverstraw$YEAR)[r_mat$year]

# ggplot(r_mat, aes(x = year, y = r, group = year))+
#   geom_hline(yintercept = 0, linetype = 2) +
#   geom_violin(draw_quantiles = c(0.25, 0.75)) +
#   stat_summary(geom = "point", fun = "mean", size = 4) +
#   xlab("Year") +
#   ylab(expression(paste(italic(r)["t"])))
>>>>>>> b0137a1f65d9f3536ef4e4b6062042d788200ae0
