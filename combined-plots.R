# Libraries ----
library(tidyverse)
<<<<<<< HEAD
=======
library(readxl)
>>>>>>> b0137a1f65d9f3536ef4e4b6062042d788200ae0
library(lubridate)
library(reshape)
library(gridExtra)

# Juveniles ----
# . Data read ----
<<<<<<< HEAD
juv_haverstraw <- read.csv("data/juv_haverstraw.csv")

=======
# Maximum number of rows in an Excel file
n_max <- 1048576

# Data exports from the state-wide fishery database
sturg_len <- read_xlsx("data/STURG_LENGTHS.xlsx", guess_max = n_max)
sturg_a <- read_xlsx("data/STURG_A.xlsx", guess_max = n_max)
sturg_b <- read_xlsx("data/STURG_B.xlsx", guess_max = n_max)
sturg_n <- read_xlsx("data/STURG_N.xlsx", guess_max = n_max)


# .. Juvenile surveys ----
# Filter swfdb data to get juvenile surveys (program 11) and
# only Atlantic sturgeon (SPEC = 262)
juv_a <- filter(sturg_a, PROG == 11)
juv_b <- filter(sturg_b, PROG == 11 & SPEC == 262)
juv_n <- filter(sturg_n, PROG == 11)

# . Data manipulation ----
river_miles <- juv_a[, c("BATCH", "RM", "SHORE", "SET_SAMP")]
net_sets <- juv_n[, c("BATCH", "DATE", "YEAR", "HABITAT", "LAT_MIDPT", "LONG_MIDPT")]
fish <- juv_b[, c("BATCH", "NUMBER", "SEX")]

rm_nets <- merge(river_miles, net_sets, by = "BATCH")
juv_c <- merge(rm_nets, fish, by = "BATCH", all.x = TRUE)

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

juv_c %>% 
  group_by(YEAR) %>% 
  summarize(
    mins = min(DATE),
    maxes = max(DATE)
  )

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
>>>>>>> b0137a1f65d9f3536ef4e4b6062042d788200ae0

# . Results ----
# .. Load result file ----
load("results/juv_posts.rda")

<<<<<<< HEAD

# .. Extract posteriors ----
jposts <- fit$BUGSoutput$sims.list


# Adults ----
# . Data read ----
adult_norrie <- read.csv("data/adult_norrie.csv")


=======
# .. Extract posteriors ----
jposts <- fit$BUGSoutput$sims.list

# . Model validation

# Adults ----


# . Data read ----
# Add 2010 data for empty nets that were missing from SWFDB
# A Higgs sent in seperate file 8/1/2023
adult_2010_empty_nets <- read.csv("data/adult_2010_empty_nets.csv")
adult_2010_empty_nets$DATE <- as.Date(adult_2010_empty_nets$DATE, format = "%m/%d/%Y")

# .. Adult surveys ----
# Filter swfdb data to get Adult surveys (program 12) and
# only Atlantic sturgeon (SPEC = 262)
adult_a <- filter(sturg_a, PROG == 12)
adult_b <- filter(sturg_b, PROG == 12 & SPEC == 262)
adult_n <- filter(sturg_n, PROG == 12)

# . Data manipulation ----
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

>>>>>>> b0137a1f65d9f3536ef4e4b6062042d788200ae0
# . Results ----
# .. Load result file ----
load("results/adult_posts.rda")


# .. Extract posteriors ----
posts <- fit$BUGSoutput$sims.list


<<<<<<< HEAD
=======

>>>>>>> b0137a1f65d9f3536ef4e4b6062042d788200ae0
# Figures for manuscript ----
# . Figure S1 (posterior checks) ----
# .. Juveniles ----
# Get sums of observed and simulated values from posteriors
jp_check <- data.frame(
  y = melt(jposts$sum_y)$value,
  y_rep = melt(jposts$sum_y_rep)$value)

# .. Adults ----
# Get fitted and predicted values from posteriors
p_check <- data.frame(
  y = melt(posts$sum_y)$value,
  y_rep = melt(posts$sum_y_rep)$value)

# .. Plot ----
# predicted vs fitted with a 1:1 line
jpeg(filename = "results/FigureS1.jpg",
     res = 300,
     height = 1600, width = 2400)

<<<<<<< HEAD
jp_check$Survey <- "Juvenile"
p_check$Survey <- "Adult"
=======
jp_check$Survey = "Juvenile"
p_check$Survey = "Adult"
>>>>>>> b0137a1f65d9f3536ef4e4b6062042d788200ae0

p_check_plotter <- rbind(jp_check, p_check)
p_check_plotter$Survey = factor(p_check_plotter$Survey,
                                levels = c("Juvenile", "Adult"))

dat_text <- data.frame(
  label = c("(a)", "(b)"),
  Survey  = factor(c("Juvenile", "Adult")),
  x = c(29000, 5000),
  y = c(34000, 25000))

ggplot(p_check_plotter, aes(x = y, y = y_rep)) +
  geom_point(alpha = 0.05) +
  geom_abline(intercept = 0, slope = 1) +
  ylab("") +
  ylab(expression(paste(italic("T(y"^"rep"), italic(")")))) +
  xlab(expression(paste(italic("T(y)")))) +
  facet_wrap(~ Survey, scales = "free") +
  theme_bw() +
  theme(axis.title.x = element_text(vjust = -1),
        axis.title.y = element_text(vjust = 3),
        strip.background = element_blank())+
  geom_text(data = dat_text, mapping = aes(x = x, y = y, label = label))
  
dev.off()

# . Figure 2 (Detection) ----
# .. Juveniles ----
# Get posteriors for p
jpp <- melt(jposts$p)
names(jpp) <- c("iteration", "site", "year", "value")

# Assign text labels from original data to column labels in posts
jpp$site <- sort(unique(juv_haverstraw$SITE))[jpp$site]
jpp$year <- sort(unique(juv_haverstraw$YEAR))[jpp$year]

# Remove dashes and shore designations with cardinal directions
jpp$site <- gsub(pattern = "-3", replacement = "-W", jpp$site)
jpp$site <- gsub(pattern = "-1", replacement = "-E", jpp$site)

# Replace river mile with river kilometer for paper
jpp$site <- gsub(pattern = "35-", replacement = "56-", jpp$site)
jpp$site <- gsub(pattern = "36-", replacement = "58-", jpp$site)
jpp$site <- gsub(pattern = "37-", replacement = "60-", jpp$site)
jpp$site <- gsub(pattern = "38-", replacement = "61-", jpp$site)
jpp$site <- gsub(pattern = "39-", replacement = "63-", jpp$site)

jp <- jpp %>%
  group_by(site, year) %>%
  summarize(fit = mean(value),
            lwr = quantile(value, 0.025),
            upr = quantile(value, 0.975))

figure2a <- ggplot(jp, aes(x = year, y = fit, color = site)) +
  geom_point(position = position_dodge2(width = .5)) +
  geom_linerange(aes(xmax = year, ymin = lwr, ymax = upr),
                 position = position_dodge2(width = .5)) +
  geom_linerange(aes(xmax = year, ymin = lwr, ymax = upr)) +
  scale_color_grey() +
  labs(color = "Site") +
  scale_x_continuous(limits = c(2003, 2022.5)) +
  scale_y_continuous(limits = c(0, 0.035)) +
  annotate("text", x = 2003.75, y = .0325, label = "(a)") +  
  xlab("") +
  ylab(expression(paste(
    "Individual detection probability (", italic(p["i, t"]), ")"))) +
  theme_bw() +
  theme(axis.title.x = element_text(vjust = 3),
        axis.title.y = element_text(vjust = 2),
        text = element_text(size = 12),
        legend.background = element_blank())  


# ... Descriptive statistics ----
# Overall mean and 95% CRI
mean(jposts$p)
quantile(jposts$p, c(0.025, 0.975))

# Means and CRIs by year
jpp %>%
  group_by(year) %>%
  summarize(fit = mean(value),
            lwr = quantile(value, 0.025),
            upr = quantile(value, 0.975))

# Means and CRIs by year
jpp %>%
  group_by(site) %>%
  summarize(fit = mean(value),
            lwr = quantile(value, 0.025),
            upr = quantile(value, 0.975))


# .. Adults ----
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

<<<<<<< HEAD
p$site <- paste(p$site, "  ")
=======
p$site <- paste(p$site, "  ") # check out padding from stringr
>>>>>>> b0137a1f65d9f3536ef4e4b6062042d788200ae0

p <- p %>%
  group_by(site, year) %>%
  summarize(fit = mean(value),
            lwr = quantile(value, 0.025),
            upr = quantile(value, 0.975))

figure2b <- ggplot(p, aes(x = year, y = fit, color = site)) +
  geom_point(position = position_dodge2(width = .5)) +
  geom_linerange(aes(xmax = year, ymin = lwr, ymax = upr),
                 position = position_dodge2(width = .5)) +
  scale_color_grey() +
  labs(color = "Site") +
  scale_x_continuous(limits = c(2003, 2022.5)) +
  annotate("text", x = 2003.75, y = .081, label = "(b)") +    
  xlab("Year") +
  ylab(expression(paste(
    "Individual detection probability (", italic(p["i, t"]), ")"))) +
  theme_bw() +
  theme(axis.title.x = element_text(vjust = -1),
        axis.title.y = element_text(vjust = 2),
        text = element_text(size = 12),
        legend.background = element_blank())  


# .. Plot ----
jpeg(filename = "results/Figure2.jpg",
     res = 300,
     height = 2400, width = 2400)

gridExtra::grid.arrange(figure2a, figure2b, nrow = 2)

dev.off()


# . Figure 3 ----
# .. Juveniles ----
# Extract posterior samples for abundance from fitted model
jn_mat <- melt(jposts$N)
names(jn_mat) <- c("iteration", "year", "n")
jn_mat$year <- unique(juv_haverstraw$YEAR)[jn_mat$year]

# Summarize posteriors by year
jn_plotter <- jn_mat %>%
  group_by(year) %>%
  summarize(
    fit = mean(n),
    lwr = quantile(n, 0.025),
    Q2 = quantile(n, 0.25),
    Q3 = quantile(n, 0.75),
    upr = quantile(n, 0.975))

# Mean observed catch
jobserved <- juv_haverstraw %>%
  group_by(YEAR) %>%
  summarize(obs = mean(NUMBER, na.rm = TRUE)) %>% 
  mutate(predicted = jn_plotter$fit)
names(jobserved) <- c("year", "mean", "fit")

# Plot
figure3a <- ggplot(jn_plotter, aes(x = year, y = fit)) +
  geom_line() +
  geom_errorbar(aes(xmax = year, ymin = lwr, ymax = upr), width = 0,
                position = position_dodge(width = 0.1)) +
  geom_ribbon(aes(xmax = year, ymin = Q2, ymax = Q3, color = NULL),
              alpha = 0.25) +
  scale_x_continuous(limits = c(2004, 2023)) +
  scale_y_continuous(limits = c(0, 2500)) +
  annotate("text", x = 2004, y = 2400, label = "(a)") +
  xlab("") +
  ylab("Relative abundance") +
  theme_bw() +
  theme(axis.title.x = element_text(vjust = 3),
        axis.title.y = element_text(vjust = 3),
        text = element_text(size = 12))  

# Descriptive statistics
# Overall mean and 95% CRI
mean(jposts$N)
quantile(jposts$N, c(0.025, 0.975))

# Means and CRIs by year
jn_mat %>%
  group_by(year) %>%
  summarize(fit = mean(n),
            lwr = quantile(n, 0.025),
            upr = quantile(n, 0.975))

mean(jn_plotter$fit[jn_plotter$year >= 2019])
quantile(jn_plotter$fit[jn_plotter$year >= 2019], c(0.025, 0.975))


# .. Adults ----
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

# Plot
figure3b <- ggplot(n_plotter, aes(x = year, y = fit)) +
  geom_line() +
  geom_errorbar(aes(xmax = year, ymin = lwr, ymax = upr), width = 0,
                position = position_dodge(width = 0.1)) +
  geom_ribbon(aes(xmax = year, ymin = Q2, ymax = Q3, color = NULL),
              alpha = 0.25) +
  scale_x_continuous(limits = c(2004, 2023)) +
  scale_y_continuous(limits = c(0, 3500)) +
  annotate("text", x = 2004, y = 3250, label = "(b)") +
<<<<<<< HEAD
  geom_point(aes(x = 2013.75, y = 466)) +
  geom_linerange(aes(x = 2013.75, xmax = 2013.75, ymin = 310, ymax = 745)) +
=======
>>>>>>> b0137a1f65d9f3536ef4e4b6062042d788200ae0
  xlab("Year") +
  ylab("Relative abundance") +
  theme_bw() +
  theme(axis.title.x = element_text(vjust = -1),
        axis.title.y = element_text(vjust = 3),
        text = element_text(size = 12))  


# .. Plot ----
jpeg(filename = "results/Figure3.jpg",
      res = 300,
      height = 2400, width = 2400)

gridExtra::grid.arrange(figure3a, figure3b, nrow = 2)

dev.off()
<<<<<<< HEAD

=======
>>>>>>> b0137a1f65d9f3536ef4e4b6062042d788200ae0
# Population growth rate ----
# . Juveniles ----
j_growth <- jn_plotter %>% mutate(
  fit = log(fit/lag(fit)),
  lwr = log(lwr/lag(lwr)),
  upr = log(upr/lag(upr))
)

mean(j_growth$fit, na.rm = TRUE)
quantile(j_growth$fit, c(0.025, 0.975), na.rm = TRUE)

<<<<<<< HEAD
# . Adults ----
growth <- n_plotter %>% mutate(
  fit = log(fit/lag(fit)),
  lwr = log(lwr/lag(lwr)),
  upr = log(upr/lag(upr))
)

mean(growth$fit, na.rm = TRUE)
quantile(growth$fit, c(0.025, 0.975), na.rm = TRUE)

quantile(growth$fit, c(.16), na.rm = TRUE)

=======
>>>>>>> b0137a1f65d9f3536ef4e4b6062042d788200ae0
