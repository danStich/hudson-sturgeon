# Library loads ----
library(tidyverse)
library(reshape)
library(lubridate)
library(R2jags)

# Data read & manipulation ----
# . Catch data ----
hudson <- read.csv("data/hudson.csv")

# Have a look at first few rows.
head(hudson)

# Get Date into a format that R can recognize.
hudson$Date <- as.Date(as.character(hudson$DATE), format = "%m/%d/%Y")

# Have a look again
head(hudson)

# Any missing dates?
any(is.na(hudson$Date))

# To what years do they correspond?
unique(hudson$YEAR[is.na(hudson$Date)])

# Need info on batches now to see if we can get 2016 data...

# . Site (batch) data ----
# Read in the data file
sites <- read.csv("data/sites.csv")

# We only need a couple columns, so we will filter
# the data set to get just those columns
names(sites)

# Here we select the batch, date, location, rm, and gear columns
# by numerical index in the list of names above
site_sub <- sites[ , c(1, 4, 8, 9, 11)]
site_sub$Date <- as.Date(as.character(site_sub$DATE), format = "%m/%d/%Y")

# . Data merge and clean up ----
# Merge the fish data with the site data
fish <- merge(hudson, site_sub, by = "BATCH", all.y = TRUE)

# Get rid of the duplicated date columns, 
# saving only the dates from the batch data. 
# We'll also boil down the data set to clean up
fish <- fish %>% 
  select( c("BATCH", "YEAR", "NUMBER", "SEX", "STAGE", 
            "LOCATION", "RM", "GEAR.y", "Date.y") )

# Change the names of the last couple columns
names(fish)[7:9] <- c("river_mile", "gear", "date")

# Make the rest of them lowercase so it is easier to type!
names(fish) <- tolower(names(fish))

# Get year, month, day from the batch dates
fish$year <- year(fish$date)
fish$month <- month(fish$date)
fish$day <- yday(fish$date)

# Assign zeroes to nets that did not catch fish
fish$number[is.na(fish$number)] <- 0


# Data exploration ----
# Woo-hoo! We made it!

# How many years?
length(unique(fish$year))

# Which ones?
sort(unique(fish$year))

# How many observations per year?
with(fish, table(year))

# How many nets per year?
# Summarize the data so we can plot it
nets_per_year <- fish %>% 
  group_by(year) %>% 
  summarize(n_nets = length(unique(batch)))

# Make a plot
ggplot(nets_per_year, aes(x = year, y = n_nets)) +
  geom_line()

# How many fish per net?
# Summarize the data so we can plot it
fish_per_net <- fish %>% 
  group_by(batch) %>% 
  summarize(n_fish = sum(number))

# Make a plot. Right-skewed catch data...shocking
ggplot(fish_per_net, aes(n_fish)) +
  geom_histogram()

# What is the minimum number of fish caught per net?
min(fish_per_net$n_fish)

# . * Question about non-zero catch ----
# Are these presence-only data? Are the rest of the rows in the 
# Site data actually samples that yielded zero fish? Looks like
# it based on batch data. I've included those for now.

# More Data exploration ----
# How many fish per day each year?
# Summarize the data so we can plot it
fish_per_day <- fish %>% 
  group_by(year, day) %>% 
  summarize(
    n_fish = sum(number),
    n_nets = length(unique(batch)),
    fish_per_net = sum(number)/length(unique(batch))
    )

# Make a plot of fish per day. Right-skewed catch data...shocking
ggplot(fish_per_day, aes(n_fish)) +
  geom_histogram()

# Plot of nets per day
ggplot(fish_per_day, aes(n_nets)) +
  geom_histogram()

# Plot of fish per net - looks pretty much the same as fish
# per day, so could use day or net as unit of replication?
# Note that this earlier years had most of the zero-catches
# but also most of the higher catches
ggplot(fish_per_day, aes(fish_per_net)) +
  geom_histogram() +
  facet_wrap(~year)

# What is the minimum number of fish caught per net?
# Okay, so same question as above. Have any zero catch
min(fish_per_day$fish_per_net)

# Plot catch against day/month to see if we have patterns there
# that we need to think about

# Looks like detection and/or occupancy change throughout the year
ggplot(fish_per_day, aes(x = day, y = n_fish)) +
  geom_point(alpha = 0.2)

# Looks like effort is relatively consistent throughout the season
# but varies between years. Pretty constant after 2008
ggplot(fish_per_day, aes(x = day, y = n_nets)) +
  geom_point(alpha = 0.2) +
  facet_wrap(~year)

# . Spatial patterns ----
# Work with data 2009-2020 to avoid apparent change in
# sampling methods. Ignores effort info for now.
fish <- filter(fish, year >= 2009)

# How many sampling locations?
locations <- unique(fish$location)
locations

# That's a lot of locations, plus looks like many are
# same but labeled different. Can we reduce to river mile?
# Not as many.
unique(fish$river_mile)

table(fish$river_mile, fish$day)

rowSums(table(fish$river_mile, fish$day))


# Now can we just boil it down to places where fish were
# caught?
fish <- fish %>%
  filter(river_mile %in% c(80:84))

# Detection model testing ----
# Get a 3-d array of fish number by site/day and year
caps <- fish %>% 
  cast(river_mile~day~year, value = "number", fun.aggregate = "sum")

# ----
# DIC = 6005 p ----
# DIC = 5819 p[i] (site) ----
# DIC = 5705 p[t] (day) ----
# DIC = 5776 p[j] (year) ----
# DIC = 3913 p[t, j] ----
# DIC = 4817 p[i, j] ----
# DIC = 2068 p[i, t, j] ----
# ----
# Abundance model testing ----
# ----
# DIC = 2068 N[i] (site) ----
# DIC = 2097 N[j] (year) ----
# DIC = 2095 N[i, j] (site and year) ----
# ----
# Best model (p[i, t, j] and N[i]) ----
# Get a 3-d array of fish number by site/day and year
caps <- fish %>% 
  cast(river_mile~day~year, value = "number", fun.aggregate = "sum")

# . Model code ----
modelstring <- "
  model{

    # Count likelihood
    for(i in 1:n_sites){
      for(t in 1:n_days){
        for(j in 1:n_years){
          y[i, t, j] ~ dbin(p[i, t, j], N[i, j])
        }
      }
    }
  
    # Priors
    # Lambda and abundance within sites across years
    for(i in 1:n_sites){
      for(j in 1:n_years){
        lambda[i, j] ~ dunif(0, 100000)  
        N[i, j] ~ dpois(lambda[i, j])
      }
    }

    # Detection probability
    for(i in 1:n_sites){
      for(t in 1:n_days){
        for(j in 1:n_years){
          # logit(p[i, t, j]) <- lp[i, t, j]
          # lp[i, t, j] ~ dnorm(p_mu, 1)
          p[i, t, j] <- p_mu
        }
      }
    }
    
    p_mu ~ dunif(0, .05)
    
    # Derived abundance estimate
    N_total <- sum(N)
  
  }"


# . Run model ----
# Format data
jags.data <- list(n_sites = length(unique(fish$river_mile)),
                  n_days = length(unique(fish$day)),
                  n_years = length(unique(fish$year)),
                  y = caps
                  )

# Parameters monitored
params <- c("N", "N_total", "p_mu", "p")

# Compile the model
jags.out <- jags(jags.data, inits = NULL,
                 parameters.to.save = params,
                 model.file=textConnection(modelstring),
                 n.chains=3, n.iter=5000,
                 n.burnin=2500, n.thin=5)

# Print summary
print(jags.out, digits=3)



# . Result summary ----
# .. Abundance ----
n_ests <- melt(jags.out$BUGSoutput$sims.list$N)
names(n_ests) <- c("iter", "site", "year", "N")

year_s <- n_ests %>% 
  group_by(iter, year) %>% 
  summarize(total = sum(N))

# ... Sum across sites ----
result <- year_s %>% 
  group_by(year) %>% 
  summarize(fit = mean(total),
            lwr = quantile(total, 0.025),
            upr = quantile(total, 0.975)
            ) %>% 
  mutate(year = year + 2008)

ggplot(result, aes(x = year, y =fit)) +
  geom_point() +
  geom_line() +
  geom_segment(aes(xend = year, y = lwr, yend = upr), lty = 2) +
  scale_x_continuous(limits = c(2004, 2020))

# ... Average of sites ---
result <- n_ests %>% 
  group_by(year) %>% 
  summarize(fit = mean(N),
            lwr = quantile(N, 0.025),
            upr = quantile(N, 0.975)
            ) %>% 
  mutate(year = year + 2008)

ggplot(result, aes(x = year, y =fit)) +
  geom_point() +
  geom_line() +
  geom_segment(aes(xend = year, y = lwr, yend = upr), lty = 2) +
  scale_x_continuous(limits = c(2004, 2020))

# .. Detection ----
p_ests <- melt(jags.out$BUGSoutput$sims.list$p)
names(p_ests) <- c("iter", "site", "day", "year", "p")

result <- p_ests %>% 
  group_by(year, site) %>% 
  summarize(fit = mean(p),
            lwr = quantile(p, 0.025),
            upr = quantile(p, 0.975)
            ) %>% 
  mutate(year = year + 2008)

ggplot(result, aes(x = year, y =fit
                   )) +
  geom_point() +
  geom_line() +
  geom_ribbon(aes(xmax = year, ymin = lwr, ymax = upr), alpha = 0.15) +
  facet_wrap(~ site)
