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
sturg_a <- read_xlsx("data/adult-data-update-2025.xlsx", sheet = "a", guess_max = n_max)
sturg_b <- read_xlsx("data/adult-data-update-2025.xlsx", sheet = "b", guess_max = n_max)
sturg_n <- read_xlsx("data/adult-data-update-2025.xlsx", sheet = "n", guess_max = n_max)


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
# Don't need this for the data update
# adult_2010_empty_nets <- read.csv("data/adult_2010_empty_nets.csv")
# adult_2010_empty_nets$DATE <- as.Date(adult_2010_empty_nets$DATE, format = "%m/%d/%Y")
# 
# # Combine adult data with empty nets from 2010
# adult_c <- rbind(adult_c, adult_2010_empty_nets)

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

# Write out a data file that can be used to reproduce the analysis
write.table(adult_norrie, "data/adult_norrie_update_2425.csv",
            sep = ",", quote = FALSE, row.names = FALSE)

