source("./R/SCRIPTS/000-Libraries.R")

# TEST YEAR IS SET TO 2020
test_year = 2015

# LAUNCH YEAR IS THE SAME AS THE TEST YEAR
launch_year = test_year
# THE NUMBER OF AGE GROUPS
SIZE<-18
# NUMBER OF PROJECTION STEPS
STEPS<-17
# FORECAST LENGTH. SINCE THE PROJECTION INTERVAL IS 5 YEARS IT IS (STEPS*5)

FORLEN<-(STEPS*5)

years <- 0
years$YEAR <- seq(launch_year+5,launch_year+(STEPS*5), 5)

## Loading the CBG population projections. These come from the SLR Typology paper
proj <- read_csv("./R/DATA-RAW/blkgrp_projections_20002100_controlled.csv")
zz <- proj %>%
  group_by(FIPS, YEAR) %>%
  dplyr::summarise(SSP2 = sum(SSP1))

## Loading in the Projections from Scott Kulpt and Climate Central
files <- paste0("./R/DATA-RAW/hauer_v3_mar2020/", list.files(path = "./R/DATA-RAW/hauer_v3_mar2020/",pattern = 'rcp85\\.csv'))
temp <- lapply(files, read_csv)

## Converting the Climate Central data into a useable format.
### Also summing to the national level
together <- rbindlist( temp ) %>% # binding the data together
  # filter(GEOID10 == "220979604002") %>%
  pivot_longer(-GEOID10, names_to = "variable", values_to = "count") %>% # going from wide to long
  mutate(var2 = substr(variable, 1,3)) #selecting only the EAE. Creating a var to do that.
together <- together[which(together$var2 == "EAE"),] %>%
  # filter(var2 == "EAE") %>%
  # group_by(GEOID10, variable) %>%
  # dplyr::summarise(Population = sum(count)) %>% #getting a sum.
  separate(variable, c("variable", "year", "prob", "SSP"))
together <- together[which(together$prob == "SSP2"),] %>%
  mutate(YEAR = as.numeric(year)) %>%
    dplyr::select(GEOID = GEOID10, YEAR, variable, prob, count)

ssp5rcp85 <- left_join(together, proj) %>%
  mutate(ssp5rcp85 = count / SSP2) %>%
  group_by(YEAR, FIPS) %>%
  dplyr::summarise(count = sum(count),
                   SSP5 = sum(SSP2)) %>%
  group_by(FIPS) %>%
   mutate(ssp5rcp85 = 1-((count-lag(count)))/SSP5)
