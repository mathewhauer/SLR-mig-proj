
source("./R/SCRIPTS/000-Libraries.R")

controlled <- read_csv("./R/DATA-RAW/blkgrp_projections_20002100_controlled.csv") %>%
  dplyr::select(everything(), GEOID10=GEOID, year = YEAR) %>%
  pivot_longer(c(-GEOID10, -FIPS, -year, -STATE, -COUNTY), names_to = "SSP2", values_to = "Projection") %>%
  mutate(GEOID = substr(GEOID10,1,5)) %>%
  group_by(year, GEOID, SSP2) %>%
  dplyr::summarise(proj = sum(Projection))

## Loading in the Projections from Scott Kulp and Climate Central
files <- paste0("R/DATA-RAW/Dat/", list.files(path = "./R/DATA-RAW/Dat/",pattern = 'rcp45\\.csv'))
files <- files[files != "R/DATA-RAW/Dat/BlockgroupProjectedEAE.v3.LA.rcp45.csv" ]
files2 <- read_csv("./R/DATA-RAW/Dat/BlockgroupProjectedEAE.v3.LA.rcp45.csv") %>%
  pivot_longer(-GEOID10, names_to = "variable", values_to = "count") %>%
  mutate(GEOID = substr(GEOID10,1,5)) %>%
  group_by(GEOID, variable) %>%
  dplyr::summarise(Population = sum(count)) %>%
  separate(variable, c("variable", "year", "prob", "SSP")) %>%
  filter(variable != "LECZ")
temp <- lapply(files, read_csv)

## Converting the Climate Central data into a useable format.
### Also summing to the county level
together <- rbindlist( temp ) %>%
  # filter(GEOID10 == "220979604002") %>%
  pivot_longer(-GEOID10, names_to = "variable", values_to = "count") %>%
  mutate(GEOID = substr(GEOID10,1,5)) %>%
  group_by(GEOID, variable) %>%
  dplyr::summarise(Population = sum(count)) %>%
  separate(variable, c("variable", "year", "prob", "SSP")) %>%
  rbind(., files2) %>%
  group_by(variable, GEOID, year, prob, SSP) %>%
  dplyr::summarise(Population = sum(Population)) %>%
  mutate(SSP2 = if_else(is.na(SSP), prob, SSP),
         prob2 = if_else(is.na(SSP),"p50", prob),
         Type = case_when(
           variable == "EAE" ~ "Exp. Ann. Flood",
           variable == "MHHW" ~ "Inundated",
           variable == "RL100" ~ "100-year FP"
         ),
         year = as.numeric(year))  %>%
  ungroup() %>%
  dplyr::select(everything(), -variable, -prob, -SSP ) %>%
  na.omit() %>%
  pivot_wider( names_from = Type, values_from = Population)

## Calculating the percentage of the population at-risk to SLR
together2 <- left_join(together, controlled, by = c("GEOID", "year", "SSP2")) %>%
  pivot_longer(cols = c(`Exp. Ann. Flood`:`100-year FP`), names_to = "Var",
               values_to = "AtRisk") %>%
  mutate(PerAtRisk = AtRisk/proj) %>%
  dplyr::select(-proj, -AtRisk) %>%
  pivot_wider(names_from = Var, values_from = PerAtRisk) 

## Creating a new dataframe that contains the off-years. Climate Central's data is in 10-year increments.
df <- together2 %>%
  mutate(year = year+5,
         `Exp. Ann. Flood` = NA,
         `Inundated` = NA,
         `100-year FP` = NA) %>%
  filter(year <2100) 

# Joining these back to together so I have 2000-2100 in 5-year increments. Linear interpolating between points.
together3 <- rbind(together2, df) %>%
  arrange(GEOID, year, SSP2, prob2) %>%
  group_by(GEOID, SSP2, prob2) %>%
  mutate(Inundated = case_when(
    is.na(Inundated) ~ (lag(Inundated) + lead(Inundated))/2,
    TRUE ~ Inundated)
  ) %>%
  filter(prob2 == "p50",
         SSP2 == "SSP2",
         year >=2020)

## Loading in the model coefficients.
coeffs <- read_csv("./R/DATA-PROCESSED/modelcoeffs.csv")

z <- crossing(together3, coeffs) %>%
  mutate(reduce = exp(b * log(1-Inundated) + c * (log(1-Inundated)^2)))

write_csv(z, "./R/DATA-PROCESSED/atriskpops.csv")
write_csv(together3, "./R/DATA-PROCESSED/basepercentagepoploss.csv")
