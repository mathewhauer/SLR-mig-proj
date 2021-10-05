source('./R/SCRIPTS/007-FormatSSPs.R')
proj <- read_csv("./R/DATA-PROCESSED/PROJECTIONS/projections_AS.csv")
totals <- proj %>%
  group_by(AGE, SEX, YEAR) %>%
  dplyr::summarise(tot_mig = sum(mean_mig),
                   tot_base = sum(mean_base)) 
totals2 <- left_join(proj, totals) %>%
  mutate(percentage_mig = (mean_mig/tot_mig),
         percentage_base = (mean_base/tot_base))

test <- left_join(totals2, SSPs2) %>%
  mutate(SSP1_BASE = SSP1*percentage_base*1000000,
         SSP2_BASE = SSP2*percentage_base*1000000,
         SSP3_BASE = SSP3*percentage_base*1000000,
         SSP4_BASE = SSP4*percentage_base*1000000,
         SSP5_BASE = SSP5*percentage_base*1000000,
         SSP1_MIG = SSP1*percentage_mig*1000000,
         SSP2_MIG = SSP2*percentage_mig*1000000,
         SSP3_MIG = SSP3*percentage_mig*1000000,
         SSP4_MIG = SSP4*percentage_mig*1000000,
         SSP5_MIG = SSP5*percentage_mig*1000000
  ) %>%
  dplyr::select(YEAR, SEX, GEOID, AGE, SSP2_BASE,SSP2_MIG)

test2 <- test %>%
  group_by(YEAR, GEOID) %>%
  dplyr::summarise(#SSP1_BASE = sum(SSP1_BASE),
                   SSP2_BASE = sum(SSP2_BASE),
                   # SSP3_BASE = sum(SSP3_BASE),
                   # SSP4_BASE = sum(SSP4_BASE),
                   # SSP5_BASE = sum(SSP5_BASE),
                   # SSP1_MIG = sum(SSP1_MIG),
                   SSP2_MIG = sum(SSP2_MIG),
                   # SSP3_MIG = sum(SSP3_MIG),
                   # SSP4_MIG = sum(SSP4_MIG),
                   # SSP5_MIG = sum(SSP5_MIG)
                   ) 

baseloss <- read_csv("./R/DATA-PROCESSED/basepercentagepoploss.csv") %>%
  filter(SSP2 == "SSP2",
         prob2 == "p95")

projsums <- test2 %>%
  # group_by(STATE, COUNTY, GEOID,YEAR) %>%
  # group_by(GEOID, YEAR) %>%
  # dplyr::summarise(mean_mig = sum(mean_mig),
  #                  mean_base = sum(mean_base)) %>%
  left_join(., baseloss %>% dplyr::select(everything(), YEAR = year)) %>%
  mutate(Inundated = case_when(
    is.na(Inundated) ~ 1,
    TRUE ~ 1- Inundated
  ),
  SSP2_inun = Inundated * SSP2_BASE) %>%
  # mean_inun = Inundated * mean_base) %>%
  dplyr::select(-`Exp. Ann. Flood`, -`100-year FP`, -prob2) %>%
  mutate(diff = SSP2_MIG-SSP2_BASE)

write_csv(projsums, "./R/DATA-PROCESSED/PROJECTIONS/projections_TOT_controlled.csv")

write_csv(test, "./R/DATA-PROCESSED/PROJECTIONS/projections_AS_controlled.csv")