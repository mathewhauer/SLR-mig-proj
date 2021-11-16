source('./R/SCRIPTS/007-FormatSSPs.R')
source('./R/SCRIPTS/001-fipscodes.R')
proj <- read_csv("./R/DATA-PROCESSED/PROJECTIONS/projections_AS_eae_MSP.csv")
totals <- proj %>%
  group_by(AGE, SEX, prob, YEAR) %>%
  dplyr::summarise(tot_mig = sum(mean_mig),
                   tot_base = sum(mean_base)) 
totals2 <- left_join(proj, totals) %>%
  mutate(percentage_mig = (mean_mig/tot_mig),
         percentage_base = (mean_base/tot_base),
         SEX = as.numeric(SEX))

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
  dplyr::select(YEAR, SEX, GEOID, AGE, prob, SSP1_BASE:SSP5_MIG)

test2 <- test %>%
  group_by(YEAR, prob, GEOID) %>%
  dplyr::summarise(SSP1_BASE = sum(SSP1_BASE),
                   SSP2_BASE = sum(SSP2_BASE),
                    SSP3_BASE = sum(SSP3_BASE),
                    SSP4_BASE = sum(SSP4_BASE),
                    SSP5_BASE = sum(SSP5_BASE),
                    SSP1_MIG = sum(SSP1_MIG),
                   SSP2_MIG = sum(SSP2_MIG),
                    SSP3_MIG = sum(SSP3_MIG),
                    SSP4_MIG = sum(SSP4_MIG),
                    SSP5_MIG = sum(SSP5_MIG)
                   ) %>%
  ungroup() %>%
  rowwise() %>%
  mutate(Base_max = max(SSP1_BASE, SSP2_BASE, SSP3_BASE, SSP4_BASE, SSP5_BASE),
         Base_min = min(SSP1_BASE, SSP2_BASE, SSP3_BASE, SSP4_BASE, SSP5_BASE),
         Mig_max = max(SSP1_MIG, SSP2_MIG, SSP3_MIG, SSP4_MIG, SSP5_MIG),
         Mig_min = min(SSP1_MIG, SSP2_MIG, SSP3_MIG, SSP4_MIG, SSP5_MIG)) %>%
  dplyr::select(YEAR, GEOID, SSP2_BASE, prob, SSP2_MIG, Base_max:Mig_min)

baseloss <- read_csv("./R/DATA-PROCESSED/basepercentagepoploss.csv") %>%
  filter(SSP2 == "SSP2") %>%
  mutate(prob = prob2) %>%
  dplyr::select(-prob2)

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
  SSP2_inun = Inundated * SSP2_BASE,
  Inun_max = Inundated * Base_max,
  Inun_min = Inundated * Base_min) %>%
  # mean_inun = Inundated * mean_base) %>%
  dplyr::select(-SSP2_inun, -EAE) %>%
  mutate(diff = SSP2_MIG-SSP2_BASE)

## Producing the Inundation projection
# baseloss <- read_csv("./R/DATA-PROCESSED/basepercentagepoploss.csv") %>%
#   filter(SSP2 == "SSP2"),
#          prob2 == "p95")

migs <- read_csv("./R/DATA-PROCESSED/migrationprobs.csv") %>%
  mutate(origin = str_pad(origin,5, pad="0"),
         destination = str_pad(destination, 5, pad="0")) %>%
  filter(origin %in% toymodel)
migs$COUNTYRACE <- paste0(migs$origin, "_", migs$RACE)
migs <- as.data.frame(migs)
migs2 <- migs %>%
  filter(origin!=destination) %>%
  dplyr::select(origin, destination, step, `Point Forecast`) %>%
  group_by(origin, destination, step) %>%
  pivot_wider(names_from = destination, values_from = `Point Forecast`, values_fill = 0) %>%
  pivot_longer(cols = c(3:ncol(.)), names_to = "destination", values_to = "freq") %>%
  dplyr::select(GEOID = origin, destination, freq, step)
migs2 <- migs2[which(migs2$freq>0),] %>%
  group_by(GEOID, step) %>%
  mutate(freq = `freq`/sum(`freq`))
if(length(setdiff(toymodel, unique(migs2$destination)))>0){
  missing <- data.frame(destination=  setdiff(toymodel, unique(migs2$destination)),
                        GEOID = '01001',
                        freq = 0)
  migs2 <- rbind(migs2, missing)
  rm(missing)
}
migs2$YEAR = migs2$step*5 + 2015


proj_inun <- projsums %>%
  # group_by(GEOID, YEAR) %>%
  # dplyr::summarise(mean_base = sum(mean_base)) %>%
  # ungroup() %>%
  left_join(., baseloss %>% dplyr::select(everything(), YEAR = year)) %>%
  mutate(Inundated = case_when(
    is.na(Inundated) ~ 0,
    TRUE ~ 1-Inundated
  ),
  mean_inun = Inundated * SSP2_BASE,
  Inun_max = Inundated * Base_max,
  Inun_min = Inundated * Base_min) %>%
  # dplyr::select(-`Exp. Ann. Flood`, -`100-year FP`, -prob2) %>%
  mutate(migrants = SSP2_BASE - mean_inun,
         migrants_min = Base_min - Inun_min,
         migrants_max = Base_max - Inun_max) 
proj_inun2 <- proj_inun %>%
  left_join(., migs2) %>%
  mutate(migrants2 = mean_inun * freq,
         migrants_min = Inun_min * freq,
         migrants_max = Inun_max * freq) %>%
  group_by(destination, prob, YEAR) %>%
  dplyr::summarise(mean_mig = sum(migrants2),
                   mig_min = sum(migrants_min),
                   mig_max = sum(migrants_max)) %>%
  dplyr::select(GEOID = destination, YEAR, prob, mean_mig:mig_max)

proj_inun3 <- left_join(proj_inun, proj_inun2) %>%
  mutate(Inundated2 = SSP2_BASE + mean_mig - mean_inun,
         Inundated_max = Base_max + mig_max - Inun_max,
         Inundated_min = Base_min + mig_min - Inun_min) %>%
  dplyr::select(GEOID, YEAR, prob, SSP2_inun = Inundated2, Inundated_max, Inundated_min) 

projsums2 <- left_join(projsums, proj_inun3)%>%
  mutate(SSP2_inun = ifelse(is.na(SSP2_inun), SSP2_BASE, SSP2_inun),
         Inundated_max = ifelse(is.na(Inundated_max), Base_max, Inundated_max),
         Inundated_min = ifelse(is.na(Inundated_min), Base_min, Inundated_min))

write_csv(projsums2, "./R/DATA-PROCESSED/PROJECTIONS/projections_TOT_controlled_eae_MSP.csv")

write_csv(test, "./R/DATA-PROCESSED/PROJECTIONS/projections_AS_controlled_eae_MSP.csv")