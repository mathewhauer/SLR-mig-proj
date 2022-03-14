source('./R/SCRIPTS/007-FormatSSPs.R')
source('./R/SCRIPTS/001-fipscodes.R')

## Pulling in the raw population projections
proj26 <- read_csv("./R/DATA-PROCESSED/PROJECTIONS/projections_AS_MSP26.csv")
proj45 <- read_csv("./R/DATA-PROCESSED/PROJECTIONS/projections_AS_MSP45.csv")
proj85 <- read_csv("./R/DATA-PROCESSED/PROJECTIONS/projections_AS_MSP85.csv")

proj26$scen = "RCP2.6"
proj45$scen = "RCP4.5"
proj85$scen = "RCP8.5"

proj <- rbind(proj26, proj45, proj85)

## We will rake the AS projections to control to the SSPs for the entire US.
## To do so, we need to calculate the percentage of each individual AS group for the total.
totals <- proj %>%
  group_by(AGE, SEX, prob, scen, YEAR) %>%
  dplyr::summarise(tot_base = sum(mean_base), # Summing all ASC groups to create the total
                   tot_mig = sum(mean_mig)) 

totals2 <- left_join(proj, totals) %>%
  mutate(percentage_base = (mean_base/tot_base), # Calculating the % of each ASC group to the projected total.
         percentage_mig = (mean_mig/tot_mig),
         SEX = as.numeric(SEX))

## With the percentages, we simply calculate the controlled population as
## SSP total * the percentage * 1,000,000.
test <- left_join(totals2, SSPs2) %>%
  mutate(SSP1_BASE = SSP1*percentage_base*1000000,
         SSP2_BASE = SSP2*percentage_base*1000000,
         SSP3_BASE = SSP3*percentage_base*1000000,
         SSP4_BASE = SSP4*percentage_base*1000000,
         SSP5_BASE = SSP5*percentage_base*1000000
  ) %>%
  dplyr::select(YEAR, SEX, GEOID, AGE, prob,scen, SSP1_BASE:SSP5_BASE)

## Fixing NA values. Also calculating the ratio between the BASE and MIG projections called per.
proj2 <- proj
proj2$per <- proj2$mean_mig / proj2$mean_base
proj2 <- dplyr::select(proj2, -mean_base,-mean_mig)
proj2[mapply(is.nan, proj2)] <- NA
proj2[mapply(is.infinite, proj2)] <- NA
proj2[is.na(proj2)] <-1

# Joining these together to calculate the MIG scenario.
test <- left_join(test, proj2)
test <- test %>%
  mutate(SSP1_MIG = SSP1_BASE * per,
         SSP2_MIG = SSP2_BASE * per,
         SSP3_MIG = SSP3_BASE * per,
         SSP4_MIG = SSP4_BASE * per,
         SSP5_MIG = SSP5_BASE * per)

## Getting totals.
test2 <- test %>%
  group_by(YEAR, prob, scen, GEOID) %>%
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
  ungroup() 

## Now we have to calculate the Inundated Population.
baseloss <- rbind(
    read_csv("./R/DATA-PROCESSED/basepercentagepoploss26.csv") %>% mutate(scen = "RCP2.6"),
    read_csv("./R/DATA-PROCESSED/basepercentagepoploss45.csv") %>% mutate(scen = "RCP4.5"),
    read_csv("./R/DATA-PROCESSED/basepercentagepoploss85.csv") %>% mutate(scen = "RCP8.5")
    
    ) %>%
  filter(SSP2 == "SSP2") %>%
  mutate(prob = prob2) %>%
  dplyr::select(-prob2)



## Pulling in the migration data. This gives us the probability of migrating from county i to county j.
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

# Joining up the BASE projection with the Inundated %
projsums <- test2 %>%
  left_join(., baseloss %>% dplyr::select(everything(), YEAR = year)) %>%
  mutate(Inundated = case_when(
    is.na(Inundated) ~ 1,
    TRUE ~ 1-Inundated
  )) %>%
  pivot_longer( cols = SSP1_BASE:SSP5_BASE, names_to = "Scen", values_to = "BASE") %>%
  mutate(Inundated_pop = Inundated * BASE) %>%  # Calculating the INUNDATED projection
  dplyr::select(-Inundated, -EAE, -SSP2, -BASE, -SSP1_MIG:-SSP5_MIG) %>%
  mutate(Scen = str_replace(Scen, pattern = "BASE", replacement= "INUN")) %>%
  pivot_wider(names_from = Scen, values_from = Inundated_pop) %>%
  left_join(test2) %>%
  pivot_longer(cols = SSP1_INUN:SSP5_MIG, names_to = "Scen") %>%
  separate(Scen, into = c("SSP", "Scen"), sep = "_") %>%
  pivot_wider(names_from = Scen, values_from = value) %>%
  mutate(migrants = BASE - INUN) # This is the number of migrants who will move


## Vector size is too large for this next operation so we have to chop it up.
proj_inun2 <- data.table()
for(this.rcp in c("RCP2.6", "RCP4.5", "RCP8.5")){
a <- projsums[which(projsums$scen == this.rcp),] %>%
  left_join(., migs2) %>%
  mutate(migrants2 = migrants * freq) %>% # moving the migrants
  group_by(destination, prob, YEAR, scen, SSP) %>%
  dplyr::summarise(mig = sum(migrants2))  %>% # getting the total in-migrants
dplyr::select(GEOID = destination, everything())

proj_inun2 <- rbind(proj_inun2, a)
}

# Calculating the Inundation percentage.
proj_inun3 <- left_join(projsums, proj_inun2) %>%
  mutate(Inundated2 = BASE + mig - migrants) %>%
  mutate(Inundated2 = ifelse(is.na(Inundated2), BASE, Inundated2)) %>% # INUN scenario is the BASE + in-migrants - out-migrants
  dplyr::select(GEOID, YEAR, prob, INUN = Inundated2, BASE, MIG, scen, SSP) 

projsums2 <- proj_inun3 %>%
  # left_join(., red) %>%
  mutate(Disp = abs(BASE - INUN), # The number of displaced persons is equal to the absolute value of the difference between BASE and INUN
         mig = abs(BASE - MIG)) %>% # The calculation for the MIG pop is the same.
  group_by(YEAR, prob, SSP, scen) %>%
  dplyr::summarise(INUN = sum(INUN),
                   BASE = sum(BASE),
                   MIG = sum(MIG),
                   DISP = sum(abs(Disp)),
                   Mig = sum(mig))

# Creating the final values for the table.
projsums3 <- projsums2 %>%
  dplyr::select(-INUN:-MIG) %>%
  mutate(DISP = prettyNum(DISP/1e6,digits = 2), # Moving decimals to be in millions and only 2 digits.
         Mig = prettyNum(Mig /1e6, digits = 2)) %>%
  pivot_wider(names_from = prob, values_from = c(DISP, Mig)) %>% # Putting all values as columns
  mutate(Disp = paste0(DISP_p50," [", DISP_p5," - ", DISP_p95,"]"),
         Mig = paste0(Mig_p50," [", Mig_p5," - ", Mig_p95,"]") ) %>% # Pasting the values together
  dplyr::select(-DISP_p5:-Mig_p95) %>%
  pivot_longer(cols = c(Disp, Mig), names_to = "name") %>% # Going back to long in order to propertly go back to wide.
  pivot_wider(names_from = SSP, values_from = value) %>%
  filter(YEAR == 2100)

# SLR amounts for each RCP.
SLRamnt <- data.frame(RCP = c("8.5", "4.5", "2.6"),
                      SLR = c("0.79 [0.52 - 1.2]", "0.59 [0.36 - 0.93]", "0.5 [0.29 - 0.82]"))



# projsums2 <- left_join(projsums, proj_inun3)%>%
#   mutate(SSP2_inun = ifelse(is.na(SSP2_inun), SSP2_BASE, SSP2_inun),
#          Inundated_max = ifelse(is.na(Inundated_max), Base_max, Inundated_max),
#          Inundated_min = ifelse(is.na(Inundated_min), Base_min, Inundated_min))
# 
# write_csv(projsums2, "./R/DATA-PROCESSED/PROJECTIONS/projections_TOT_controlled_MSP.csv")
# 
# write_csv(test, "./R/DATA-PROCESSED/PROJECTIONS/projections_AS_controlled_MSP.csv")