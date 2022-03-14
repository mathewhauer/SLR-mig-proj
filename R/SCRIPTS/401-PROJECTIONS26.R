set.seed(100)

source('./R/SCRIPTS/000-Libraries.R')      # loading in the libraries
source('./R/SCRIPTS/001-fipscodes.R')
source('./R/SCRIPTS/003-proj_basedataload.R')
# source('./R/SCRIPTS/007-FormatSSPs.R')
Klaunch <- K05_pop[which(K05_pop$YEAR==launch_year),]
statelist <- unique(Klaunch$STATE)

samp <- unique(Klaunch$COUNTYRACE)
# samp <- c("12086_", "12095_", sample(samp, 1000))
# samp <- unique(K05_pop$COUNTYRACE[which(K05_pop$STATE == 15)])
# samp <- c("13121_", "12086_")
# samp <- "13121_"

# Making a list to loop over.
x = unlist(list(paste0(samp)))
toymodel <- substr(x, 1,5)

# Filtering out the pop data to include only those in the list x.
K05 <- K05_pop[which(K05_pop$COUNTYRACE %in% x),] %>%
  K05 <- K05_pop[which(K05_pop$COUNTYRACE %in% samp[2017]),] %>%
  group_by(YEAR,  STATE, COUNTY, SEX, AGE, COUNTYRACE) %>%
  dplyr::summarise(POPULATION = sum(POPULATION)) %>%
  ungroup()

z <- K05_pop[which(K05_pop$COUNTYRACE %in% x & K05_pop$YEAR == launch_year),] %>%
  arrange(GEOID, SEX, AGE) %>%
  pivot_wider(names_from = AGE, values_from = POPULATION, values_fill = 0) %>%
  pivot_longer(cols = c(`1`:`18`), names_to = "AGE", values_to = "POPULATION") %>%
  mutate(AGE = as.numeric(AGE)) %>%
  arrange(GEOID, SEX, AGE)

# p0 <- array(0,c(nrow(z),3))
# pred0 <- array(0,c(nrow(z),3))
# # for(j in 1:3){
# # p0[,j] <- z$POPULATION
p0p5 <- p0p50 <- p0p95 <- z$POPULATION
pred0p5 <-pred0p50 <-pred0p95 <- z$POPULATION
# }


basedat <- K05_pop[which(K05_pop$COUNTYRACE %in% x & K05_pop$YEAR == launch_year),] %>%
  arrange(GEOID, SEX, AGE) %>%
  pivot_wider(names_from = AGE, values_from = POPULATION, values_fill = 0) %>%
  pivot_longer(cols = c(`1`:`18`), names_to = "AGE", values_to = "POPULATION") %>%
  mutate(AGE = as.numeric(AGE)) %>%
  arrange(GEOID, SEX, AGE) %>%
  ungroup() %>%
  dplyr::select(-POPULATION, -YEAR)

# rm(BACCR, BACCRreduce, gqpop, K05, Klaunch)
gc(reset=TRUE)
proj <- NULL

probs <- c("p5", "p50", "p95")

for (i in 1:STEPS){
  print(i)
  for(this.prob in probs){
  S <- readMM(paste0("./R/DATA-PROCESSED/MATRICES/S",i,this.prob,"26.mtx"))
  MS <- readMM(paste0("./R/DATA-PROCESSED/MATRICES/MS",i,this.prob,"26.mtx"))
  popdat<- Matrix(get(paste0("p",i-1,this.prob))) # getting the population vector
  popdatred<- Matrix(get(paste0("pred",i-1, this.prob))) # getting the population vector

  
  proj2 <- S %*% popdat
  projred2 <- MS %*% popdatred
  projredbase <- S %*% popdatred
  
  proj3 <-cbind(basedat, as.matrix(proj2), as.matrix(projred2))
  colnames(proj3)[7] <- paste0("mean_base")
  colnames(proj3)[8] <- paste0("mean_mig")
  # projz <- cbind(basedat, proj2)
  proj3$YEAR <- 2020 + i*5 # setting the year
  proj3$prob <- this.prob
  head(proj3)
  
  # Joining the full projection with the rest of the group.
  proj <- rbind(proj, proj3 )
  # Converting back to a data matrix. THis becomes the basepopulation for the next step in the projections
  proj4red <- proj3$mean_mig %>% data.matrix
  proj4  <- proj3$mean_base %>% data.matrix
  
  assign(paste0("pred",i, this.prob), proj4red)
  assign(paste0("p",i, this.prob), proj4)
  }
}

write_csv(proj, "./R/DATA-PROCESSED/PROJECTIONS/projections_AS_MSP26.csv")