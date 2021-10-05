set.seed(100)

source('./R/SCRIPTS/000-Libraries.R')      # loading in the libraries
source('./R/SCRIPTS/001-fipscodes.R')
source('./R/SCRIPTS/003-proj_basedataload.R')
source('./R/SCRIPTS/007-FormatSSPs.R')
Klaunch <- K05_pop[which(K05_pop$YEAR==launch_year),]
statelist <- unique(Klaunch$STATE)


## SSP data from IIASA
# unzip(zipfile='./R/DATA-RAW/SspDb_country_data_2013-06-12.csv.zip', exdir = "./R/DATA-RAW")

# gq_2010.csv IS THE RESULT OF 
gqpop <- read.csv("R/DATA-PROCESSED/gq_2010.csv") %>%
  mutate(STATE = str_pad(STATE, 2, pad = "0"),
         COUNTY = str_pad(COUNTY, 3, pad="0"),
         GEOID = paste0(STATE, COUNTY),
         AGE = AGEGRP,
         SEX = case_when(
           SEX == "MALE" ~ "1",
           SEX == "FEMALE" ~ "2"),
         RACE = case_when(
           RACE == "BLACK, NH" ~ "2",
           RACE == "OTHER" ~ "3",
           RACE == "WHITE, NH" ~ "1",
           RACE == "HISPANIC" ~ "3",
           RACE == "OTHER, NH" ~ "3")) %>%
  group_by(.dots = GROUPING) %>%
  dplyr::summarise(group_quarters = sum(GQ, na.rm=T))

# Setting the GEOID for the gq file
gqpop$GEOID <- paste0(gqpop$STATE, gqpop$COUNTY)
gqpop$COUNTYRACE <- paste0(gqpop$GEOID, "_", gqpop$RACE)

# Pulling in the fertility rate data. This is state-level.
stateferts <- read_csv("R/DATA-PROCESSED/state-level-fert-rates_20152100.csv")



samp <- unique(Klaunch$COUNTYRACE)
# samp <- c("12086_", "12095_", sample(samp, 1000))
# samp <- unique(K05_pop$COUNTYRACE[which(K05_pop$STATE == 15)])
# samp <- c("13121_", "12086_")
# samp <- "13121_"

# Making a list to loop over.
x = unlist(list(paste0(samp)))

toymodel <- substr(x, 1,5)

## Pulling in the reductions of CCRs by year.
reductions <- read_csv("./R/DATA-PROCESSED/atriskpops.csv") %>%
  filter(GEOID %in% toymodel) %>%
  mutate(sex = as.character(sex)) %>%
  dplyr::select(GEOID, year, SEX=sex, ccr = groups, reduce) %>%
  group_by(GEOID, SEX,ccr) %>%
  mutate(step = as.character(row_number()))
reductions$COUNTYRACE <- paste0(reductions$GEOID, "_", reductions$RACE)

## Pulling in the migration probabilities data.
migs <- read_csv("./R/DATA-PROCESSED/migrationprobs.csv") %>%
  mutate(origin = str_pad(origin,5, pad="0"),
         destination = str_pad(destination, 5, pad="0")) %>%
  filter(origin %in% toymodel)
migs$COUNTYRACE <- paste0(migs$origin, "_", migs$RACE)
migs <- as.data.frame(migs)
odpairs <- data.frame(pairs = unique(paste(migs$origin, migs$destination))) %>%
  mutate(nums = 1:n()) %>%
  separate(pairs, into = c("origin", "destination"), sep = " ")

# migs2 <- migs[which(migs$step == 1),] %>%
#   group_by(origin, destination) %>%
#   tidyr::expand(new = as.numeric(seq(1:36))) %>%
#   left_join(migs) %>%
#   mutate(SEX = as.character(if_else(new<=18,1,2))) %>%
#   ungroup() %>%
#   as.data.frame() %>%
#   mutate(AGE = case_when(
#     new > 18 ~ new-18,
#     TRUE ~ new)
#   ) %>%
#   dplyr::select(GEOID = origin, destination, freq, step, SEX, AGE, COUNTYRACE)

# Filtering out the pop data to include only those in the list x.
K05 <- K05_pop[which(K05_pop$COUNTYRACE %in% x),] %>%
  group_by(YEAR,  STATE, COUNTY, SEX, AGE, COUNTYRACE) %>%
  dplyr::summarise(POPULATION = sum(POPULATION)) %>%
  ungroup()


# migs <- left_join(K05, migs) %>%
#   dplyr::select(STATE, GEOID, `Point Forecast`:`Hi 80`, STEP)


### Calculating the CCRs
CCRs<- K05 %>%
  ungroup() %>%
  mutate(AGE = paste0("X", str_pad(AGE, 2, pad ="0")),
         GEOID = paste0(STATE, COUNTY),
         POPULATION = as.numeric(POPULATION)) %>%
  spread(AGE, POPULATION)
if(is.null(CCRs$X01)){CCRs$X01=0}else{CCRs$X01=CCRs$X01}
if(is.null(CCRs$X02)){CCRs$X02=0}else{CCRs$X02=CCRs$X02}
if(is.null(CCRs$X03)){CCRs$X03=0}else{CCRs$X03=CCRs$X03}
if(is.null(CCRs$X04)){CCRs$X04=0}else{CCRs$X04=CCRs$X04}
if(is.null(CCRs$X05)){CCRs$X05=0}else{CCRs$X05=CCRs$X05}
if(is.null(CCRs$X06)){CCRs$X06=0}else{CCRs$X06=CCRs$X06}
if(is.null(CCRs$X07)){CCRs$X07=0}else{CCRs$X07=CCRs$X07}
if(is.null(CCRs$X08)){CCRs$X08=0}else{CCRs$X08=CCRs$X08}
if(is.null(CCRs$X09)){CCRs$X09=0}else{CCRs$X09=CCRs$X09}
if(is.null(CCRs$X10)){CCRs$X10=0}else{CCRs$X10=CCRs$X10}
if(is.null(CCRs$X11)){CCRs$X11=0}else{CCRs$X11=CCRs$X11}
if(is.null(CCRs$X12)){CCRs$X12=0}else{CCRs$X12=CCRs$X12}
if(is.null(CCRs$X13)){CCRs$X13=0}else{CCRs$X13=CCRs$X13}
if(is.null(CCRs$X14)){CCRs$X14=0}else{CCRs$X14=CCRs$X14}
if(is.null(CCRs$X15)){CCRs$X15=0}else{CCRs$X15=CCRs$X15}
if(is.null(CCRs$X16)){CCRs$X16=0}else{CCRs$X16=CCRs$X16}
if(is.null(CCRs$X17)){CCRs$X17=0}else{CCRs$X17=CCRs$X17}
if(is.null(CCRs$X18)){CCRs$X18=0}else{CCRs$X18=CCRs$X18}
CCRs<- CCRs %>%
  arrange(GEOID, SEX, YEAR) %>%
  mutate(ccr1 = X02 / lag(X01, 5),
         ccr2 = X03 / lag(X02, 5),
         ccr3 = X04 / lag(X03, 5),
         ccr4 = X05 / lag(X04, 5),
         ccr5 = X06 / lag(X05, 5),
         ccr6 = X07 / lag(X06, 5),
         ccr7 = X08 / lag(X07, 5),
         ccr8 = X09 / lag(X08, 5),
         ccr9 = X10 / lag(X09, 5),
         ccr10 = X11 / lag(X10, 5),
         ccr11 = X12 / lag(X11, 5),
         ccr12 = X13 / lag(X12, 5),
         ccr13 = X14 / lag(X13, 5),
         ccr14 = X15 / lag(X14, 5),
         ccr15 = X16 / lag(X15, 5),
         ccr16 = X17 / lag(X16, 5),
         ccr17 = X18 / (lag(X17, 5) + lag(X18, 5)),
         ccr18 = ccr17
         ) %>%
  filter(YEAR >= min(YEAR +5, na.rm=T) & YEAR <= test_year)

CCRs[mapply(is.infinite, CCRs)] <- NA
CCRs[mapply(is.nan, CCRs)] <- NA
CCRs[is.na(CCRs)] <-0

# A function to project the CCRs/CCDs
predccr = function(ccr, sex, x, DF){
  y <- as_data_frame(DF[[as.character(ccr)]][which(DF$COUNTYRACE == x & DF$SEX == sex )])
  # y <- as_data_frame(CCRs[[as.character("ccr1")]][which(CCRs$COUNTYRACE == x & CCRs$SEX == "2" )])
  
  num<- seq(1,FORLEN,5)
  fore<- tryCatch(forecast(arima(y$value, order = arima_order), h= FORLEN)
                  , error=function(e) array(0, c(STEPS)))
  
  mean <- fore$mean[c(num)]
  lower <- fore$lower[c(num)]
  upper <- fore$upper[c(num)]
  pred<- (t(rbind(mean, lower, upper)))
  
  return(pred)
}

# # Creating a blank dataframe. This will hold the forecasted CCRs/CCDs
# BACCR <- data.table()
# for(this.x in x){
#   print(this.x)
#   for (i in 1:(SIZE)){
#     state <- unique(CCRs$STATE[which(CCRs$COUNTYRACE == this.x)])
#       data_tablef <- as.data.table(cbind(ccr=paste0("ccr",i),
#                                          COUNTYRACE=paste0(this.x),
#                                          STATE = state,
#                                          SEX = "2",
#                                          step = as.numeric(1:STEPS),
#                                          predccr(paste0("ccr",i), "2", x, CCRs)
#                                          ))
#       data_tablem <- as.data.table(cbind(ccr=paste0("ccr",i),
#                                          COUNTYRACE=paste0(this.x),
#                                          STATE = state,
#                                          SEX = "1",
#                                          step = as.numeric(1:STEPS),
#                                          predccr(paste0("ccr",i), "1", x, CCRs)))
#       BACCR <- rbind(BACCR, data_tablef, data_tablem)
#       rm(data_tablef, data_tablem)
#     }
# }
# 
# # Sorting the output.
# BACCR <- as.data.frame(BACCR %>% arrange(COUNTYRACE, as.numeric(step), SEX, as.numeric(ccr)))

# write_rds(BACCR, "./R/DATA-PROCESSED/BACCR.R")
BACCR <- read_rds("./R/DATA-PROCESSED/BACCR.R") %>%
  filter(COUNTYRACE %in% samp)
BACCRreduce <- left_join(BACCR, reductions) %>%
  mutate(reduce = ifelse(ccr=="ccr18",lag(reduce), reduce),
         reduce = ifelse(is.na(reduce),1, reduce),
  `mean` = reduce * as.numeric(`mean`),
         lower = reduce * as.numeric(lower),
         upper = reduce * as.numeric(upper))
        


### Calculating the forecasted CWR's from the UCMs. Confidence interval is set at 80% (1.28*SD) 
n02 <- filter(stateferts, STATE %in% unique(CCRs$STATE)) %>%
  group_by(STATE) %>%
  mutate(STEP = dplyr::row_number(),
         STATE = as.character(STATE))
n02 <- left_join(K05_launch, n02) %>%
  dplyr::select(STATE, GEOID, `Point Forecast`:`Hi 80`, STEP) %>%
  filter(GEOID %in% substr(x, 1,5))

## Putting the CCRs into leslie matrices.
for (i in 1:STEPS){
  
    # Making a giant blank leslie matrix to hold all of the info.
    # Dimensions are SIZE (18 age groups) * the length(x) (the number of counties) * 2 (for men/women)
  weird_data <-  Matrix(0, SIZE*length(x)*2,SIZE*length(x)*2, sparse=TRUE)
    # for(j in 1:3){
  j=1
            # Creating the subdiagonal for the leslie matrix data. This contains the CCRs
      z <- as.numeric(BACCR[which(BACCR$step==i),5+j])
      z <- rbind(0,cbind(Diagonal(x=z),0))
        # Setting the last value in the leslie matrix to 0.
      z[seq(18+1,ncol(z),18),seq(18,nrow(z),18)] = 0
        # "Truncating" the leslie matrix so the open ended interval gets 2 values.
      z[seq(18,ncol(z),18),seq(18,nrow(z),18)] = z[seq(18,ncol(z),18),seq(17,nrow(z),18)]
      z<- z[1:(ncol(z)-1),1:(nrow(z)-1)]
      # assign(paste0("z",j), z)

      # weird_data[,,j] <- z
      weird_data <- z
      rm(z)
    # }
      # weird_data <- array(c(z1, z2, z3), dim = c(SIZE*length(x)*2,SIZE*length(x)*2,3))
assign(paste0("S",i), weird_data)
rm(weird_data)


# weird_data <-  array(0,c(SIZE*length(x)*2,SIZE*length(x)*2, 3))
weird_data <-  Matrix(0, SIZE*length(x)*2,SIZE*length(x)*2, sparse=TRUE)
# for(j in 1:3){
  # Creating the subdiagonal for the leslie matrix data with the reduced CCRs. This contains the CCRs
  # z <- rbind(0,cbind(diag(BACCRreduce[which(BACCRreduce$step==i),5+j]),0))
  z <- as.numeric(BACCRreduce[which(BACCRreduce$step==i),5+j])
  z <- rbind(0,cbind(Diagonal(x=z),0))
  # Setting the last value in the leslie matrix to 0. 
  z[seq(18+1,ncol(z),18),seq(18,nrow(z),18)] = 0
  # "Truncating" the leslie matrix so the open ended interval gets 2 values.
  z[seq(18,ncol(z),18),seq(18,nrow(z),18)] = z[seq(18,ncol(z),18),seq(17,nrow(z),18)]
  z<- z[1:(ncol(z)-1),1:(nrow(z)-1)]
  
  # weird_data[,,j] <- z
  weird_data <- z
  rm(z)
# }
assign(paste0("Sred",i), weird_data)
rm(weird_data)

# weird_data <- array(0,c(SIZE*length(x)*2,SIZE*length(x)*2, 3))
# for(j in 1:3){
#   mig3 <- migs[which(migs$step == i),] %>%
#     arrange(new)
#   # Creating the subdiagonal for the leslie matrix data with the reduced CCRs. This contains the CCRs
#   z <- rbind(0,diag(migs2[which(migs2$step == i),8]))
#   # Setting the last value in the leslie matrix to 0. 
#   z[seq(18+1,ncol(z),18),seq(18,nrow(z),18)] = 0
#   # "Truncating" the leslie matrix so the open ended interval gets 2 values.
#   z[seq(18,ncol(z),18),seq(18,nrow(z),18)] = z[seq(18,ncol(z),18),seq(17,nrow(z),18)]
#   z<- z[1:(ncol(z)-1),1:(nrow(z)-1)]
#   
#   weird_data[,,j] <- z
#   rm(z)
# }

}



### Formatting the base POPULATION data as equal to the total POPULATION minus the group quaters.

z <- K05_pop[which(K05_pop$COUNTYRACE %in% x & K05_pop$YEAR == launch_year),] %>%
  arrange(GEOID, SEX, AGE) %>%
  pivot_wider(names_from = AGE, values_from = POPULATION, values_fill = 0) %>%
  pivot_longer(cols = c(`1`:`18`), names_to = "AGE", values_to = "POPULATION") %>%
  mutate(AGE = as.numeric(AGE)) %>%
  arrange(GEOID, SEX, AGE)

p0 <- array(0,c(nrow(z),3))
pred0 <- array(0,c(nrow(z),3))
# for(j in 1:3){
  # p0[,j] <- z$POPULATION
  p0 <- z$POPULATION
  pred0 <- z$POPULATION
# }


basedat <- K05_pop[which(K05_pop$COUNTYRACE %in% x & K05_pop$YEAR == launch_year),] %>%
  arrange(GEOID, SEX, AGE) %>%
  pivot_wider(names_from = AGE, values_from = POPULATION, values_fill = 0) %>%
  pivot_longer(cols = c(`1`:`18`), names_to = "AGE", values_to = "POPULATION") %>%
  mutate(AGE = as.numeric(AGE)) %>%
  arrange(GEOID, SEX, AGE) %>%
  ungroup() %>%
  dplyr::select(-POPULATION, -YEAR)

rm(BACCR, BACCRreduce, gqpop, K05, Klaunch)
gc(reset=TRUE)
proj <- NULL
for (i in 1:STEPS){
  print(i)
  # Prepping the migration data
  migs2 <- migs[which(migs$step == i),] %>%
    filter(origin!=destination) %>%
    dplyr::select(origin, destination, step, `Point Forecast`) %>%
    group_by(origin, destination) %>%
    pivot_wider(names_from = destination, values_from = `Point Forecast`, values_fill = 0) %>%
    pivot_longer(cols = c(3:ncol(.)), names_to = "destination", values_to = "freq") %>%
    dplyr::select(GEOID = origin, destination, freq)
  migs2 <- migs2[which(migs2$freq>0),] %>%
    group_by(GEOID) %>%
    mutate(freq = `freq`/sum(`freq`))
  if(length(setdiff(toymodel, unique(migs2$destination)))>0){
    missing <- data.frame(destination=  setdiff(toymodel, unique(migs2$destination)),
      GEOID = '01001',
      freq = 0)
    migs2 <- rbind(migs2, missing)
    rm(missing)
  }
                        
  data_tablef <- get(paste0("S",i)) # getting the Survival matrix
  popdat<- Matrix(get(paste0("p",i-1))) # getting the population vector
  popdatred<- Matrix(get(paste0("pred",i-1))) # getting the population vector
  proj2 <- array(0,c(length(p0),3)) # setting up the projection matrix for collecting results
 
  # Repeating this setup for the reduced population.
  data_tablefred <- get(paste0("Sred",i)) # getting the Survival matrix
  # popdatred<- get(paste0("p",i-1)) # getting the population vector
  projred2 <- array(0,c(length(p0),3)) # setting up the projection matrix for collecting results
  
  # actual population projection step  
  # for(j in 1:3){
      proj2 <- data_tablef %*% popdat
      projred2 <- data_tablefred %*% popdatred
      projredbase <- data_tablef %*% popdatred
    # }
    # 
  # migdat <- migs2[which(migs2$step==i),]
  z<- projredbase - projred2
  # zz<-as.data.table(cbind(as.matrix(proj2), as.matrix(projred2)))
  # zz$diff = zz[,1] - zz[,2]
  z2 <-cbind(basedat, as.matrix(z)) %>%
    left_join(., migs2)


  z2$migrants1 = z2[,7] * z2[,9]
  
  # z2$migrants2 = z2$`2`*z2$freq
  # z2$migrants3 = z2$`3`*z2$freq
  z2 <- z2 %>%
    group_by(destination, AGE, SEX) %>%
    dplyr::summarise(`mean_mig` = sum(migrants1),
                     # `2` = sum(migrants2),
                     # `3` = sum(migrants3)
                     ) %>%
    arrange(destination, SEX, AGE) %>%
    filter(destination %in% toymodel)
  displacees <- z2[,4] %>% data.matrix
  proja <- projred2 + displacees
  min(proja)
  # joining the basedata info with the projection
  proj3 <-cbind(basedat, as.matrix(proja)) %>%
    cbind(., as.matrix(proj2))
  colnames(proj3)[8] <- "mean_base"
  # projz <- cbind(basedat, proj2)
  proj3$YEAR <- launch_year + i*5 # setting the year
  
  # Prepping for the 0-year olds age category
  females <- males <- proj3 %>%
    filter(SEX == 2,
           AGE %in% 4:10) %>% # summing the women of childbearing ages
    group_by(STATE, COUNTY, GEOID, COUNTYRACE) %>%
    dplyr::summarise(`mean_mig` = sum(`mean_mig`),
                     `mean_base` = sum(`mean_base`),
                     # `3` = sum(`3`)
                     ) %>%
    ungroup()
  fvar <- n02[which(n02$STEP==i),] # Grabbing the fertility rates for this year.
  # Declaring variables for men/women. Also calculating the # of newborns
  males$AGE <- 1
  males$SEX <- 1
  males$`mean_mig` <- males$`mean_mig` * fvar$`Point Forecast` * 0.512
  males$`mean_base` <- males$`mean_base` * fvar$`Point Forecast` * 0.512
  # males$`3` <- males$`3` * fvar$`Hi 80` * 0.512
  females$AGE <- 1
  females$SEX <- 2
  females$`mean_mig` <- females$`mean_mig` * fvar$`Point Forecast` * 0.488
  females$`mean_base` <- females$`mean_base` * fvar$`Point Forecast` * 0.488
  # females$`3` <- females$`3` * fvar$`Hi 80` * 0.488

  # joining them back together and declaring the year.
  kids <- rbind(females, males)
  kids$YEAR <- launch_year + i*5
  # Joining the kids with the rest of the projected population. Properly sorting it.
  proj3 <- rbind(kids, proj3[which(proj3$AGE != 1),]) %>%
    arrange(GEOID, SEX, AGE)
  # Joining the full projection with the rest of the group.
  proj <- rbind(proj, proj3 )
  # Converting back to a data matrix. THis becomes the basepopulation for the next step in the projections
  proj4red <- proj3[,5] %>% data.matrix
  proj4  <- proj3[,6] %>% data.matrix
  
  assign(paste0("pred",i), proj4red)
  assign(paste0("p",i), proj4)
  
  rm(kids, males, females, proj3, proj4, proj2)
}

proj <- proj %>%
  dplyr::select(GEOID, mean_mig:YEAR)
### Writing projections to the hard drive.
# write_csv(proj, "./R/DATA-PROCESSED/PROJECTIONS/projections_AS.csv")

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
  dplyr::select(YEAR, SEX, GEOID, AGE, SSP1_BASE:SSP5_MIG)

test2 <- test %>%
  group_by(YEAR, GEOID) %>%
  dplyr::summarise(SSP1_BASE = sum(SSP1_BASE),
                   SSP2_BASE = sum(SSP2_BASE),
                   SSP3_BASE = sum(SSP3_BASE),
                   SSP4_BASE = sum(SSP4_BASE),
                   SSP5_BASE = sum(SSP5_BASE),
                   SSP1_MIG = sum(SSP1_MIG),
                   SSP2_MIG = sum(SSP2_MIG),
                   SSP3_MIG = sum(SSP3_MIG),
                   SSP4_MIG = sum(SSP4_MIG),
                   SSP5_MIG = sum(SSP5_MIG)) %>%
  gather(Scenario, Population, SSP1:SSP5)



baseloss <- read_csv("./R/DATA-PROCESSED/basepercentagepoploss.csv") %>%
  filter(SSP2 == "SSP2",
         prob2 == "p95")

projsums <- proj %>%
  # group_by(STATE, COUNTY, GEOID,YEAR) %>%
  group_by(GEOID, YEAR) %>%
  dplyr::summarise(mean_mig = sum(mean_mig),
                   mean_base = sum(mean_base)) %>%
  left_join(., baseloss %>% dplyr::select(everything(), YEAR = year)) %>%
  mutate(Inundated = case_when(
    is.na(Inundated) ~ 1,
    TRUE ~ 1- Inundated
  ),
  mean_inun = Inundated * mean_base) %>%
  dplyr::select(-`Exp. Ann. Flood`, -`100-year FP`, -prob2) %>%
  mutate(diff = mean_mig-mean_base)

write_csv(projsums, "./R/DATA-PROCESSED/PROJECTIONS/projections_TOT.csv")

# cnty <- "06081"
# proj_cnty <- projsums[which(projsums$GEOID == cnty),]
proj_cntya <- test[which(proj$GEOID == cnty),] %>%
  proj_cntya <- test %>%
  group_by(YEAR,
           AGE,
           GEOID) %>%
  dplyr::summarise(mean_mig = sum(SSP2_MIG),
                   mean_base = sum(SSP2_BASE)) %>%
  group_by(YEAR, GEOID) %>%
  # mutate(mean_mig = mean_mig/sum(mean_mig),
  #        mean_base = mean_base / sum(mean_base),
  #   mean_mig = if_else(SEX==1, mean_mig*-1, mean_mig),
  #   mean_base = if_else(SEX==1, mean_base*-1, mean_base)
  #        ) %>%
  mutate(csum = cumsum(mean_mig),
         tot = sum(mean_mig),
         mid = if_else((csum/tot)<=0.5,1,0),
         midpoint = tot/2,
         medage = (midpoint-csum)/(mean_mig)*5 + (AGE*5-5),
         
         csum_base = cumsum(mean_base),
         tot_base = sum(mean_base),
         mid_base= if_else((csum_base/tot_base)<=0.5,1,0),
         midpoint_base = tot_base/2,
         medage_base = (midpoint_base-csum_base)/(mean_base)*5 + (AGE*5-5)) %>%
  # filter(YEAR==2100) %>%
  filter(mid == 1  | mid_base==1) %>%
  filter(AGE == max(AGE)) %>%
  mutate(dif = medage-medage_base)
# ggplot(proj_cnty) +
#   geom_line(aes(x=YEAR, y = mean_mig), color = "red") +
#   annotate("text", x=2090, y=max(proj_cnty$mean_mig)*0.98, label= "mean_mig", color="red") + 
#   geom_line(aes(x=YEAR, y = mean_base), color = "blue") +
#   annotate("text", x=2090, y=max(proj_cnty$mean_base)*0.95, label= "mean_base", color="blue") + 
#   geom_line(aes(x=YEAR, y = mean_inun), color = "black") +
#   annotate("text", x=2090, y=max(proj_cnty$mean_inun)*0.92, label= "mean_ind", color="black") + 
#   labs(title = paste(cnty))
# 

proj_cntyb <- test[which(test$GEOID == cnty),] %>%
  group_by(YEAR, SEX,AGE) %>%
  dplyr::summarise(mean_mig = sum(SSP2_MIG),
                   mean_base = sum(SSP2_BASE)) %>%
  # group_by(YEAR, GEOID) %>%
  mutate(
    # mean_mig = mean_mig/sum(mean_mig),
    #      mean_base = mean_base / sum(mean_base),
    mean_mig = if_else(SEX==1, mean_mig*-1, mean_mig),
    mean_base = if_else(SEX==1, mean_base*-1, mean_base)
         )
y2020 <- filter(proj_cntyb, YEAR == 2020)
y2100 <- filter(proj_cntyb, YEAR == 2100)
# 
scalelabs <- c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39",
               "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", "85+")

ggplot(proj_cntyb,aes(x= AGE, y = mean_mig)) +
  geom_col(data=y2100, aes(fill="2100"), color="black") +
  geom_segment(data=y2100,
               aes(x=AGE,
                   xend=AGE,
                   y=0,
                   yend=mean_base,
                   color = "Mean_base")) +
  scale_color_manual(name = "", values = c("Mean_base" = "red")) +
  scale_fill_manual(name = "", values = c("2100" = NA)) +
  geom_point(data=y2100, aes(y=mean_base), color = "red") +
  theme_bw() +
  theme(legend.position = c(0.9, 0.3),
        legend.background = element_rect(fill=alpha('white', 0)))+
  # scale_y_continuous(limits = c(-150000, 150000),
  #                    breaks = seq(-150000, 150000, 50000),
  #                    # labels = paste0(as.character(c(seq(15, 0, -5), seq(5, 15, 5))), "m")
  #                    ) +
  scale_x_continuous(breaks = seq(1,18,1),
                     sec.axis =dup_axis(),
                     labels = paste0(scalelabs),
                     expand = c(0,0.1)) +
  coord_flip() +
  # annotate("text", x=1, y =100000, label ="Female") +
  # annotate("text", x=1, y =-100000, label ="Male") +
  labs(y = "Population",
       x = "")

#### RUN TO THIS POINT ONLY
# 
# rm()
#   
#   
#   data_tablem <- as.data.frame.table(get(paste0("proj",i,"m")))
#   data_tablem$YEAR <- launch_year + (i*5)
#   data_tablem$SEX <- "1"
#   data_tablef <- as.data.frame.table(get(paste0("proj",i,"f")))
#   data_tablef$YEAR <- launch_year+ (i*5)
#   data_tablef$SEX <- "2"
#   projm <- rbind(projm, data_tablem)
#   projf <-rbind(projf, data_tablef)
#   namm<- get(paste0("proj",i,"m"))
#   rm(data_tablem)
# }
# 
# 
# project = function(x){
#   tryCatch({#print(x)
#     ###   Prediction of the CCR function
#     predccr = function(ccr, sex, x, DF){
#       y <- as_data_frame(DF[[as.character(ccr)]][which(DF$COUNTYRACE== x & DF$SEX == sex )])
#       num<- seq(1,FORLEN,5)
#       # pred<- tryCatch(predict(ucm(value~0, data = y, level = TRUE, slope = FALSE)$model, n.ahead = FORLEN)[c(num),]
#       #                 , error=function(e) array(0, c(STEPS)))
#       pred<- tryCatch(forecast(arima(y$value, order = arima_order), h= FORLEN)$mean[c(num)]
#                       , error=function(e) array(0, c(STEPS)))
#       return(pred)
#     }
# 
#     ###################
#     ### DATA PREP
#     ##################
#     ### Filtering the Census data based on the county/race combination
#     K05 <- K05_pop[which(K05_pop$COUNTYRACE %in% x),] %>%
#       group_by(YEAR,  STATE, COUNTY, RACE, SEX, AGE, COUNTYRACE) %>%
#       dplyr::summarise(POPULATION = sum(POPULATION)) %>%
#       ungroup()
#    
#     ### Calculating the cohort-change differences (CCDs)
#     CCDs<- K05 %>%
#       ungroup() %>%
#       group_by(COUNTYRACE) %>%
#       mutate(AGE = paste0("X", str_pad(AGE, 2, pad ="0")),
#              GEOID = paste0(STATE, COUNTY),
#              POPULATION = as.numeric(POPULATION)) %>%
#       spread(AGE, POPULATION)
#     if(is.null(CCDs$X01)){CCDs$X01=0}else{CCDs$X01=CCDs$X01}
#     if(is.null(CCDs$X02)){CCDs$X02=0}else{CCDs$X02=CCDs$X02}
#     if(is.null(CCDs$X03)){CCDs$X03=0}else{CCDs$X03=CCDs$X03}
#     if(is.null(CCDs$X04)){CCDs$X04=0}else{CCDs$X04=CCDs$X04}
#     if(is.null(CCDs$X05)){CCDs$X05=0}else{CCDs$X05=CCDs$X05}
#     if(is.null(CCDs$X06)){CCDs$X06=0}else{CCDs$X06=CCDs$X06}
#     if(is.null(CCDs$X07)){CCDs$X07=0}else{CCDs$X07=CCDs$X07}
#     if(is.null(CCDs$X08)){CCDs$X08=0}else{CCDs$X08=CCDs$X08}
#     if(is.null(CCDs$X09)){CCDs$X09=0}else{CCDs$X09=CCDs$X09}
#     if(is.null(CCDs$X10)){CCDs$X10=0}else{CCDs$X10=CCDs$X10}
#     if(is.null(CCDs$X11)){CCDs$X11=0}else{CCDs$X11=CCDs$X11}
#     if(is.null(CCDs$X12)){CCDs$X12=0}else{CCDs$X12=CCDs$X12}
#     if(is.null(CCDs$X13)){CCDs$X13=0}else{CCDs$X13=CCDs$X13}
#     if(is.null(CCDs$X14)){CCDs$X14=0}else{CCDs$X14=CCDs$X14}
#     if(is.null(CCDs$X15)){CCDs$X15=0}else{CCDs$X15=CCDs$X15}
#     if(is.null(CCDs$X16)){CCDs$X16=0}else{CCDs$X16=CCDs$X16}
#     if(is.null(CCDs$X17)){CCDs$X17=0}else{CCDs$X17=CCDs$X17}
#     if(is.null(CCDs$X18)){CCDs$X18=0}else{CCDs$X18=CCDs$X18}
#     CCDs<- CCDs %>%
#       arrange(GEOID, SEX, YEAR) %>%
#       mutate(ccr1 = X02 - lag(X01, 5),
#              ccr2 = X03 - lag(X02, 5),
#              ccr3 = X04 - lag(X03, 5),
#              ccr4 = X05 - lag(X04, 5),
#              ccr5 = X06 - lag(X05, 5),
#              ccr6 = X07 - lag(X06, 5),
#              ccr7 = X08 - lag(X07, 5),
#              ccr8 = X09 - lag(X08, 5),
#              ccr9 = X10 - lag(X09, 5),
#              ccr10 = X11 - lag(X10, 5),
#              ccr11 = X12 - lag(X11, 5),
#              ccr12 = X13 - lag(X12, 5),
#              ccr13 = X14 - lag(X13, 5),
#              ccr14 = X15 - lag(X14, 5),
#              ccr15 = X16 - lag(X15, 5),
#              ccr16 = X17 - lag(X16, 5),
#              ccr17 = X18 - (lag(X17, 5) + lag(X18, 5))) %>%
#       filter(YEAR >= min(YEAR +5, na.rm=T) & YEAR <= test_year)
#     
#     ### Calculating the CCRs
#     CCRs<- K05 %>%
#       ungroup() %>%
#       mutate(AGE = paste0("X", str_pad(AGE, 2, pad ="0")),
#              GEOID = paste0(STATE, COUNTY),
#              POPULATION = as.numeric(POPULATION)) %>%
#       spread(AGE, POPULATION)
#     if(is.null(CCRs$X01)){CCRs$X01=0}else{CCRs$X01=CCRs$X01}
#     if(is.null(CCRs$X02)){CCRs$X02=0}else{CCRs$X02=CCRs$X02}
#     if(is.null(CCRs$X03)){CCRs$X03=0}else{CCRs$X03=CCRs$X03}
#     if(is.null(CCRs$X04)){CCRs$X04=0}else{CCRs$X04=CCRs$X04}
#     if(is.null(CCRs$X05)){CCRs$X05=0}else{CCRs$X05=CCRs$X05}
#     if(is.null(CCRs$X06)){CCRs$X06=0}else{CCRs$X06=CCRs$X06}
#     if(is.null(CCRs$X07)){CCRs$X07=0}else{CCRs$X07=CCRs$X07}
#     if(is.null(CCRs$X08)){CCRs$X08=0}else{CCRs$X08=CCRs$X08}
#     if(is.null(CCRs$X09)){CCRs$X09=0}else{CCRs$X09=CCRs$X09}
#     if(is.null(CCRs$X10)){CCRs$X10=0}else{CCRs$X10=CCRs$X10}
#     if(is.null(CCRs$X11)){CCRs$X11=0}else{CCRs$X11=CCRs$X11}
#     if(is.null(CCRs$X12)){CCRs$X12=0}else{CCRs$X12=CCRs$X12}
#     if(is.null(CCRs$X13)){CCRs$X13=0}else{CCRs$X13=CCRs$X13}
#     if(is.null(CCRs$X14)){CCRs$X14=0}else{CCRs$X14=CCRs$X14}
#     if(is.null(CCRs$X15)){CCRs$X15=0}else{CCRs$X15=CCRs$X15}
#     if(is.null(CCRs$X16)){CCRs$X16=0}else{CCRs$X16=CCRs$X16}
#     if(is.null(CCRs$X17)){CCRs$X17=0}else{CCRs$X17=CCRs$X17}
#     if(is.null(CCRs$X18)){CCRs$X18=0}else{CCRs$X18=CCRs$X18}
#     CCRs<- CCRs %>%
#       arrange(GEOID, SEX, YEAR) %>%
#       mutate(ccr1 = X02 / lag(X01, 5),
#              ccr2 = X03 / lag(X02, 5),
#              ccr3 = X04 / lag(X03, 5),
#              ccr4 = X05 / lag(X04, 5),
#              ccr5 = X06 / lag(X05, 5),
#              ccr6 = X07 / lag(X06, 5),
#              ccr7 = X08 / lag(X07, 5),
#              ccr8 = X09 / lag(X08, 5),
#              ccr9 = X10 / lag(X09, 5),
#              ccr10 = X11 / lag(X10, 5),
#              ccr11 = X12 / lag(X11, 5),
#              ccr12 = X13 / lag(X12, 5),
#              ccr13 = X14 / lag(X13, 5),
#              ccr14 = X15 / lag(X14, 5),
#              ccr15 = X16 / lag(X15, 5),
#              ccr16 = X17 / lag(X16, 5),
#              ccr17 = X18 / (lag(X17, 5) + lag(X18, 5))) %>%
#       filter(YEAR >= min(YEAR +5, na.rm=T) & YEAR <= test_year)
# 
#     CCRs[mapply(is.infinite, CCRs)] <- NA
#     CCRs[mapply(is.nan, CCRs)] <- NA
#     CCRs[is.na(CCRs)] <-0
#     CCDs[mapply(is.infinite, CCDs)] <- NA
#     CCDs[mapply(is.nan, CCDs)] <- NA
#     CCDs[is.na(CCDs)] <-0
#     ##################################################
#     ### Start of the Additive projections
#     ##################################################
#     
#     ###  Calculating the UCM's of the CCD's for each age/sex group. The confidence interval is set to 80% (1.28*SD) 
#     for (i in 1:(SIZE-1)){
#       data_tablef <- cbind(predccr(paste0("ccr",i), "2", x, CCDs),0,0)
#       data_tablem <- cbind(predccr(paste0("ccr",i), "1", x, CCDs),0,0)
#       errf <- sd(CCDs[[as.character(paste0("ccr",i))]][which(CCDs$SEX == "2")])*1.28
#       errm <- sd(CCDs[[as.character(paste0("ccr",i))]][which(CCDs$SEX == "1")])*1.28
#       data_tablef[,2]<- data_tablef[,1]- ifelse(is.na(errf),0, errf)
#       data_tablef[,3]<- data_tablef[,1]+ ifelse(is.na(errf),0, errf)
#       data_tablem[,2]<- data_tablem[,1]- ifelse(is.na(errm),0, errm)
#       data_tablem[,3]<- data_tablem[,1]+ ifelse(is.na(errm),0, errm)
#       assign(paste0("BA",i,"f"), data_tablef[1:STEPS,1:3])
#       assign(paste0("BA",i,"m"), data_tablem[1:STEPS,1:3])
#       rm(data_tablef, data_tablem, errf, errm)
#     }
#     ### "Stacking" the CCDs into a single vector with a high/medium/low  
#     for (i in 1:STEPS){
#       namm<-paste0("lx", i, "m")
#       namf<-paste0("lx",i,"f")
#       assign(namm, rbind(BA1m[i,],BA2m[i,], BA3m[i,], BA4m[i,], BA5m[i,], BA6m[i,], BA7m[i,], BA8m[i,], BA9m[i,], BA10m[i,]
#                          , BA11m[i,], BA12m[i,], BA13m[i,], BA14m[i,], BA15m[i,], BA16m[i,], BA17m[i,]))
#       assign(namf, rbind(BA1f[i,],BA2f[i,], BA3f[i,], BA4f[i,], BA5f[i,], BA6f[i,], BA7f[i,], BA8f[i,], BA9f[i,], BA10f[i,]
#                          , BA11f[i,], BA12f[i,], BA13f[i,], BA14f[i,], BA15f[i,], BA16f[i,], BA17f[i,]))}
#     ###   Placing the CCD's into the subdiagonal of a leslie matrix.
#     for (i in 1:STEPS){
#       data_tablef <- get(paste0("lx",i,"f"))
#       weird_dataf <- array(0,c(SIZE,SIZE,ncol(data_tablef)))
#       data_tablem <- get(paste0("lx",i,"m"))
#       weird_datam <- array(0,c(SIZE,SIZE,ncol(data_tablem)))
#       for(j in 1:ncol(data_tablef)){
#         weird_dataf[,,j] <- rbind(0,cbind(diag(data_tablef[,j]),0))
#         weird_datam[,,j] <- rbind(0,cbind(diag(data_tablem[,j]),0))
#         assign(paste0("S",i,"m"), weird_datam)
#         assign(paste0("S",i,"f"), weird_dataf)
#       }
#       rm(data_tablef)
#       rm(weird_dataf)
#     }
#     ### Formatting the base POPULATION data as equal to the total POPULATION minus the group quaters.
#     popf <- array(0, c(SIZE))
#     for(i in 1:SIZE){    popf[i] <- ifelse(length(K05$POPULATION[which(K05$SEX == "2" & K05$YEAR == launch_year & K05$AGE == i)])==0,
#                                            0,
#                                            K05$POPULATION[which(K05$SEX == "2" & K05$YEAR == launch_year & K05$AGE == i)])}
#     # gqf <-  if (length(gqpop$group_quarters[which(gqpop$SEX == "2" & gqpop$COUNTYRACE == x)]) > 0){
#     #   gqpop$group_quarters[which(gqpop$SEX == "2" & gqpop$COUNTYRACE == x)]} else {
#     #     0}
#     # popf <- popf - gqf
#     
#     popm <- array(0, c(SIZE))
#     for(i in 1:SIZE){popm[i] <- ifelse(length(K05$POPULATION[which(K05$SEX == "1" & K05$YEAR == launch_year & K05$AGE == i)])==0,
#                                        0,
#                                        K05$POPULATION[which(K05$SEX == "1" & K05$YEAR == launch_year & K05$AGE == i)])}
#     # gqm <- if (length(gqpop$group_quarters[which(gqpop$SEX == "1" & gqpop$COUNTYRACE == x)]) > 0){
#     #   gqpop$group_quarters[which(gqpop$SEX == "1" & gqpop$COUNTYRACE == x)]} else {
#     #     0}
#     # popm <- popm - gqm 
#     p0f <-array(0,c(SIZE,SIZE,ncol(lx1f)))
#     p0m <-array(0,c(SIZE,SIZE,ncol(lx1f)))
#     for (i in 1:ncol(lx1f)){
#       p0f[,,i] <-  rbind(0,cbind(diag(popf),0))[1:18,1:18]
#       p0f[18,18,i] = popf[18]
#       p0m[,,i] <-  rbind(0,cbind(diag(popm),0))[1:18,1:18]
#       p0m[18,18,i] = popm[18]
#     }  
#     ### Calculating the forecasted CWR's from the UCMs. Confidence interval is set at 80% (1.28*SD) 
#     n02 <- filter(stateferts) %>%
#       dplyr::select(value)
#     n01 <- array(0,c(STEPS,ncol(lx1f)))
#     n01[,1] <-n02$value[1:STEPS]
#     
#     ### PROJECTION ITSELF ###
#     
#     # Actually projecting with the additive model
#     for (i in 1:STEPS){
#       data_tablef <- get(paste0("S",i,"f"))
#       data_tablem <- get(paste0("S",i,"m"))
#       projm<-projf <- array(0,c(SIZE,ncol(lx1f)), dimnames = list(
#         c("a1", "a2", "a3", "a4", "a5","a6", "a7", "a8", "a9", "a10", "a11", "a12", "a13", "a14", "a15", "a16", "a17", "a18")))
#       popdatf<- get(paste0("p",i-1,"f"))
#       popdatm<- get(paste0("p",i-1,"m"))
#       for(j in 1:ncol(lx1f)){  
#         projf[,j] <- rowSums(data_tablef[,,j] + popdatf[,,j])
#         projm[,j] <- rowSums(data_tablem[,,j] + popdatm[,,j])
#         projf[1,j] <- (n01[i,j] * sum(projf[4:10,j]))*.487
#         projm[1,j] <- (n01[i,j] * sum(projf[4:10,j]))*.512
#         popdatf[,,j] <-rbind(0,cbind(diag(projf[,j]),0))[1:18,1:18]
#         popdatm[,,j] <-rbind(0,cbind(diag(projm[,j]),0))[1:18,1:18]
#         popdatf[18,18,j] <- projf[18,j]
#         popdatm[18,18,j] <- projm[18,j]
#         assign(paste0("p",i,"f"), popdatf)
#         assign(paste0("p",i,"m"), popdatm)
#         assign(paste0("proj",i,"f"),projf)
#         assign(paste0("proj",i,"m"), projm)
#       }
#       rm(data_tablef, data_tablem, projf, projm, popdatf, popdatm)
#     }
#     ### Collecting the additive projections together.
#     projm<-NULL
#     projf<-NULL
#     for (i in 1:STEPS){
#       data_tablem <- as.data.frame.table(get(paste0("proj",i,"m")))
#       data_tablem$YEAR <- launch_year+ (i*5)
#       data_tablem$SEX <- "1"
#       data_tablef <- as.data.frame.table(get(paste0("proj",i,"f")))
#       data_tablef$YEAR <- launch_year+ (i*5)
#       data_tablef$SEX <- "2"
#       projm <- rbind(projm, data_tablem)
#       projf <-rbind(projf, data_tablef)
#       namm<- get(paste0("proj",i,"m"))
#       rm(data_tablem)
#     }
#     ### Declaring several variables
#     projadd <-rbind(projm, projf) %>%
#       dplyr::rename(Scenario = Var2)
#     projadd$COUNTYRACE <-x
#     projadd$TYPE<- "ADD"
#     
#     ######################################
#     ### PROJECTING THE CCRs
#     
#     ### Calculating the CCR UCMs for each individual age group
#     for (i in 1:(SIZE-1)){
#       data_tablef <- cbind(predccr(paste0("ccr",i), "2", x, CCRs),0,0)
#       data_tablem <- cbind(predccr(paste0("ccr",i), "1", x, CCRs),0,0)
#       errf <- sd(CCRs[[as.character(paste0("ccr",i))]][which(CCRs$SEX == "2")])*1.28
#       errm <- sd(CCRs[[as.character(paste0("ccr",i))]][which(CCRs$SEX == "1")])*1.28
#       data_tablef[,2]<- data_tablef[,1]- ifelse(is.na(errf),0, errf)
#       data_tablef[,3]<- data_tablef[,1]+ ifelse(is.na(errf),0, errf)
#       data_tablem[,2]<- data_tablem[,1]- ifelse(is.na(errm),0, errm)
#       data_tablem[,3]<- data_tablem[,1]+ ifelse(is.na(errm),0, errm)
#       assign(paste0("BA",i,"f"), data_tablef[1:STEPS,1:3])
#       assign(paste0("BA",i,"m"), data_tablem[1:STEPS,1:3])
#       rm(data_tablef, data_tablem, errf, errm)
#     }
#     ### Stacking the forecasted CCRs into single vectors.
#     for (i in 1:STEPS){
#       namm<-paste0("lx", i, "m")
#       namf<-paste0("lx",i,"f")
#       assign(namm, rbind(BA1m[i,],BA2m[i,], BA3m[i,], BA4m[i,], BA5m[i,], BA6m[i,], BA7m[i,], BA8m[i,], BA9m[i,], BA10m[i,]
#                          , BA11m[i,], BA12m[i,], BA13m[i,], BA14m[i,], BA15m[i,], BA16m[i,], BA17m[i,]))
#       assign(namf, rbind(BA1f[i,],BA2f[i,], BA3f[i,], BA4f[i,], BA5f[i,], BA6f[i,], BA7f[i,], BA8f[i,], BA9f[i,], BA10f[i,]
#                          , BA11f[i,], BA12f[i,], BA13f[i,], BA14f[i,], BA15f[i,], BA16f[i,], BA17f[i,]))}
#     ### Setting the sub-diagonal of a leslie matrix as equal to the projected CCRs
#     for (i in 1:STEPS){
#       data_tablef <- get(paste0("lx",i,"f"))
#       data_tablem <- get(paste0("lx",i,"m"))
#       weird_dataf <- array(0,c(SIZE,SIZE,ncol(data_tablef)))
#       weird_datam <- array(0,c(SIZE,SIZE,ncol(data_tablem)))
#       for(j in 1:ncol(data_tablef)){  
#         weird_dataf[,,j] <- rbind(0,cbind(diag(data_tablef[,j]),0))
#         weird_dataf[18,18,j]=data_tablef[17,j]
#         weird_datam[,,j] <- rbind(0,cbind(diag(data_tablem[,j]),0))
#         weird_datam[18,18,j]=data_tablem[17,j]
#         assign(paste0("S",i,"f"), weird_dataf)
#         assign(paste0("S",i,"m"), weird_datam)
#       }
#       rm(data_tablef, data_tablem, weird_dataf, weird_datam)
#     }
#     ### Formatting the base POPULATION data.
#     p0f <-array(0,c(SIZE,1,ncol(lx1f)))
#     p0m <-array(0,c(SIZE,1,ncol(lx1f)))
#     for (i in 1:ncol(lx1f)){
#       p0f[,,i] <-  cbind(popf)
#       p0m[,,i] <-  cbind(popm)
#       
#     }  
#     ### PROJECTING THE CCRs
#     for (i in 1:STEPS){
#       data_tablef <- get(paste0("S",i,"f"))
#       data_tablem <- get(paste0("S",i,"m"))
#       projm<-projf <- array(0,c(SIZE,1,ncol(lx1f)), dimnames = list(
#         c("a1", "a2", "a3", "a4", "a5","a6", "a7", "a8", "a9", "a10", "a11", "a12", "a13", "a14", "a15", "a16", "a17", "a18")))
#       popdatf<- get(paste0("p",i-1,"f"))
#       popdatm<- get(paste0("p",i-1,"m"))
#       for(j in 1:ncol(lx1f)){  
#         projf[,,j] <- data_tablef[,,j] %*% popdatf[,,j]
#         projm[,,j] <- data_tablem[,,j] %*%  popdatm[,,j]
#         projf[1,,j] <- (n01[i,j] * sum(projf[4:10,,j]))*.487
#         projm[1,,j] <- (n01[i,j] * sum(projf[4:10,,j]))*.512
#         assign(paste0("p",i,"f"), projf)
#         assign(paste0("p",i,"m"), projm)
#         assign(paste0("proj",i,"f"), projf)
#         assign(paste0("proj",i,"m"), projm)
#       }
#     }
#     ### Collecting the projection results
#     projm<-NULL
#     projf<-NULL
#     for (i in 1:STEPS){
#       data_tablem <- as.data.frame.table(get(paste0("proj",i,"m")))
#       data_tablem$YEAR <- launch_year+ (i*5)
#       data_tablem$SEX <- "1"
#       data_tablef <- as.data.frame.table(get(paste0("proj",i,"f")))
#       data_tablef$YEAR <- launch_year+ (i*5)
#       data_tablef$SEX <- "2"
#             projm <- rbind(projm, data_tablem)
#       projf <-rbind(projf, data_tablef)
#       namm<- get(paste0("proj",i,"m"))
#       rm(data_tablem)
#     }
#     
#     projmult <-rbind(projm, projf) %>%
#       dplyr::select(-Var2) %>%
#       dplyr::rename(Scenario = Var3)
#     projmult$COUNTYRACE <-x
#     projmult$TYPE<- "Mult"
#    
#     # Collecting all projections together
#     proj <-rbind(projadd, projmult) #%>%
#     
#     return(proj)
#   }
#   , error=function(e){cat(x," ERROR :",conditionMessage(e), "\n")})
# }
# 
# # for(this.state in stateid){
# #   x = unlist(list(unique(K05_pop$COUNTYRACE[which(K05_pop$STATE==this.state)])))
# #   KT = rbindlist(pbmclapply(x, project, mc.cores = detectCores()-1))
# #   KT2 <- KT %>%
# #     mutate(AGE = as.numeric(substr(Var1, 2,3))) %>%
# #     group_by(YEAR, COUNTYRACE, SEX, AGE) %>%
# #     spread(Scenario, Freq)
# #   write.table(KT2, paste0("PROJECTIONS/PROJ/COUNTY_20152100_",this.state,".csv"))
# # }
# 
# KT = rbindlist(lapply(x, project))
# 
# z <- KT %>%
#   filter(TYPE == "Mult") %>%
#   mutate(AGE = as.numeric(substr(Var1, 2,3))) %>%
#   group_by(YEAR, COUNTYRACE, SEX, AGE) %>%
#   spread(Scenario, Freq) %>%
#   mutate(STATE= substr(COUNTYRACE, 1,2),
#          COUNTY = "000",
#          GEOID = paste0(STATE, COUNTY),
#          A = as.numeric(A),
#          B = as.numeric(B),
#          C = as.numeric(C),
#          A = if_else(A<0, 0, A),
#          B = if_else(B<0, 0, B),
#          C = if_else(C<0,0, C),
#          RACE = 0)
# 
# z[is.na(z)] <-0
# basesum <-  Klaunch[which( Klaunch$YEAR == launch_year),] %>%
#   mutate(STATE = as.character(STATE)) %>%
#   dplyr::select(STATE, COUNTY, POPULATION, RACE)
# 
# addsum <- z[which(z$TYPE=="ADD" & z$YEAR == (launch_year+FORLEN)),] %>%
#   group_by(STATE, COUNTY, GEOID, RACE, TYPE) %>%
#   dplyr::summarise(A = sum(A))
# 
# addmult <- left_join(addsum, basesum) %>%
#   mutate(COMBINED = if_else(A>= POPULATION, "ADD" ,"Mult")) %>%
#   dplyr::select(STATE, COUNTY, GEOID, RACE, COMBINED)
# 
# 
# basesum2 <-  Klaunch[which( Klaunch$YEAR == launch_year),] %>%
#   dplyr::select(STATE, COUNTY, POPULATION, RACE) %>%
#   group_by(RACE) %>%
#   dplyr::summarise(poptot = sum(POPULATION))
# 
# combined<- left_join(z, addmult) %>%
#   filter(TYPE == COMBINED) %>%
#   mutate(TYPE = "ADDMULT") %>%
#   dplyr::select(-COMBINED)
# 
# z2<- rbind(z, combined) %>%
#   dplyr::select(-V1)
# z2<-  left_join(as.data.frame(z2), as.data.frame(K05_launch2))
# z2<- left_join(z2, countynames)
# z2[is.na(z2)] <-0
# z2<- filter(z2,
#             !GEOID %in% c("02900", "04910", "15900", "35910", "36910", "51910", "51911","51911", "51913", "51914", "51915", "51916", "51918"))
# z3 <- filter(z2,
#              TYPE == "ADDMULT")
# 
# totals <- z %>%
#   mutate(agegroup = case_when(
#     AGE <= 3 ~ "Young",
#     AGE >= 14 ~ "Old",
#     TRUE ~ "Working Age"
#   )) %>%
#   group_by(YEAR, agegroup) %>%
#   dplyr::summarise(poptot = sum(A)) %>%
#   spread(agegroup, poptot)
# totals2 <- z %>%
#   group_by(YEAR) %>%
#   dplyr::summarise(poptot = sum(A)) %>%
#   left_join(., totals)
# totals2 <- left_join(z3, totals) %>%
#   mutate(percentage = (A/poptot))
# 
# 
# 
# 
# write_csv(KT2, paste0("R/PROJECTIONS/PR2017.csv"))
# 
# 
# pckgs <- c("data.table", "doParallel", "foreach", "tidyverse", "rucm", "forecast")
# (start.time <- Sys.time())
# 
# foreach(i = 1:length(stateid), 
#         .combine = rbind, 
#         .errorhandling = "stop", 
#         .packages = pckgs) %dopar% {
#           
#           x = unlist(list(unique(K05_pop$COUNTYRACE[which(K05_pop$STATE==stateid[i])])))
#           KT = rbindlist(lapply(x, project))
#           
#           KT2 <- KT %>%
#             mutate(AGE = as.numeric(substr(Var1, 2,3))) %>%
#             group_by(YEAR, COUNTYRACE, SEX, AGE) %>%
#             spread(Scenario, Freq)
#           
#           write.table(KT2, paste0("PROJECTIONS/PROJ/COUNTY_20152100_",stateid[i],".csv"))
#           
#         }

