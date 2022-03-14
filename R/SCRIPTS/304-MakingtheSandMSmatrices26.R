set.seed(100)

source('./R/SCRIPTS/000-Libraries.R')      # loading in the libraries
source('./R/SCRIPTS/001-fipscodes.R')
source('./R/SCRIPTS/003-proj_basedataload.R')

# source('./R/SCRIPTS/007-FormatSSPs.R')
Klaunch <- K05_pop[which(K05_pop$YEAR==2015),]

# Pulling in the fertility rate data. This is state-level.
stateferts <- read_csv("R/DATA-PROCESSED/state-level-fert-rates_20152100.csv")

samp <- unique(Klaunch$COUNTYRACE)
# samp <- c("12086_", "12095_", sample(samp, 1000))
# samp <- unique(K05_pop$COUNTYRACE[which(K05_pop$STATE == 15)])
# samp <- c("13121_", "12086_")
# samp <- "13121_"
# samp <- c("12011_", "12086_", "13121_")
# Making a list to loop over.
x = samp

toymodel <- substr(x, 1,5)

## Pulling in the reductions of CCRs by year.
reductions <- read_csv("./R/DATA-PROCESSED/atriskpops26.csv")
reductions <- reductions[which(reductions$GEOID %in% toymodel),] %>%
  # filter(GEOID %in% toymodel) %>%
  mutate(sex = as.character(sex)) %>%
  dplyr::select(GEOID, year, SEX=sex, ccr = groups, prob2, reduce_in, reduce_eae) %>%
  group_by(GEOID, prob2, SEX,ccr) %>%
  mutate(step = as.character(row_number()))
reductions$COUNTYRACE <- paste0(reductions$GEOID, "_", reductions$RACE)

## Will produce a data file containing the Unaffected CCRs and the reduced CCRs
type1 <- "reduce_in"
type2 = "mean_in"
BACCR <- read_rds("./R/DATA-PROCESSED/BACCR_2015.R")  %>%
  filter(COUNTYRACE %in% x) %>%
  arrange(COUNTYRACE, as.numeric(step), SEX, as.numeric(ccr))
BACCRreduce1 <- NULL
for(this.prob in c("p5", "p50", "p95")){

BACCRreduce <- left_join(BACCR, reductions[which(reductions$prob2==this.prob),]) %>%
  mutate(GEOID = substr(COUNTYRACE,1,5),
    reduce_eae = ifelse(ccr=="ccr18",lag(reduce_eae), reduce_eae) ,
         reduce_eae = ifelse(is.na(reduce_eae),1, reduce_eae),
         reduce_in = ifelse(ccr=="ccr18",lag(reduce_in), reduce_in),
         reduce_in = ifelse(is.na(reduce_in),1, reduce_in),
         mean_eae = reduce_eae * as.numeric(`mean`),
         lower_eae = reduce_eae * as.numeric(lower),
         upper_eae = reduce_eae * as.numeric(upper),
         mean_in = reduce_in * as.numeric(`mean`),
         lower_in = reduce_in * as.numeric(lower),
         upper_in = reduce_in * as.numeric(upper),
    AGE = as.numeric(substr(ccr,4,5))) %>%
  mutate(prob2 = this.prob)



### Calculating the forecasted CWR's from the UCMs. Confidence interval is set at 80% (1.28*SD) 
n02 <- filter(stateferts, STATE %in% unique(stateid)) %>%
  group_by(STATE) %>%
  mutate(STEP = dplyr::row_number(),
         STATE = as.character(STATE))
n02 <- left_join(K05_launch, n02) %>%
  dplyr::select(STATE, GEOID, `Point Forecast`:`Hi 80`, STEP) %>%
  filter(GEOID %in% substr(x, 1,5))%>%
  mutate(cwr = `Point Forecast`) %>%
  dplyr::select(GEOID, cwr, STEP)

### P_t
z <- K05_pop[which(K05_pop$COUNTYRACE %in% x & K05_pop$YEAR == launch_year),] %>%
  arrange(GEOID, SEX, AGE) %>%
  pivot_wider(names_from = AGE, values_from = POPULATION, values_fill = 0) %>%
  pivot_longer(cols = c(`1`:`18`), names_to = "AGE", values_to = "POPULATION") %>%
  mutate(AGE = as.numeric(AGE)) %>%
  arrange(GEOID, SEX, AGE)

z2 <- z %>%
  filter(AGE %in% c(4,5,6,7,8,9,10), # women ages 15-49
         #SEX == "2" 
         ) %>%
  full_join(., n02) %>%
  mutate(step = as.character(STEP),
         cwr = ifelse(SEX==1, cwr* 0.512, cwr *0.488))%>%
  ungroup() %>%
  dplyr::select(GEOID, AGE, step, SEX, cwr)

BACCRreduce <- left_join(BACCRreduce, z2) %>%
  mutate(cwr = ifelse(is.na(cwr),0,cwr))

BACCRreduce1 <- rbind(BACCRreduce1, BACCRreduce)
}

BACCRreduce1 <- BACCRreduce1 %>%
  arrange(COUNTYRACE, as.numeric(step), SEX, as.numeric(ccr))

rowws <- data.frame(GEOID = toymodel,
                        destination = toymodel) %>%
      mutate(identc = row_number(),
             identr = row_number())

tic() 

for (i in 1:STEPS){
  for(this.prob in c("p5", "p50", "p95")){
    
    ## Pulling inthe CCrs and generating the row/column location for all CCRs.
    ## This also makes sure the open ended interval is 'bent up'
    ccrs <- BACCRreduce1[which(BACCRreduce1$step==i &
                                 BACCRreduce1$prob2 == this.prob),] %>%
      # arrange(COUNTYRACE, as.numeric(step), SEX, as.numeric(ccr))
      group_by(GEOID) %>%
      mutate(AGE2 = row_number()) %>%
      left_join(.,rowws %>% dplyr::select(GEOID, identc)) %>%
      mutate(rowws = ifelse(AGE == 18, (identc-1)*36+AGE2,(identc-1)*36+AGE2+1),
             colss = (identc-1)*36+AGE2) %>%
      ungroup() 
    ## With the CCR locs, we just select those values.
    ccrs <- ccrs %>% dplyr::select(rowws, colss, val = mean)
    
    ## Doing the same thing with the CCRs but with the CWRs.
    cwrs <-   BACCRreduce1[which(BACCRreduce1$step==i &
                                   BACCRreduce1$prob2 == this.prob),]  %>%
      group_by(GEOID) %>%
      mutate(AGE2 = row_number()) %>%
      left_join(.,rowws %>% dplyr::select(GEOID, identc)) %>%
      ungroup() %>%
      mutate(colss = identc*18+AGE+(18*(identc-1)))
    cwrs <- cbind(cwrs, rowws=sort(rep(seq(1,nrow(cwrs), 18), 18)))
    
    cwrs <- cwrs %>% dplyr::select(rowws, colss, val = cwr) %>%
      filter(val >0)
    
    # 'Stacking' the CCR and CWR locs on top of each other.
    ccrs <- rbind(ccrs, cwrs)
    
    ## Generating a Sparse Matrix using the locations.
    z<-sparseMatrix(ccrs$rowws, 
                    ccrs$colss, 
                    x = as.numeric(ccrs$val))
    # write.excel(as.matrix(z[1:72,1:72])) ## function to check the output visually in excel.
    assign(paste0("S",i,this.prob), z)
    writeMM(get(paste0("S",i, this.prob)), file = paste0("./R/DATA-PROCESSED/MATRICES/S",i,this.prob,"26.mtx" ))
  }
}

## Pulling in the migration probabilities data.
migs <- read_csv("./R/DATA-PROCESSED/migrationprobs.csv") %>%
  mutate(origin = str_pad(origin,5, pad="0"),
         destination = str_pad(destination, 5, pad="0"))
migs <- migs[which(migs$origin %in% toymodel),]

migs$COUNTYRACE <- paste0(migs$origin, "_", migs$RACE)
migs <- as.data.frame(migs)

tic()
for (i in 1:STEPS){
  ## This generates an ID column for the rows/columsn of the leslie matrix.
#    for(this.prob in c("p5", "p50", "p95")){
#    
#     ## Pulling inthe CCrs and generating the row/column location for all CCRs.
#     ## This also makes sure the open ended interval is 'bent up'
#     ccrs <- BACCRreduce1[which(BACCRreduce1$step==i &
#                                  BACCRreduce1$prob2 == this.prob),] %>%
#       group_by(GEOID) %>%
#       mutate(AGE2 = row_number()) %>%
#       left_join(.,rowws %>% dplyr::select(GEOID, identc)) %>%
#       mutate(rowws = ifelse(AGE == 18, (identc-1)*36+AGE2,(identc-1)*36+AGE2+1),
#              colss = (identc-1)*36+AGE2) %>%
#       ungroup()
#     ## With the CCR locs, we just select those values.
#     ccrs <- ccrs %>% dplyr::select(rowws, colss, val = mean_in)
#     
#     ## Doing the same thing with the CCRs but with the CWRs.
#     cwrs <-   BACCRreduce1[which(BACCRreduce1$step==i &
#                                    BACCRreduce1$prob2 == this.prob),]  %>%
#       group_by(GEOID) %>%
#       mutate(AGE2 = row_number()) %>%
#       left_join(.,rowws %>% dplyr::select(GEOID, identc)) %>%
#       ungroup() %>%
#       mutate(colss = identc*18+AGE+(18*(identc-1)))
#     cwrs <- cbind(cwrs, rowws=sort(rep(seq(1,nrow(cwrs), 18), 18)))
#     
#     cwrs <- cwrs %>% dplyr::select(rowws, colss, val = cwr) %>%
#       filter(val >0)
#     
#     # 'Stacking' the CCR and CWR locs on top of each other.
#     ccrs <- rbind(ccrs, cwrs)
#   
#     ## Generating a Sparse Matrix using the locations.
#     z<-sparseMatrix(ccrs$rowws, 
#              ccrs$colss, 
#              x = as.numeric(ccrs$val))
#     assign(paste0("S",i,this.prob), z)
#     # writeMM(get(paste0("S",i, this.prob)), file = paste0("./R/DATA-PROCESSED/MATRICES/S",i,this.prob,".mtx" ))
# }
    # Getting the Migration data for the M matrix.
      migs2 <- migs[which(migs$step == i & migs$destination %in% substr(x,1,5)),] %>%
      # filter(#origin!=destination,
      #        destination %in% substr(x,1,5)) %>%
      dplyr::select(origin, destination, step, `Point Forecast`) %>%
      group_by(origin, destination) %>%
      pivot_wider(names_from = destination, values_from = `Point Forecast`, values_fill = 0) %>%
      pivot_longer(cols = c(3:ncol(.)), names_to = "destination", values_to = "freq") %>%
      mutate(freq = ifelse(origin==destination,0, freq))%>%
      dplyr::select(GEOID = origin, destination, freq)
    migs2 <- migs2[which(migs2$freq>=0),] %>%
      group_by(GEOID) %>%
      mutate(freq = `freq`/sum(`freq`))
    # if(length(setdiff(toymodel, unique(migs2$destination)))>0){
    #   missing <- data.frame(destination=  setdiff(toymodel, unique(migs2$destination)),
    #                         GEOID = substr(x[j],1,5),
    #                         freq = 0)
    #   migs2 <- rbind(migs2, missing)
    #   rm(missing)
    # }
    migs2$freq <- ifelse(migs2$GEOID==migs2$destination, 1, migs2$freq)
    migs2 <- migs2[which(migs2$freq>0),]
    migs3 <- migs2[which(migs2$GEOID %in% reductions$GEOID),]
    migs4 <-  migs2[which(!migs2$GEOID %in% reductions$GEOID &
                            migs2$GEOID == migs2$destination),]
    migs2 <- rbind(migs3, migs4)

    rowws <- data.frame(GEOID = toymodel,
                        destination = toymodel) %>%
      mutate(identc = row_number(),
             identr = row_number())
    for(this.prob in c("p5", "p50", "p95")){

    ccrs2 <-BACCRreduce1[which(BACCRreduce1$step==i &
                                 BACCRreduce1$prob2 == this.prob),] %>%
      dplyr::select(SEX, step, AGE, GEOID, COUNTYRACE, reduce_in) %>%
      left_join(., migs2) %>%
      mutate(migrate = (1-reduce_in)*freq,
             migrate = case_when(
               GEOID == destination ~ reduce_in,
               TRUE ~ migrate
             )) %>%
      arrange(GEOID, SEX, AGE, step) %>%
      group_by(GEOID, destination) %>%
      mutate(AGE2 = row_number()) %>%
      arrange(destination, SEX, AGE) %>%
      left_join(.,rowws %>% dplyr::select(GEOID, identc)) %>%
      left_join(., rowws %>% dplyr::select(destination, identr) ) %>%
      mutate(rowss = (identr-1)*36+AGE2,
             colss = (identc-1)*36+AGE2)
    
    migmatr<- sparseMatrix(ccrs2$rowss, 
                        ccrs2$colss, 
                        x = ccrs2$migrate)
    # write.excel(as.matrix(migmatr[1:72,1:72]))
   assign(paste0("S",i,this.prob), readMM(paste0("./R/DATA-PROCESSED/MATRICES/S",i,this.prob,"26.mtx")))
   assign(paste0("MS",i, this.prob),  migmatr %*% get(paste0("S",i,this.prob)))
   # assign(paste0("S",i,"p50"), readMM(paste0("./R/DATA-PROCESSED/MATRICES/S",i,"p50.mtx")))
   # assign(paste0("S",i,"p95"), readMM(paste0("./R/DATA-PROCESSED/MATRICES/S",i,"p95.mtx")))
    
  # assign(paste0("MS",i, "p5"),  migmatr %*% get(paste0("S",i,"p5")))
  # assign(paste0("MS",i, "p50"), migmatr %*% get(paste0("S",i,"p50")))
  # assign(paste0("MS",i, "p95"), migmatr %*% get(paste0("S",i,"p95")))
   # assign(paste0("MS",i, "p5"),  migmatr %*% get(paste0("S",i,"p5")))
   # assign(paste0("MS",i, "p50"), migmatr %*% get(paste0("S",i,"p50")))
   # assign(paste0("MS",i, "p95"), migmatr %*% get(paste0("S",i,"p95")))
  writeMM(get(paste0("MS",i,this.prob)),  file = paste0("./R/DATA-PROCESSED/MATRICES/MS",i,this.prob,"26.mtx" ))
  # writeMM(get(paste0("MS",i,"p50")), file = paste0("./R/DATA-PROCESSED/MATRICES/MS",i,"p50.mtx" ))
  # writeMM(get(paste0("MS",i,"p95")), file = paste0("./R/DATA-PROCESSED/MATRICES/MS",i,"p95.mtx" ))
    }
}
toc()