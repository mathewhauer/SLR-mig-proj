set.seed(100)

source('./R/SCRIPTS/000-Libraries.R')      # loading in the libraries
source('./R/SCRIPTS/001-fipscodes.R')
source('./R/SCRIPTS/003-proj_basedataload.R')

# Klaunch <- K05_pop[which(K05_pop$YEAR==2019),]
samp <- unique(K05_launch$COUNTYRACE)
x = samp

toymodel <- substr(x, 1,5)

K05 <- K05_pop[which(K05_pop$COUNTYRACE %in% x),] %>%
  group_by(YEAR,  STATE, COUNTY, SEX, AGE, COUNTYRACE) %>%
  dplyr::summarise(POPULATION = sum(POPULATION)) %>%
  ungroup()

CCRs<- K05_pop %>%
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
  group_by(GEOID) %>%
  filter(YEAR >= min(YEAR +5, na.rm=T) & YEAR <= test_year)

CCRs[mapply(is.infinite, CCRs)] <- NA
CCRs[mapply(is.nan, CCRs)] <- NA
CCRs[is.na(CCRs)] <-0

CCRs <- CCRs %>%
  group_by(GEOID, SEX) %>%
  mutate(ccr1 = rollapply(ccr1, 5, mean, align='right', fill=NA),
         ccr2 = rollapply(ccr2, 5, mean, align='right', fill=NA),
         ccr3 = rollapply(ccr3, 5, mean, align='right', fill=NA),
         ccr4 = rollapply(ccr4, 5, mean, align='right', fill=NA),
         ccr5 = rollapply(ccr5, 5, mean, align='right', fill=NA),
         ccr6 = rollapply(ccr6, 5, mean, align='right', fill=NA),
         ccr7 = rollapply(ccr7, 5, mean, align='right', fill=NA),
         ccr8 = rollapply(ccr8, 5, mean, align='right', fill=NA),
         ccr9 = rollapply(ccr9, 5, mean, align='right', fill=NA),
         ccr10 = rollapply(ccr10, 5, mean, align='right', fill=NA),
         ccr11 = rollapply(ccr11, 5, mean, align='right', fill=NA),
         ccr12 = rollapply(ccr12, 5, mean, align='right', fill=NA),
         ccr13 = rollapply(ccr13, 5, mean, align='right', fill=NA),
         ccr14 = rollapply(ccr14, 5, mean, align='right', fill=NA),
         ccr15 = rollapply(ccr15, 5, mean, align='right', fill=NA),
         ccr16 = rollapply(ccr16, 5, mean, align='right', fill=NA),
         ccr17 = rollapply(ccr17, 5, mean, align='right', fill=NA),
         ccr18 = rollapply(ccr18, 5, mean, align='right', fill=NA),
         )

a <- CCRs2 %>% dplyr::select(-X01:-X18)
# A function to project the CCRs/CCDs
predccr = function(ccr, sex, x, DF){

  y <- as_data_frame(DF[[as.character(ccr)]][which(DF$COUNTYRACE == x & DF$SEX == sex )])
  # y <- as_data_frame(CCRs[[as.character("ccr1")]][which(CCRs$COUNTYRACE == x2[j] & CCRs$SEX == "2" )])
  
  num<- seq(1,FORLEN,5)
  fore<- tryCatch(forecast(arima(y$value, order = arima_order), h= FORLEN)
                  , error=function(e) data.table(array(data= 0, 
                                                       dim = c(FORLEN,3),
                                                       dimnames = list(seq(1,FORLEN,1),
                                                                       c("mean", "lower", "upper")))))
  
  mean <- fore$mean[c(num)]
  lower <- fore$lower[c(num)]
  upper <- fore$upper[c(num)]
  pred<- (t(rbind(mean, lower, upper)))
  
  return(pred)
}

# # Creating a blank dataframe. This will hold the forecasted CCRs/CCDs
BACCR <- data.table()
BACCRl <- list()
x2 <-setdiff(x, unique(BACCR$COUNTYRACE))



# for(j in 1:length(x2)){
#   print(x2[j])

projectccrs <- function(j){
  # print(x2[j])
  print(j) 
  BACCRl <- list()
  state <- substr(j,1,2)
  for (i in 1:(SIZE)){
   
      data_tablef <- as.data.table(cbind(ccr=paste0("ccr",i),
                                         COUNTYRACE=paste0(j),
                                         STATE = state,
                                         SEX = "2",
                                         step = as.numeric(1:STEPS),
                                         predccr(paste0("ccr",i), "2", j, CCRs)
                                         ))
      data_tablem <- as.data.table(cbind(ccr=paste0("ccr",i),
                                         COUNTYRACE=paste0(j),
                                         STATE = state,
                                         SEX = "1",
                                         step = as.numeric(1:STEPS),
                                         predccr(paste0("ccr",i), "1", j, CCRs)))
      BACCRl[[i]] <- rbind( data_tablef, data_tablem)
      # return(BACCRl)
      # BACCR <- rbind(BACCR, data_tablef, data_tablem)
      # rm(data_tablef, data_tablem)
  }
  return(rbindlist(BACCRl))
}


z <- lapply(x2, projectccrs)
BACCR <- rbindlist(z)
# # Sorting the output.
BACCR2 <- as.data.frame(BACCR %>% arrange(COUNTYRACE, as.numeric(step), SEX, as.numeric(ccr)))
# BACCR <- read_
write_rds(BACCR2, "./R/DATA-PROCESSED/BACCR_2015.R")