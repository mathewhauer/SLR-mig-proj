###################
### DATA PREP
##################
rm(list=ls())
source('./R/SCRIPTS/000-Libraries.R')      # loading in the libraries
source('./R/SCRIPTS/001-fipscodes.R')      # Getting a Fips List
source('./R/SCRIPTS/003-proj_basedataload.R')

K05_pop <- K05_pop %>%
  group_by(.dots = GROUPING) %>%
  dplyr::summarise(POPULATION = sum(POPULATION))
K05_pop$GEOID <- paste0(K05_pop$STATE, K05_pop$COUNTY)
K05_pop$COUNTYRACE <- paste0(K05_pop$GEOID, "_", K05_pop$RACE)

races <- unique(K05_pop$RACE)

fertrats_20152100<- data.frame()
for(this.state in stateid){
  # for(this.race in races){
    K05t <- K05_pop[which(K05_pop$STATE == this.state),] %>%
      group_by(YEAR, STATE, SEX, AGE) %>%
      dplyr::summarise(POPULATION = sum(POPULATION)) %>%
      ungroup()
    newbornst <- K05t %>%
      filter(AGE == 1) %>% # AGE 1 = newborns.
      group_by(STATE, YEAR)  %>%
      dplyr::summarise(Newborns = sum(POPULATION))
    childbearingt <- K05t %>%
      filter(AGE %in% c(4,5,6,7,8,9,10), # women ages 15-49
             SEX == "2" ) %>%
      group_by(STATE, YEAR) %>%
      dplyr::summarise(Women1550 = sum(POPULATION)) %>%
      left_join(., newbornst) %>%
      mutate(fertrat = Newborns/Women1550) %>%
      filter(YEAR <= test_year)
    childbearingt$SEX <- "2"
    childbearingt[mapply(is.infinite, childbearingt)] <- NA
    childbearingt[mapply(is.nan, childbearingt)] <- NA
    childbearingt[is.na(childbearingt)] <-0
    num <- seq(1,FORLEN,5)
    # predcwr = function(ccr, sex, x, DF){
    #   hyndtran = function(ccr,DF){log((ccr - a) / (b - ccr))}
    #   b <- max(DF[[as.character(ccr)]][which(DF$RACE== x)])*1.01
    #   a <- -0.00000001
    #   y <-as_data_frame(hyndtran(DF[[as.character(ccr)]][which(DF$STATE== x & DF$SEX == sex & DF$RACE == this.race)]))
    # 
    #   num <- seq(1,FORLEN,5)
    #   pred<- tryCatch(round(predict(ucm(value~0, data = y, level = TRUE, slope = FALSE)$model, n.ahead = FORLEN)[c(num),],5)
    #                   , error=function(e) array(hyndtran(DF$fertrat[which.max(DF$YEAR)]), c(STEPS)))
    #   pred2 <-(b-a)*exp(pred)/(1+exp(pred))+a
    #   return(round(pred2,6))#
    # }
    forecasst <- as_data_frame(forecast(arima(childbearingt$fertrat, order = arima_order), h= FORLEN))[c(num),c(1:3)]
    fertrats_20152100<-rbind(fertrats_20152100,forecasst %>%
                   mutate(STATE = this.state,
                      SEX = 2))
  }

write_csv(fertrats_20152100, "R/DATA-PROCESSED/state-level-fert-rates_20152100.csv")