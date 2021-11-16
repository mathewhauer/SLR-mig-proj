source("./R/SCRIPTS/000-Libraries.R")
source('./R/SCRIPTS/001-fipscodes.R')
source('./R/SCRIPTS/003-proj_basedataload.R')
# Reading in the migration data
mig <- read.table("./R/DATA-RAW/IRSmig/county_migration_data.txt", header=TRUE) %>%
  mutate(odpair = paste(origin, destination)) %>%
  filter(#origin %in% toymodel,
         origin != "99999",
         destination != "99999"
         # origin != destination
         )

num <- seq(1,FORLEN,5)

odpairs <- unique(mig$odpair)
 lam <- 0
forecasts <- function(this.od){
# for(this.od in unique(mig$odpair)){
# print(this.od)
 z<- mig[which(mig$odpair == this.od),] %>%
    gather(Year, mig, `X1990`:`X2017`) %>%
   mutate(mig = mig+1) %>%
    pull(mig) %>%
    as.ts() %>%
   BoxCox(., lambda =lam)
 pred <- as_data_frame(forecast(ets(z), h = FORLEN, lambda=lam))[c(num),c(1:3)]
 pred$odpair <- this.od
 return(pred)
 # forecasts <- rbind(forecasts, pred)
}
safeforecasts <- safely(.f = forecasts)
plan(multisession, workers = detectCores() - 1)
tic()
res <- future_map(odpairs, safeforecasts)
toc()

res2 <- rbindlist(res$result)

res2 <-res %>%
  map("result") %>%
  compact()

res2 <- rbindlist(res2)


res2 <- res2 %>%
  separate(odpair, into = c("origin", "destination"), sep = " ") %>%
  group_by(origin, destination) %>%
  mutate(step = row_number()) %>%
  group_by(origin, step) %>%
  mutate(freq = `Point Forecast`/sum(`Point Forecast`),
         freq = if_else(origin==destination,0,freq)) # calculating the relative frequency

write_csv(res2, "./R/DATA-PROCESSED/migrationprobs.csv")