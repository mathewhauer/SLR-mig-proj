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

# county_migration_probs <- gather(mig, Year, mig, `X1990`:`X2017`) %>% # going from wide to tall
#   group_by(origin, Year) %>% # grouping by origin and year
#   mutate(freq = mig/sum(mig)) %>% # calculating the relative frequency
#   dplyr::select(-mig) %>% # deselecting the count variable
#   spread(Year, freq) # going from tall to wide

 num <- seq(1,FORLEN,5)

odpairs <- unique(mig$odpair)
 
forecasts <- function(this.od){
# for(this.od in unique(mig$odpair)){
  # print(this.od)
 z<- mig[which(mig$odpair == this.od),] %>%
    gather(Year, mig, `X1990`:`X2017`) %>%
    pull(mig) %>%
    as.ts()
 pred <- as_data_frame(forecast(ets(z), h = FORLEN))[c(num),c(1:3)]
 pred$odpair <- this.od
 return(pred)
 # forecasts <- rbind(forecasts, pred)
}

plan(multisession, workers = detectCores() - 1)
tic()
res <- future_map(odpairs, forecasts)
toc()

res <- rbindlist(res)

res2 <- res %>%
  separate(odpair, into = c("origin", "destination"), sep = " ") %>%
  group_by(origin, destination) %>%
  mutate(step = row_number()) %>%
  group_by(origin, step) %>%
  mutate(freq = `Point Forecast`/sum(`Point Forecast`),
         freq = if_else(origin==destination,0,freq)) # calculating the relative frequency

  
# dat <- res2 %>%
#   filter(origin %in% toymodel,
#          destination %in% toymodel)

write_csv(res2, "./R/DATA-PROCESSED/migrationprobs.csv")