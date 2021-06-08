source('./R/SCRIPTS/000-Libraries.R')      # loading in the libraries
source('./R/SCRIPTS/001-fipscodes.R')

# https://github.com/mathewhauer/irs-migration-replication/blob/master/DATA-PROCESSED/county_migration_data.csv
# 
# 
# download.file("https://osf.io/6eygw/download", "R/DATA-RAW/county_migration_data.csv", mode='wb', cacheOK=FALSE)
# 
IRSmig <- read_csv(file="https://osf.io/6eygw/download", col_names = TRUE) %>%
  mutate(origin = case_when(
    origin == "12025" ~ "12086",
    TRUE ~ origin
  ),
  destination = case_when(
    destination == "12025" ~ "12086",
    TRUE ~ destination
  )) %>%
  gather(Year, mig, `1990`:`2010`) %>%
  group_by(origin, destination, Year) %>%
  dplyr::summarise(mig = sum(mig)) %>%
  spread(Year, mig) 


##### Proportions Dataset #####
county_migration_probs <- gather(IRSmig, Year, mig, `1990`:`2010`) %>% # going from wide to tall
  group_by(origin, Year) %>% # grouping by origin and year
  mutate(freq = mig/sum(mig)) %>% # calculating the relative frequency
  dplyr::select(-mig) %>% # deselecting the count variable
  spread(Year, freq) %>% # going from tall to wide
  filter(origin %in% toymodel,
         destination %in% toymodel) %>%
  dplyr::select(origin, destination, `2010`)