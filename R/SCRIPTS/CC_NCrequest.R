source('./R/SCRIPTS/001-fipscodes.R')

proj <- read_csv("./R/DATA-PROCESSED/PROJECTIONS/projections_AS_controlled_MSP.csv") %>%
  left_join(., fipslist) %>%
  filter(state == "NC")

write_csv(proj, "./R/DATA-PROCESSED/projectionsNC_AS.csv")