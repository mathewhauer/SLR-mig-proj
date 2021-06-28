

anomalies <- read_csv("./R/DATA-PROCESSED/anomalies.csv")

cnty_years <- c(paste(anomalies$GEOID, anomalies$YEAR, sep = "_"),
                     paste(anomalies$GEOID, anomalies$YEAR+1, sep = "_")
)

anoms <- anomalies %>%
  mutate(cnty_year = paste(GEOID, YEAR, sep = "_"),
         anomalyyear = "a") %>%
  dplyr::select(cnty_year, anomalyyear)

unzip(zipfile = "./R/DATA-RAW/UID6147_ZIP.ZIP",
      exdir = "./R/DATA-RAW")

dat_sheldus1 <- read_csv("./R/DATA-RAW/UID6147f_AGG_A.csv") %>%
  dplyr::select(1:5,7,8,10,11,14,15)
dat_sheldus2 <- read_csv("./R/DATA-RAW/UID6147f_AGG_B.csv") %>%
  dplyr::select(1:5,7,8)

dat_sheldus <- bind_rows(dat_sheldus1, dat_sheldus2)
dat_sheldus2 <- dat_sheldus %>%
  dplyr::select(-Hazard) %>%
  pivot_longer(cols = c(5:12),
               names_to = "variables",
               values_to = "value") %>%
  mutate(value = replace_na(value, 0)) %>%
  group_by(`State Name`, `County Name`, `County FIPS`, Year, variables) %>%
    dplyr::summarise(value = sum(value)) %>%
  pivot_wider(names_from = variables, values_from = value) %>%
  mutate(cnty_year = paste(substr(`County FIPS`,2,6), Year, sep = "_")) %>%
  filter(cnty_year %in% cnty_years) %>%
  left_join(., anoms) %>%
  filter(`PropertyDmgPerCapita(ADJ 2019)` >= 0.38467) # This is the 50th percentile value
  group_by(`State Name`, `County Name`, `County FIPS`, anomalyyear) %>%
    dplyr::summarise(propdam = max(`PropertyDmgPerCapita(ADJ 2019)`))