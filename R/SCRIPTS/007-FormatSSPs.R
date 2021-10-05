SSPs <- read_csv("./R/DATA-RAW/SspDb_country_data_2013-06-12.csv") %>%
  filter(REGION == "USA",
         grepl("Population",VARIABLE)) %>%
  separate(VARIABLE, c("VARIABLE", "VARIABLE1", "VARIABLE2", "VARIABLE3", "VARIABLE4"), sep ="\\|")

SSPs2 <- SSPs %>%
  dplyr::select(-`1950`:-`2010`, -`2105`:-`2150`) %>%
  mutate(SEX = case_when(
    VARIABLE1 == "Female" ~ 2,
    VARIABLE1 == "Male" ~ 1),
    AGE = case_when(
      VARIABLE2 == "Aged0-4" ~ 1,
      VARIABLE2 == "Aged5-9" ~ 2,
      VARIABLE2 == "Aged10-14" ~ 3,
      VARIABLE2 == "Aged15-19" ~ 4,
      VARIABLE2 == "Aged20-24" ~ 5,
      VARIABLE2 == "Aged25-29" ~ 6,
      VARIABLE2 == "Aged30-34" ~ 7,
      VARIABLE2 == "Aged35-39" ~ 8,
      VARIABLE2 == "Aged40-44" ~ 9,
      VARIABLE2 == "Aged45-49" ~ 10,
      VARIABLE2 == "Aged50-54" ~ 11,
      VARIABLE2 == "Aged55-59" ~ 12,
      VARIABLE2 == "Aged60-64" ~ 13,
      VARIABLE2 == "Aged65-69" ~ 14,
      VARIABLE2 == "Aged70-74" ~ 15,
      VARIABLE2 == "Aged75-79" ~ 16,
      VARIABLE2 == "Aged80-84" ~ 17,
      VARIABLE2 == "Aged85-89" ~ 18,
      VARIABLE2 == "Aged90-94" ~ 18,
      VARIABLE2 == "Aged95-99" ~ 18,
      VARIABLE2 == "Aged100+" ~ 18),
    SSP = case_when(
      grepl("SSP1", SCENARIO) ~ "SSP1",
      grepl("SSP2", SCENARIO) ~ "SSP2",
      grepl("SSP3", SCENARIO) ~ "SSP3",
      grepl("SSP4", SCENARIO) ~ "SSP4",
      grepl("SSP5", SCENARIO) ~ "SSP5"
    )) %>%
  filter(is.na(VARIABLE4),
         !is.na(VARIABLE3)) %>%
  dplyr::select(-MODEL:-UNIT) %>%
  na.omit %>%
  gather(YEAR, Population, `2015`:`2100`) %>%
  group_by(SEX, AGE, SSP, YEAR) %>%
  dplyr::summarise(Population = sum(Population)) %>%
  ungroup() %>%
  spread(SSP, Population) %>%
  mutate(YEAR = as.integer(YEAR)
         # SEX = as.character(SEX))
  )

rm(SSPs)