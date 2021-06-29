source("./R/SCRIPTS/000-Libraries.R")
fipslist <- read_csv(file="https://www2.census.gov/geo/docs/reference/codes/files/national_county.txt", col_names = FALSE) %>%
  mutate(GEOID = paste0(X2, X3)) %>%
  dplyr::rename(state = X1,
                STATEID = X2,
                CNTYID = X3,
                NAME = X4) %>%
  filter(!STATEID %in% c("60", "66", "69", "72", "74", "78")) # filtering out the outerlying areas.

# Setting the groupings
GROUPING <- c("STATE", "COUNTY", "YEAR")
# READING THE cdc DATA INTO R. THE DATA ARE IN A SINGLE COLUMN FORMAT AND SO THEY MUST BE BROKEN APART.
K05_pop<- read.table("DATA-RAW/us.1969_2018.19ages.txt") 
K05_pop$V1 <- as.character(K05_pop$V1) # SETTING THE ENTIRE SINGLE VARIABLE INTO A CHARACTER
K05_pop$YEAR <- as.numeric(substr(K05_pop$V1,1,4)) # SEPARATING THE YEAR AND SETTING IT AS A NUMBER
K05_pop$STATEID <- substr(K05_pop$V1, 5,6) # SEPARATING THE 2 CHARACTER STATE ABBREVIATION
K05_pop$STATE <- substr(K05_pop$V1, 7,8) # SEPARATING THE 2-DIGIT STATE CODE
K05_pop$COUNTY <- substr(K05_pop$V1,9,11) # SEPARATING THE 3-DIGIT COUNTY CODE
K05_pop$REGISTRY <- substr(K05_pop$V1, 12,12) # REGISTRY IS A THROW AWAY VARIABLE REFERING TO ODD GEOGRAPHIES
K05_pop$RACE <- substr(K05_pop$V1, 14,14) # SEPARATING OUT THE RACE CODES.
K05_pop$ORIGIN <- substr(K05_pop$V1, 15,15) # SEPARATING OUT HISPANIC ORIGIN. THIS VARIABLE IS NOT APPLICABLE IN THE 1969-2016 DATA
K05_pop$SEX <- substr(K05_pop$V1, 16,16) # SEPARATING OUT THE SEX DATA

# SEPARATING OUT AGE CATEGORIES. THE CDC DATA CONSISTS OF 19 AGE GROUPS WHERE "00" IS CODED AS 0 YEAR OLDS AND "01" IS CODED AS 1-4 YEAR OLDS.
# I RECODE 00 TO 01 TO CREATE A 0-4 YEAR OLD AGE GROUP.
K05_pop$AGE <- as.numeric(if_else(substr(K05_pop$V1, 17, 18) == "00","01",substr(K05_pop$V1, 17, 18)))

K05_pop$POPULATION <- as.numeric(substr(K05_pop$V1, 19, 30)) # SEPARATING THE ACTUAL POPULATION ESTIMATES.

# THE DATA NEED TO BE AGGREGATED TO THE LEVEL OF ANALYSIS BASED ON THE GROUPING FROM ABOVE. THIS IS TO SUM THE 0 AND 1-4 AGE GROUPS
# INTO THE 0-4 AGE GROUP

K05_pop <- K05_pop %>%
  group_by(STATE, COUNTY, YEAR, AGE, SEX) %>%
  dplyr::summarise(POPULATION = sum(POPULATION))

# Aggregating the 
K05_tot <- K05_pop %>%
  group_by(.dots = GROUPING) %>%
  dplyr::summarise(POPULATION = sum(POPULATION))

K05_pop$GEOID <- paste0(K05_pop$STATE, K05_pop$COUNTY) # SETTING THE 5-DIGIT FIPS CODE
K05_tot$GEOID <- paste0(K05_tot$STATE, K05_tot$COUNTY) # SETTING THE 5-DIGIT FIPS CODE


anomalies <- read_csv("./R/DATA-PROCESSED/anomalies.csv")

cnty_years <- c(paste(anomalies$GEOID, anomalies$YEAR, sep = "_"),
                paste(anomalies$GEOID, anomalies$YEAR+1, sep = "_")
)

## The census estimates are unstable before 1980. We look only at outliers after 1980.
timefilt <- 1980

## We are interested in population drops so we're looking for instances where the counterfactual is
## above the true value AND where the coefficients are negative (indicating decline).
popdrops <- read_csv("./R/DATA-PROCESSED/anomaliesdat.csv")
# Generating the list of counties with population drops that are greater than 1980 and a population loss.
a <- popdrops %>%  
  filter(time >= timefilt,
         perdrop <1) %>%
  left_join(., fipslist)

## Saving the output.
# write_csv(a3, "./R/DATA-PROCESSED/anomalies.csv")

# Joining our pop drops with the underlying population data.
a <- left_join(popdrops, K05_tot) %>%
  left_join(., K05_tot, by = c("GEOID", time = "YEAR")) %>%
  mutate(perdrop = POPULATION.y/POPULATION.x) %>% # this calculates the percentage drop
  filter(time >= timefilt,
         perdrop <1)

# Essentially doubling the database so we have a duplication for men and women.
a1 <- a %>% dplyr::select(GEOID, YEAR=time, perdrop)
a2 <- a1 %>% mutate(YEAR = YEAR-1) %>%
  bind_rows(., a1)

## We have to create a unique id for each county-year decline.
a1$uniqueid = paste(a1$GEOID, a1$YEAR, sep = "_")

## We'll calculate CCRs for the group of counties with pop declines.
CCRs_base<- K05_pop %>%
  filter(GEOID %in% a$GEOID)

## This adds "X" to start of the agegroup, converts some data formats, and goes from tall to wide.
CCRs <- CCRs_base %>%
  ungroup() %>%
  mutate(AGE = paste0("X", str_pad(AGE, 2, pad ="0")),
         GEOID = paste0(STATE, COUNTY),
         POPULATION = as.numeric(POPULATION)) %>%
  group_by(STATE, COUNTY, GEOID, YEAR, SEX) %>%
  spread(AGE, POPULATION)
## We have to transform some of the data to ensure null values are 0.
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

## This will calculate the CCRs for each age group for each year.
CCRs2<- CCRs %>%
  arrange(GEOID, SEX, YEAR) %>%
  ungroup() %>%
  group_by(GEOID, SEX) %>%
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
         ccr17 = X18 / (lag(X17, 5) + lag(X18, 5))
  )
## We then join our CCRs for each county-age-sex group with the county-level population decline.
CCRs3 <- left_join(CCRs2, a2) %>%
  na.omit() %>%
  dplyr::select(-X01:-X18) %>% # deselecting the raw population numbers
  group_by(SEX, GEOID, YEAR) %>%
  pivot_longer(cols = starts_with("ccr"), names_to = "groups", values_to = "CCR") %>% # going from wide to long.
  group_by(SEX, GEOID, groups) %>%
  mutate(testval = CCR / lag(CCR), # This gives us the actual change in the CCR
         uniqueid = paste(GEOID, YEAR, sep = "_")) %>%
  filter(testval != 1,
         uniqueid %in% a1$uniqueid) %>% # filtering out the non-outlier years.
  na.omit() %>%
  dplyr::select(-CCR) %>%
  pivot_wider(names_from = groups, values_from = testval) %>% # going back to wide.
  force() 
CCRs3[is.na(CCRs3)] <- 1 # setting NA values to 1, suggesting there is no change.
CCRs3 <- CCRs3 %>%
  pivot_longer(cols = starts_with("ccr"), names_to = "groups", values_to = "testval")%>%
  filter(!GEOID %in% c("26069", "37161", "41065", "47097"))


##### This will calculate the individual coefficients for each age/sex group.
##### This will calculate the individual coefficients for each age/sex group.
coeffs <- data.frame()[1:17, ]
for(i in 1:17){
  z <- filter(CCRs3, groups == paste0("ccr",i), # filtering to age group i and men
              SEX == 1)  
  ## calculating a linear model based on a 2-order polynomial of the log of the population drop
  # summary((fit <- lm(log(z$testval) ~ poly(log(z$perdrop),2))))
  summary(fit <- lm(z$testval ~ z$perdrop))
  ## gathering our results.
  coeffs2 <- data.frame(groups = paste0("ccr",i),
                        a = fit$coefficients[1],
                        b = fit$coefficients[2],
                        c = fit$coefficients[3],
                        # d = fit$coefficients[4],
                        rsq = summary(fit)$adj.r.squared,
                        n = nrow(z),
                        sex = 1)
  ## Repeating the previous code chunk but with women.
  z <- filter(CCRs3, groups == paste0("ccr",i),
              SEX == 2)
  # (fit <- lm(log(z$testval) ~ poly(log(z$perdrop),2)))
  summary(fit <- lm(z$testval ~ z$perdrop))
  coeffs3 <- data.frame(groups = paste0("ccr",i),
                        a = fit$coefficients[1],
                        b = fit$coefficients[2],
                        c = fit$coefficients[3],
                        # d = fit$coefficients[4],
                        rsq = summary(fit)$adj.r.squared,
                        n = nrow(z),
                        sex = 2)
  coeffs2 <- rbind(coeffs2, coeffs3)
  coeffs <- rbind(coeffs, coeffs2)
}

## We then join our CCRs for each county-age-sex group with the county-level population decline.
CCRs4 <- left_join(CCRs2, a2) %>%
  na.omit() %>%
  dplyr::select(-X01:-X18) %>% # deselecting the raw population numbers
  group_by(SEX, GEOID, YEAR) %>%
  pivot_longer(cols = starts_with("ccr"), names_to = "groups", values_to = "CCR") %>% # going from wide to long.
  group_by(SEX, GEOID, groups) %>%
  mutate(CCRMinusOne = lag(CCR),
    testval = CCR / lag(CCR), # This gives us the actual change in the CCR
         uniqueid = paste(GEOID, YEAR, sep = "_")) %>%
  filter(testval != 1,
         uniqueid %in% a1$uniqueid) %>% # filtering out the non-outlier years.
  na.omit() %>%
  mutate(SEX = as.numeric(SEX))

CCRs_base <- CCRs_base %>% mutate(SEX = as.numeric(SEX),
                                  cnty_year = paste(GEOID, YEAR, sep = "_"),
                                  groups = paste0("ccr", AGE))
CCRs_tminus <- CCRs_base %>%
  filter(cnty_year %in% cnty_years) %>%
  mutate(PopMinusOne = POPULATION,
         YEAR = YEAR+1) %>%
  dplyr::select(-POPULATION, -cnty_year)

CCRs_base2 <- left_join(CCRs_base, CCRs_tminus)

CCRs5 <- CCRs4 %>%
  left_join(., coeffs, by = c("groups" = "groups", "SEX" = "sex")) %>%
  filter(!GEOID %in% c("26069", "37161", "41065", "47097")) %>%
  # mutate(reduce = exp(b * log(perdrop) + c * (log(perdrop)^2)))
  mutate(reduce = exp(a + b * log(perdrop)),
         diff = testval - reduce,
         predCCR = CCRMinusOne * reduce,
         diff = CCR - predCCR) %>%
  left_join(., CCRs_base2 ) %>%
  mutate(predPop = predCCR*PopMinusOne,
         numdiff = (POPULATION - predPop),
  perdiff = numdiff/ POPULATION)
# left_join(., CCRs_tminus)

write_csv(CCRs5, "./R/DATA-PROCESSED/CCR_evaluation.csv")

summarystats <- CCRs5 %>%
  group_by(GEOID, YEAR) %>%
  dplyr::summarise(POPULATION = sum(POPULATION),
                   predPop = sum(predPop)) %>%
  mutate(diff = predPop - POPULATION,
         perdiff = diff / POPULATION)

a<-ggplot(summarystats) +
  # geom_point(aes(x = CCR, y = predCCR), alpha = 1/10) +
  geom_point(aes(x = POPULATION, y = predPop)) +
  
  theme_bw() +
  geom_abline(slope=1) +
  # geom_abline(slope=0.95, linetype="dotted") +
  # geom_abline(slope=1.05, linetype="dotted") +
  scale_x_continuous(trans='log2', label = comma) +
  scale_y_continuous(trans='log2', label = comma) +
  labs(x = "Observed Population",
       y = "Predicted Population",
       title = "Total Population",
       caption = "n=48")

b<-ggplot(CCRs5) +
  # geom_point(aes(x = CCR, y = predCCR), alpha = 1/10) +
  geom_point(aes(x = POPULATION, y = predPop)) +
  
  theme_bw() +
  geom_abline(slope=1) +
  # geom_abline(slope=0.95, linetype="dotted") +
  # geom_abline(slope=1.05, linetype="dotted") +
  scale_x_continuous(trans='log2', label = comma) +
  scale_y_continuous(trans='log2', label = comma) +
  labs(x = "Observed Population",
       y = "Predicted Population",
       title = "Population by age-group",
       caption = "n=1,630")

plot_grid(a,b, ncol=2)
cor(CCRs$CCR, CCRs$predCCR)^2
plot(CCRs$CCR, CCRs$predCCR)
mean(abs(CCRs5$perdiff))
mean(abs(summarystats$perdiff))
quantile(abs(summarystats$perdiff))
quantile(abs(CCRs5$perdiff))


evalstats <- data.frame(level = c("$\\P_t$", "$\\P_x$"))
evalstats