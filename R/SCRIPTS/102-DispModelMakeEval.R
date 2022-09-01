rm(list = ls()) # Remove Previous Workspace
gc(reset = TRUE) # Garbage Collection


# R Workspace Options
options(scipen = 12) # Scientific Notation
options(digits = 6) # Specify Digits
options(java.parameters = "-Xmx1000m") # Increase Java Heap Size

## Loading the libraries
library(tidyverse)
library(cowplot)
library(scales)

## Reading in the Anomalies list
anomalies <- read_csv("./R/DATA-PROCESSED/anomalies.csv")
## Reading in our Population Data
K05_pop <- read_csv(unz("./R/DATA-RAW/K05_pop.zip", "K05_pop.csv"))  %>%
  dplyr::select(-`...1`) # Sometimes unzipping adds a row counter column. We just delete it.

## Calculating the total population in each county and selecting years from before 2018.
K05_tot <- K05_pop %>%
  group_by(STATE, COUNTY, YEAR, GEOID) %>%
  dplyr::summarise(POPULATION = sum(POPULATION)) %>%
  filter(YEAR <= 2018)

cnty_years <- c(paste(anomalies$GEOID, anomalies$YEAR, sep = "_"),
                paste(anomalies$GEOID, anomalies$YEAR+1, sep = "_")
)

## The census estimates are unstable before 1980. We look only at outliers after 1980.
timefilt <- 1980



## We are interested in population drops so we're looking for instances where the counterfactual is
## above the true value AND where the coefficients are negative (indicating decline).
popdrops <- read_csv("./R/DATA-PROCESSED/anomaliesdat.csv")

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
CCRs_base<- K05_pop[which(K05_pop$GEOID %in% a$GEOID), ]

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
  pivot_longer(cols = starts_with("ccr"), names_to = "groups", values_to = "testval")


##### This will calculate the individual coefficients for each age/sex group.
coeffs <- data.frame()[1:17, ]
for(i in 1:17){
  z <- filter(CCRs3, groups == paste0("ccr",i), # filtering to age group i and men
              SEX == 1)  
  ## calculating a linear model based on a 2-order polynomial of the log of the population drop
  summary((fit <- lm(log(z$testval) ~ poly(log(z$perdrop),2))))

  ## gathering our results.
  coeffs2 <- data.frame(groups = paste0("ccr",i),
                        a = fit$coefficients[1],
                        b = fit$coefficients[2],
                        c = fit$coefficients[3],
                        rsq = summary(fit)$adj.r.squared,
                        n = nrow(z),
                        sex = 1)
  ## Repeating the previous code chunk but with women.
  z <- filter(CCRs3, groups == paste0("ccr",i),
              SEX == 2)
  summary((fit <- lm(log(z$testval) ~ poly(log(z$perdrop),2))))
  coeffs3 <- data.frame(groups = paste0("ccr",i),
                        a = fit$coefficients[1],
                        b = fit$coefficients[2],
                        c = fit$coefficients[3],
                        rsq = summary(fit)$adj.r.squared,
                        n = nrow(z),
                        sex = 2)
  coeffs2 <- rbind(coeffs2, coeffs3)
  coeffs <- rbind(coeffs, coeffs2)
}
# 
# write_csv(coeffs, "./R/DATA-PROCESSED/modelcoeffs.csv")

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
  mutate(reduce = exp(b * log(perdrop) + c * log(perdrop)^2),
    diff = testval - reduce,
    predCCR = CCRMinusOne * reduce,
    diff = CCR - predCCR) %>%
  left_join(., CCRs_base2 ) %>%
  mutate(predPop = predCCR*PopMinusOne,
         numdiff = (POPULATION - predPop),
         perdiff = numdiff/ POPULATION)

summarystats <- CCRs5 %>%
  group_by(GEOID, YEAR) %>%
  dplyr::summarise(POPULATION = sum(POPULATION),
                   predPop = sum(predPop)) %>%
  mutate(diff = predPop - POPULATION,
         perdiff = diff / POPULATION)

rsqa <- round(cor(summarystats$POPULATION, summarystats$predPop)^2, digits = 3)
rsqb <- round(cor(CCRs5$POPULATION, CCRs5$predPop)^2, digits = 3)

a<-ggplot(summarystats, aes(x = POPULATION, y = predPop)) +
  geom_point() +
  theme_bw() +
  geom_abline(slope=1) +
  scale_x_continuous(trans='log2', label = comma) +
  scale_y_continuous(trans='log2', label = comma) +
  annotate("text",x = min(summarystats$predPop)*5, y =max(summarystats$POPULATION),
           label = paste0("r^{2}==",rsqa), parse = TRUE) +
  labs(x = "Observed Population",
       y = "Predicted Population",
       title = "Total Population",
       caption = paste0("n=", length(unique(CCRs5$GEOID))))

b<-ggplot(CCRs5, aes(x = POPULATION, y = predPop)) +
  geom_point(alpha = 1/10) +
  theme_bw() +
  geom_abline(slope=1) +
  scale_x_continuous(trans='log2', label = comma) +
  scale_y_continuous(trans='log2', label = comma) +
  annotate("text",x = min(CCRs5$predPop)*10, y =max(CCRs5$POPULATION),
           label = paste0("r^{2}==",rsqb), parse = TRUE) +
  labs(x = "Observed Population",
       y = "Predicted Population",
       title = "Population by age-group",
       caption = paste0("n=", length(CCRs5$GEOID)))

plot_grid(a,b, ncol=2, labels = "auto")