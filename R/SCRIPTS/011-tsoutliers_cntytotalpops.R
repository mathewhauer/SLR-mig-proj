source("./R/SCRIPTS/000-Libraries.R")
# This will download a list of FIPS codes from online.
fipslist <- read_csv(file="https://www2.census.gov/geo/docs/reference/codes/files/national_county.txt", col_names = FALSE) %>%
  mutate(GEOID = paste0(X2, X3)) %>%
  dplyr::rename(state = X1,
                STATEID = X2,
                CNTYID = X3,
                NAME = X4) %>%
  filter(!STATEID %in% c("60", "66", "69", "72", "74", "78")) # filtering out the outerlying areas.

# Setting the groupings
GROUPING <- c("STATE", "COUNTY", "YEAR")

##############################################################
#
# DOWNLOADING THE CDC POPULATION ESTIMATES FOR 1969-2016.
#
# IF RUNNING THIS SCRIPT FOR THE FIRST LINE, Run the download.file line and the gunzip line.
#
# download.file("https://seer.cancer.gov/popdata.thru.2018/yr1969_2018.19ages/us.1969_2018.19ages.txt.gz", "DATA-RAW/us.1969_2018.19ages.txt.gz")
# #UNZIPPING THE DATA FILE
# gunzip("DATA-RAW/us.1969_2018.19ages.txt.gz", overwrite = TRUE, remove = TRUE)
#
###################################################################

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

# Getting a unique list of counties to loop over.
cnties <- unique(K05_pop$GEOID)
df <- data.frame() # the blank data frame to hold the results
sigma <-5 # We set this threshold for our outliers

### This will loop over each of the counties and search for outliers based on sigma.
for(this.county in cnties){
  tryCatch({
  print(this.county)
    ## We first set the output data frame
  df2<- setNames(data.frame(matrix(ncol = 5, nrow = 0)), c("type", "ind", "time", "coefhat", "tstat"))
  
  dat2 <- filter(K05_tot, GEOID == this.county) # getting just our data for this particular county
  dat3 <- ts(dat2$POPULATION, start = c(min(dat2$YEAR),1), frequency = 1) # converting the data to a time series

  # This is the function that detects the outliers. We're searching for additive outliers, level shift
  # outliers, and temporary change outliers.
(outlier.county <- tsoutliers::tso(dat3,types = c("AO","LS","TC"),cval = sigma, maxit.iloop=10))
plot(outlier.county)

# Not all counties contain outliers. So we must create a special condition if there is no outliers.
if(!is.null(outlier.county$times)){
  # we select out the true value (y), the adjusted counterfactual (yadj), and the time index (time)
  df2<- outlier.county$outliers
  altseries <- data.frame(y= outlier.county$y,
                          yadj=outlier.county$yadj,
                          time=min(dat2$YEAR):max(dat2$YEAR)) %>%
    filter(time %in% outlier.county$times)
  df2 <- left_join(df2, altseries)
} else {
  df2 <- rbind(df2, data.frame(type = NA,
                               ind = NA,
                               time = NA,
                               coefhat = NA,
                               tstat = NA,
                               y = NA,
                               yadj = NA))
  
}
# Making sure we keep the county.
df2$GEOID <- paste0(this.county)

df <- bind_rows(df, df2)
rm(dat2)
rm(dat3)
}
, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})

}
#####

## The census estimates are unstable before 1980. We look only at outliers after 1980.
timefilt <- 1980
# popdrops <- filter(df, coefhat <0) %>%
## We are interested in population drops so we're looking for instances where the counterfactual is
## above the true value AND where the coefficients are negative (indicating decline).
popdrops <- filter(df, yadj > y, coefhat <0) %>% 
  mutate(YEAR = time-1, # the time index is off by 1 year.
         perdrop = y / yadj) # We calculate the percentage drop in the population
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
CCRs<- K05_pop %>%
  filter(GEOID %in% a$GEOID)

## This adds "X" to start of the agegroup, converts some data formats, and goes from tall to wide.
CCRs <- CCRs %>%
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
  (fit <- lm(log(z$testval) ~ poly(log(z$perdrop),2)))
  ## gathering our results.
  coeffs2 <- data.frame(groups = paste0("ccr",i),
    a = fit$coefficients[1],
                       b = fit$coefficients[2],
                       c = fit$coefficients[3],
                       # d = fit$coefficients[4],
    rsq = summary(fit)$adj.r.squared,
    n = nrow(z))
  ## Repeating the previous code chunk but with women.
  z <- filter(CCRs3, groups == paste0("ccr",i),
              SEX == 2)
  (fit <- lm(log(z$testval) ~ poly(log(z$perdrop),2)))
  coeffs3 <- data.frame(groups = paste0("ccr",i),
                        a2 = fit$coefficients[1],
                        b2 = fit$coefficients[2],
                        c2 = fit$coefficients[3],
                        # d = fit$coefficients[4],
                        rsq2 = summary(fit)$adj.r.squared,
                        n2 = nrow(z))
  coeffs2 <- cbind(coeffs2, coeffs3)
  coeffs <- rbind(coeffs, coeffs2)
}
z <- filter(CCRs3, groups == "ccr5",
            !GEOID %in% c("06053", "13031", "13259", "26069", "37161",
                          "47097", "51147"))

summary(fit <- lm(log(z$testval) ~ poly(log(z$perdrop),2)))
# summary(fit)$adj.r.squared
# plot(log(z$testval), log(z$perdrop))

a<-data.frame(y =z$testval, x=z$perdrop)

# pred.data = expand.grid(wt = seq(min(a$x), max(a$x), length=108))
# pred.data$mpg = predict(fit, newdata=pred.data)


ggplot(data=a, aes(x= x, y =y)) +
  geom_point() +
  stat_smooth(method = "lm", formula = y ~ x + I(x^2)) +
  theme_bw()



rates <- CCRs3$testval
M <- matrix(log(rates), 108, 17, byrow=TRUE)
a <- colMeans(M)
for(j in 1:17) M[,j] <- M[,j] - a[j]
d <- svd(M)
b <- d$v/sum(d$v)
k <- d$u * sum(d$v) * d$d[1:3]

z <- filter(CCRs3, GEOID == "05093") %>%
  mutate(fit = c(exp(a + b * k[50,1:17])))

ggplot(z, aes(x=groups, y=testval, color=SEX)) + geom_point() + 
     geom_line(aes(x=groups, y=fit,color=SEX, group =1))+
  scale_y_continuous(limits = c(0,1.1))
     # ggtitle("Lee-Carter Fits for 1933 and 1987")
