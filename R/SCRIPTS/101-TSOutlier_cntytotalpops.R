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
sigma <-4 # We set this threshold for our outliers

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
# write_csv(popdrops, "./R/DATA-PROCESSED/anomaliesdat.csv")

# Generating the list of counties with population drops that are greater than 1980 and a population loss.
a <- popdrops %>%  
  filter(time >= timefilt,
         perdrop <1) %>%
  left_join(., fipslist)

## Saving the output.
# write_csv(a, "./R/DATA-PROCESSED/anomalies.csv")