# set working directory
setwd("/Volumes/Transcend\ 1/Documents/University/Masters/md_ml/final")
# download Historical Crime Data by Precinct from 
# https://www1.nyc.gov/site/nypd/stats/crime-statistics/historical.page
system("wget https://www1.nyc.gov/assets/nypd/downloads/excel/analysis_and_planning/historical-crime-data/seven-major-felony-offenses-by-precinct-2000-2017.xls")
# check whether data is here
system("ls")
# read data
require(gdata)
crime = read.xls ("seven-major-felony-offenses-by-precinct-2000-2017.xls", sheet = 1, header = FALSE)
# set header names and remove unnecessary rows
crime <- crime[-1,]
names(crime) <- lapply(crime[1, ], as.character)
crime <- crime[-1,]
# reset row names
rownames(crime) <- seq(length=nrow(crime)) 
# drop factors
crime <- droplevels(crime) 
# deal with NAs and forward fill PCT values for each record
crime[crime==""] <- NA
sum(is.na(crime))
colSums(is.na(crime))
require(zoo)
crime <- na.locf(na.locf(crime), fromLast = TRUE)
# now can run summary of dataset
summary(crime)
