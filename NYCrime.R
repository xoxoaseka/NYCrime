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
rapply(crime,function(x)length(unique(x)))
# check crime types and trim whitespaces
paste(unique(crime$CRIME))
crime$CRIME <- trim(crime$CRIME)
# checking column data types
sapply(crime, class)
# convert factors to characters
require(dplyr)
crime %>% mutate_if(is.factor, as.character) -> crime

### Detect and remove outliers ###
# handle missing values
#removing rows that are not for a precinct (all precincts are identified by numbers)
crime$PCT <- as.numeric(crime$PCT)
crime <- dplyr::filter(crime, !is.na(PCT))
# there are 123 precincts with 8 rows for each
# https://www1.nyc.gov/site/nypd/bureaus/patrol/precincts-landing.page
full_data_size = 123*8
# ideally we should have 123*8 = 984 rows
missing_data_dim = full_data_size - nrow(crime)
# therefore we have 368 rows of missing data

# copy to test df to detect outliers
df <- df[!df$CRIME %in% c("TOTAL SEVEN MAJOR FELONY OFFENSES"), ]
# cooksd <- cooks.distance(lm(df$`2010` ~ ., data=df))
# plot(cooksd, pch=".", cex=2, main="Influential Obs by Cooks distance")  # plot cook's distance
# abline(h = 4*mean(cooksd, na.rm=T), col="red")  # add cutoff line
# text(x=1:length(cooksd)+1, y=cooksd, 
#      labels=ifelse(cooksd>10*mean(cooksd, na.rm=T),
#                    names(cooksd),""), col="red", cex=0.5, pos=4, offset=0.2)  # add labels
# influential <- as.numeric(names(cooksd)[(cooksd > 4*mean(cooksd, na.rm=T))])
# # influential observations
# inf_df <- crime[influential, ] 

plot(df$`2010`, pch=".", cex=3, main="Crimes in 2010")
abline(h = 4*mean(df$`2010`, na.rm=T), col="red")  # add cutoff line
text(x=1:length(df$`2010`)+1, y=df$`2010`, 
     labels=ifelse(df$`2010`>4*mean(df$`2010`, na.rm=T),
                   df$`2010`,""), col="red", cex=0.5, pos=4, offset=0.2)  # add labels
influential <- as.numeric(rownames(df)[(df$`2010` > 4*mean(df$`2010`, na.rm=T))])
# influential observations
inf_df <- crime[influential, ] 
# when we filled NA, 121 precinct was filled with 120's total number of murders
# need to decide whether to drop this row since it is an outlier or 
# make separate analysis on before it was itroduced and after
# article: https://www.silive.com/opinion/index.ssf/2013/11/a_welcome_newcomer_nypds_new_p.html
# TODO:
# Plot the time series for each of the seven major felonies and for the total (aggregation of all felonies)
# to see  trends of different crime types during 2000-2017 and do a quantitative analysis of these time series (trends, periodicity etc.). 
# Look at variance across precincts
# calculate the mean and standard deviation in time for each precinct and each crime
# Next extract the total crime row for each precinct and cluster the time series
# Plot the clusters and discuss the clustering and the trends
# Next: connecting the temporal and spatial components
# obtain the NYC precincts geometry and read it in as a shapefile
# merge the precincts geodataframe with the dataframe of crime that was used to cluster the time series 
# plot a choropleth of the clusters
# Next: K-Means clustering of crime type patterns at the NYC Police precincts level during 2000-2017
# If time allows, merge ACS data and NYC park shapefiles to look at socioeconomic variables and access to green space i.e parks
