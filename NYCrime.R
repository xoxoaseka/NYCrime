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
df <- crime
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
# for the purpose of this study it was decided to exclude 121 precinct from input dataset
# in the future it would be reasonable to study 2013-2017 years separately (when 121 was introduced and data became available) 
# make copy of full data
crime_full <- crime
# drop 121 precinct
crime <- crime[!crime$PCT %in% c("121"), ]

# Plot the time series for each of the seven major felonies and for the total 
# (aggregation of all felonies)
require(ggplot2)
require(reshape2)

# create summary
counted <- crime %>% group_by(CRIME) %>% summarise_all(funs(sum))
counted <- counted %>% select(-PCT)

# turn into an easily-plottable format
counted_melt <- melt(counted, id='CRIME')

#plot graph for each felony + one for total felonies 
ggplot(data=counted_melt, aes(x=variable, y=value, group = CRIME, colour = CRIME)) +
  geom_line() + facet_wrap(~CRIME, scales = 'free_y', ncol = 1)

# plot all timeseries in one graph
rownames(counted) <- counted$CRIME
counted <- counted[,-1]
require(data.table)
counted_transpose <- transpose(counted)
colnames(counted_transpose) <- counted$CRIME
rownames(counted_transpose) <- colnames(counted)
counted_transpose <- counted_transpose[-1,]
crime_ts <- ts(data = counted_transpose, start = 2000, end = 2017, names = colnames(counted_transpose))
ts.plot(crime_ts, gpars = list(col = rainbow(8)), ylab = "Number of crimes", xlab = "Years", main = "Tire series of NYC crimes during 2000-2017 for all crime types (summed over all precincts)")
grid()
# normalize by dividing each crime type by max count
# normalize <- function(x){
#   return((x-min(x))/(max(x)-min(x)))
# }
# normalized_crime <- as.data.frame(lapply(counted_transpose$BURGLARY, normalize))
# crime_normalized <- apply(counted_transpose, 2, counted_transpose$BURGLARY/max(counted_transpose$BURGLARY))

for (i in 1:nrow(counted_transpose)){ 
  counted_transpose[i, 1] <- counted_transpose[which.max(counted_transpose[i,])]
  } 

# legend("topright", legend = colnames(counted_transpose), col = 1:8, lty = 1)
# Look at variance across precincts
precincts <- crime %>% filter(CRIME == 'TOTAL SEVEN MAJOR FELONY OFFENSES') %>% 
  select(-CRIME) %>% 
  melt(id='PCT')

precincts_var <- precincts %>% 
  group_by(PCT) %>% 
  count(var(value))

precincts_var <- rename(precincts_var, 'variance' = 'var(value)') %>% select(-n)

#plot variance for each PCT
ggplot(data=precincts_var, mapping=aes(x=PCT , y=variance, group=PCT, colour=PCT)) +
  geom_col() + xlab(NULL) +
  labs(title="Variance per precinct", x='Precinct', y='Variance')

#plot crime over time for PCT with largest (14 and 18) and lowest (22 and 94) variance 
top_variance <- precincts %>% filter(PCT == 14 |
                                       PCT == 18|
                                       PCT == 22|
                                       PCT == 94)

ggplot(data=top_variance, aes(x=variable, y=value, group = PCT, colour = PCT)) +
  geom_line() + 
  facet_wrap(~PCT, scales = 'free_y') +
  labs(title="Total crime over time", x='Year', y='Total crime')

# TODO:
# to see  trends of different crime types during 2000-2017 do a quantitative analysis of these time series (trends, periodicity etc.). 
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
