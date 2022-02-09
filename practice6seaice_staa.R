#Amadi 02/08/2022 exercise 6 with Erin

install.packages("lubridate")
library("lubridate")  # make_date()

##Accessing and reading file
# arctic_ice = read.csv("data/N_seaice_extent_daily_v3.0.csv", skip=2,
#header = FALSE, col.names = c("Year", "Month", "Day", "Extent", "Missing",
                              "Source_Data"))
url = 'ftp://sidads.colorado.edu/DATASETS/NOAA/G02135/north/daily/data/N_seaice_extent_daily_v3.0.csv'
arctic_ice = read.delim(url, skip=2, sep=",", header=FALSE,
                        col.names = c("Year", "Month", "Day", "Extent", "Missing",
                                      "Source_Data"))
head(arctic_ice)
tail(arctic_ice)

#making a column for decimal date calculated by the make_date function
arctic_ice$date = make_date(year=arctic_ice$Year, month=arctic_ice$Month,
                            day=arctic_ice$Day)
head(arctic_ice)
plot(Extent ~ date, data=arctic_ice, ylab="Arctic sea ice extent (x10^6 km^2)",
     type='l') 


# Dump the first year (since it's not complete!)
# Dump the most recent year (since it's not complete!)
arctic_ice_averages = data.frame(Year = seq(min(arctic_ice$Year)+1,
                                          max(arctic_ice$Year)-1),
                               extent_annual_avg = NA, extent_5yr_avg = NA) #arctic_ice_averages or arctic_ice_annual
head(arctic_ice_averages)
tail(arctic_ice_averages)
#arctic_ice_annual = data.frame(Year=seq(min(arctic_ice$Year)+1, max(arctic_ice$Year)-1), extent_annual_avg=NA, extent_5yr_avg=NA)
#head(arctic_ice_annual)

# Hard-coding vs. indexing the years:
arctic_ice$Extent[arctic_ice$Year == 1979]# Find the daily data for the first year
#arctic_ice[which(arctic_ice$Year == 1979),'Extent'] 
mean(arctic_ice$Extent[arctic_ice$Year == 1979]) # Calculate mean of the daily data for the first year (hard coding the year)
arctic_ice_averages$extent_annual_avg[1] = mean(arctic_ice$Extent[arctic_ice$Year == arctic_ice_averages$Year[1]]) # calc and store mean extent for that first year (without hard coding year)

arctic_ice_averages$extent_annual_avg[1] = mean(arctic_ice[which(arctic_ice$Year == arctic_ice_annual$Year[1]),'Extent']) # calc and store mean extent for that first year (without hard coding year)



# calculate annual average:
for (i in seq(dim(arctic_ice_averages)[1]))
{
  arctic_ice_averages$extent_annual_avg[i] = mean(arctic_ice$Extent[arctic_ice$Year == arctic_ice_averages$Year[i]])
  print (arctic_ice_averages$extent_annual_avg[i])
}
head(arctic_ice_averages)
#for (i in seq(dim(arctic_ice_averages)[1]))
#{
#  arctic_ice_averages$extent_annual_avg[i] = mean(arctic_ice[which(arctic_ice$Year == arctic_ice_averages$Year[i]),'Extent'])
#}
head(arctic_ice_annual)




# 5-year average:

for (i in seq(3, dim(arctic_ice_averages)[1]-2)) # skip the first 2 and last 2 years
{
  years = c((arctic_ice_averages$Year[i]-2):(arctic_ice_averages$Year[i]+2))
  arctic_ice_averages$extent_5yr_avg[i] = mean(arctic_ice$Extent[arctic_ice$Year %in% years])
}
head(arctic_ice_averages)



for (i in seq(3, dim(arctic_ice_averages)[1]-2)) # skip the first 2 and last 2 years
{
  years = c((arctic_ice_averages$Year[i]-2):(arctic_ice_averages$Year[i]+2))
  arctic_ice_averages$extent_5yr_avg[i] = mean(arctic_ice[which(arctic_ice$Year %in% years),'Extent'])
}

head(arctic_ice_averages)

# years = seq(from=(arctic_ice_averages))
#plot moving averages with seasonal data
arctic_ice_averages$date = make_date (year=arctic_ice_averages$year)


plot(extent_annual_avg ~ Year, data=arctic_ice_annual, col="red", type="l") +
  lines(extent_5yr_avg ~ Year, data=arctic_ice_annual, col="blue", type="l")


###########################All over again###################################################################################################
#reading data
url = 'ftp://sidads.colorado.edu/DATASETS/NOAA/G02135/north/daily/data/N_seaice_extent_daily_v3.0.csv'
arctic_ice = read.delim(url, skip=2, sep=",", header=FALSE,
                        col.names = c("Year", "Month", "Day", "Extent", "Missing", "Source_Data"))
tail(arctic_ice)

#changing date to decimal date and plotting
# install.packages("lubridate")
library("lubridate")  # make_date()
arctic_ice$date = make_date(arctic_ice$Year, arctic_ice$Month, arctic_ice$Day)
plot(Extent ~ date, data=arctic_ice, ylab="Arctic sea ice extent (x10^6 km^2)", type='l') 

# Dump the first year (since it's not complete!)
# Dump the most recent year (since it's not complete!)
arctic_ice_averages = data.frame(Year=seq(min(arctic_ice$Year)+1, max(arctic_ice$Year)-1), extent_annual_avg=NA, extent_5yr_avg=NA)
head(arctic_ice_averages)

# Hard-coding vs. indexing the years:
arctic_ice$Extent[arctic_ice$Year == 1979] # Find the daily data for the first year
mean(arctic_ice$Extent[arctic_ice$Year == 1979]) # Calculate mean of the daily data for the first year
arctic_ice_averages$extent_annual_avg[1] = mean(arctic_ice$Extent[arctic_ice$Year == arctic_ice_averages$Year[1]]) # calc and store mean extent for that first year (without hard coding year)

# calculate annual average:
for (i in seq(dim(arctic_ice_averages)[1]))
{
  arctic_ice_averages$extent_annual_avg[i] = mean(arctic_ice$Extent[arctic_ice$Year == arctic_ice_averages$Year[i]])
}
head(arctic_ice_averages)

# 5-year average:
for (i in seq(3, dim(arctic_ice_averages)[1]-2)) # skip the first 2 and last 2 years
{
  years = c((arctic_ice_averages$Year[i]-2):(arctic_ice_averages$Year[i]+2))
  arctic_ice_averages$extent_5yr_avg[i] = mean(arctic_ice$Extent[arctic_ice$Year %in% years])
}
head(arctic_ice_averages)
tail(arctic_ice_averages)

#decadal average
for (i in seq(3, dim(arctic_ice_averages)[1]-2)) # skip the first 2 and last 2 years
{
  years = c((arctic_ice_averages$Year[i]-2):(arctic_ice_averages$Year[i]+2))
  arctic_ice_averages$extent_5yr_avg[i] = mean(arctic_ice$Extent[arctic_ice$Year %in% years])
}


#plotting and storing images
# pdf('figures/arctic_ice_extent.pdf', width=7, height=5)
plot(extent_annual_avg ~ Year, data=arctic_ice_averages, col="red", type="l") +
  lines(extent_5yr_avg ~ Year, data=arctic_ice_averages, col="blue", type="l")
# dev.off()

#To plot the annual and 5-year averages on the same plot as the original observations:
# arctic_ice_annual$date = as.Date(paste(arctic_ice_annual$Year, 1, 1, sep = "-")) # as.Date is in base R
arctic_ice_averages$date = make_date(year=arctic_ice_averages$Year) # default month and day = 1
# arctic_ice_annual$date = make_date(year=arctic_ice_annual$Year, month=7, day=1) # set to middle of year
plot(Extent ~ date, data=arctic_ice, ylab="Arctic sea ice extent (x10^6 km^2)", type='l') +
  lines(extent_annual_avg ~ date, data=arctic_ice_averages, col="red") +
  lines(extent_5yr_avg ~ date, data=arctic_ice_averages, col="blue")

#observed rate of change in the annually averaged ice extent
ice_loss_million_km2 = arctic_ice_averages$extent_annual_avg[dim(arctic_ice_averages)[1]] - arctic_ice_averages$extent_annual_avg[1]
time_period = max(arctic_ice_averages$Year) - min(arctic_ice_averages$Year)
# Calc rate of ice loss:
ice_loss_million_km2_per_yr = ice_loss_million_km2 / time_period
