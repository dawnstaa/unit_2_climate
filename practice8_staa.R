#Amadi 02/10/2022 with Erin

# temp_anomaly = read.table("data/647_Global_Temperature_Data_File.txt", skip=5, sep="", header = FALSE, col.names = c("Year", "No_Smoothing", "Lowess_5"))
url = 'https://data.giss.nasa.gov/gistemp/graphs/graph_data/Global_Mean_Estimates_based_on_Land_and_Ocean_Data/graph.txt'
temp_anomaly = read.delim(url, skip=5,
                          sep="", header=FALSE,
                          col.names = c("Year", "No_Smoothing", "Lowess_5"))
class(temp_anomaly)
head(temp_anomaly)
tail(temp_anomaly)
dim(temp_anomaly)
summary(temp_anomaly)

temp_1998 = temp_anomaly$No[temp_anomaly$Year==1998]
#temp_1998 = temp_anomaly$No_Smoothing[temp_anomaly$Year==1998]
temp_2012 = temp_anomaly$No[temp_anomaly$Year==2012]


plot(No_Smoothing ~ Year, data=temp_anomaly,
     ylab="Global mean temperature anomaly") + # type="b") plots both lines and dots
  lines(No_Smoothing ~ Year, data=temp_anomaly) +
  lines(Lowess_5 ~ Year, data=temp_anomaly, col="red")
abline(v=1998, lty = "dashed")
abline(v=2012, lty="dashed")
lines(c(temp_1998, temp_2012) ~ c(1998, 2012), col = "blue", lwd = 3)


# function to create user defined function
data = c(1,3,5,7,9,11,13,15)
# user passes vector of ordered time series
calc_rolling_avg = function(data, moving_window=5){
  result = rep(NA, length(data)) # initializing results vector
  for(i in c(moving_window:length(data))){
    result[i] = mean(data[seq(from=(i-moving_window), to=i)])
    
  }
  return(result)
}

calc_rolling_avg(data)
calc_rolling_avg(data, moving_window = 2)

#using data
temp_anomaly_5yr = calc_rolling_avg(temp_anomaly$No_Smoothing)
temp_anomaly$avg_5_yr = temp_anomaly_5yr
head(temp_anomaly)

plot(No_Smoothing ~ Year, data=temp_anomaly, type="b")+
  lines(avg_5_yr ~ Year, data= temp_anomaly, col="red", lwd=2)

temp_anomaly_10yr = calc_rolling_avg(temp_anomaly$No_Smoothing, moving_window = 10)
temp_anomaly$avg_10_yr = temp_anomaly_10yr
head(temp_anomaly)
plot(No_Smoothing ~ Year, data=temp_anomaly, type="b")+
  lines(avg_10_yr ~ Year, data= temp_anomaly, col="green", lwd=2)


#calculating moving avg using the end point definition
dim(temp_anomaly)
seq(dim(temp_anomaly)[1])  # Note c(1:dim(temp_anomaly)[1]) produces same vector
# Initialize my results data frame
roll_avg = temp_anomaly
roll_avg$year5 = NA
roll_avg$year10 = NA
roll_avg$year20 = NA
# Use for loop to calculate 5, 10 and 20 year rolling averages
for (i in seq(dim(temp_anomaly)[1]))
{
  # Calculate 5 year moving averages
  if (i >= 5 & i <= (dim(temp_anomaly)[1])){
    roll_avg$year5[i] = mean(roll_avg[c((i-4):i),'No_Smoothing'])
  }
  # Calculate 10 year moving averages
  if (i >= 10 & i <= (dim(temp_anomaly)[1])){
    roll_avg$year10[i] = mean(roll_avg[c((i-9):i),'No_Smoothing'])
  }
  # Calculate 20 year moving averages
  if (i >= 20 & i <= (dim(temp_anomaly)[1])){
    roll_avg$year20[i] = mean(roll_avg[c((i-19):i),'No_Smoothing'])
  }
}
head(roll_avg)
  
  
