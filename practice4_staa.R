

#downloading the data and reading it
url = 'ftp://aftp.cmdl.noaa.gov/products/trends/co2/co2_mm_mlo.txt'
co2 = read.table(url, col.names = c("year", "month", "decimal_date", "monthly_average", "deseasonalized", "n_days", "st_dev_days", "monthly_mean_uncertainty"))

co2 = read.table("data/co2_mm_mlo.txt", col.names = c("year", "month", "decimal_date", "monthly_average", "deseasonalized", "n_days", "st_dev_days", "monthly_mean_uncertainty"))

# checking out data to see how it looks like
head(co2)
tail(co2)
summary(co2)
range(co2$year)
range(co2$decimal_date)
max(co2$monthly_average)

# plotting data
#'+' keeps lines together so RMarkdown doesn't close the plot b4 all elements are added
plot(monthly_average ~ decimal_date, data = co2, type="l")
plot(monthly_average ~ decimal_date, data = co2, type = "l", ylab = "CO2 (ppm)",
     xlab = "Year", main = "Keeling Curve") + 
  lines(y=co2$deseasonalized, x=co2$decimal_date, col="green")# 2 plots in one

#saving the image in a pdf document
pdf("figures/keelingCurve.pdf", width = 7, height = 5)
plot(monthly_average ~ decimal_date, data = co2, type = "l", ylab = "CO2 (ppm)",
     xlab = "Year", main = "Keeling Curve") + 
  lines(y=co2$deseasonalized, x=co2$decimal_date, col="green")
dev.off()

##Examinig the seasonality of CO2 data
co2$seasonal_cycle = co2$monthly_average - co2$deseasonalized #Calculate detrended co2 fluctuation
head(co2)
plot(seasonal_cycle ~ decimal_date, data = co2, type = "l")

#subsetting to look at the most recent 5yrs
# 2 ways to get the third row of the first column:
co2[1,3] #1st row 3rd column
co2$year[3] #year column 3rd row
# 2 ways to get the whole second column (commented out bc it's a lot to print)
co2[,2]  
co2$month 
# 3 ways to get the first 6 rows of every column
co2[c(1:6),]
co2[seq(1:6),]
head(co2)

#grab the last 5 years of data (plus a few months), 
#we only want values of `co2$decimal_date` greater than 2016
# Showing 2 ways to subset
summary(co2$decimal_date > 2016) # gives vector of TRUES and FALSES
summary(which(co2$decimal_date > 2016)) # vector of indices that meet condition 
#(see ?which for bells and whistles)

# Either method can be used to subset the data
co2_2017to2020 = co2[co2$decimal_date > 2016,]
co2_2017to2020
plot(seasonal_cycle ~ decimal_date, data=co2_2017to2020, type="l")


# 2 ways to grab seasonal_cycle data only from the month of January
#jan_anomalies = co2[which(co2$month==1), "seasonal_cycle"]
jan_anomalies = co2$seasonal_cycle[which(co2$month==1)]
# Calculate the mean of the january anomalies over the time period
mean(jan_anomalies) 

head(co2)
co2_monthly_cycle = data.frame(month=seq(12), detrended_monthly_cycle=NA) #Note how it automatically recycles NA to fill the column
head(co2_monthly_cycle)

# Can use this method
co2_monthly_cycle$detrended_monthly_cycle[1] = mean(co2$seasonal_cycle[which(co2$month==1)])
co2_monthly_cycle$detrended_monthly_cycle[2] = mean(co2$seasonal_cycle[which(co2$month==2)])
co2_monthly_cycle$detrended_monthly_cycle[3] = mean(co2$seasonal_cycle[which(co2$month==3)])
co2_monthly_cycle$detrended_monthly_cycle[4] = mean(co2$seasonal_cycle[which(co2$month==4)])
co2_monthly_cycle$detrended_monthly_cycle[5] = mean(co2$seasonal_cycle[which(co2$month==5)])
co2_monthly_cycle$detrended_monthly_cycle[6] = mean(co2$seasonal_cycle[which(co2$month==6)])
co2_monthly_cycle$detrended_monthly_cycle[7] = mean(co2$seasonal_cycle[which(co2$month==7)])
co2_monthly_cycle$detrended_monthly_cycle[8] = mean(co2$seasonal_cycle[which(co2$month==8)])
co2_monthly_cycle$detrended_monthly_cycle[9] = mean(co2$seasonal_cycle[which(co2$month==9)])
co2_monthly_cycle$detrended_monthly_cycle[10] = mean(co2$seasonal_cycle[which(co2$month==10)])
co2_monthly_cycle$detrended_monthly_cycle[11] = mean(co2$seasonal_cycle[which(co2$month==11)])
co2_monthly_cycle$detrended_monthly_cycle[12] = mean(co2$seasonal_cycle[which(co2$month==12)])

#or this to fill up 'detrended_monthly_cycle'
co2_monthly_cycle$detrended_monthly_cycle[1] = mean(co2[co2$month==1,'seasonal_cycle'])
co2_monthly_cycle$detrended_monthly_cycle[2] = mean(co2[co2$month==2,'seasonal_cycle'])
co2_monthly_cycle$detrended_monthly_cycle[3] = mean(co2[co2$month==3,'seasonal_cycle'])
co2_monthly_cycle$detrended_monthly_cycle[4] = mean(co2[co2$month==4,'seasonal_cycle'])
co2_monthly_cycle$detrended_monthly_cycle[5] = mean(co2[co2$month==5,'seasonal_cycle'])
co2_monthly_cycle$detrended_monthly_cycle[6] = mean(co2[co2$month==6,'seasonal_cycle'])
co2_monthly_cycle$detrended_monthly_cycle[7] = mean(co2[co2$month==7,'seasonal_cycle'])
co2_monthly_cycle$detrended_monthly_cycle[8] = mean(co2[co2$month==8,'seasonal_cycle'])
co2_monthly_cycle$detrended_monthly_cycle[9] = mean(co2[co2$month==9,'seasonal_cycle'])
co2_monthly_cycle$detrended_monthly_cycle[10] = mean(co2[co2$month==10,'seasonal_cycle'])
co2_monthly_cycle$detrended_monthly_cycle[11] = mean(co2[co2$month==11,'seasonal_cycle'])
co2_monthly_cycle$detrended_monthly_cycle[12] = mean(co2[co2$month==12,'seasonal_cycle'])

# Examine and plot detrended monthly cycle
co2_monthly_cycle
plot(detrended_monthly_cycle ~ month, data=co2_monthly_cycle, type="l", )


#Exercise 4.1 - Plotting the seasonal cycle of CO2 in 1959 and 2019 (correct or not?)
co2_1959 = co2[co2$year == 1959,]
co2_2019 = co2[co2$year == 2019,]
plot(seasonal_cycle ~ month, data = co2_1959, type="l", ylim= c(-5,5)) + 
  lines (y=co2_2019$seasonal_cycle, x=co2_2019$month, type="l", col ="cyan")

