#Amadi 02/15/2022 hwk_3

#Description
#The ice core data was drilled in January 1998 at the Vostok Station in Russia (Barnola, 2003) and is about 41,7378 years old and the deepest depth in the data is 3, 304.4m. 
#This was a joint ice-drilling project involving Russia, USA and France.
# The data columns include depth (m), Age of ice (yr BP), mean age of air (yr BP) and CO2 concentration (ppmv).

#Barnola, J.-M., D. Raynaud, C. Lorius, and N.I. Barkov. 2003. Historical CO2 record from the Vostok ice core. In Trends: A Compendium of Data on Global Change. Carbon Dioxide Information Analysis Center, Oak Ridge National Laboratory, U.S. Department of Energy, Oak Ridge, Tenn., U.S.A.

#Mauna Loa
#this data has record of atmospheric CO2 concentration starting from 1958 to the present day.
#recorded at the Mauna Loa Observatory in Hawaii.
#The columns in the data include year, month, decimal_date, monthly average, deseasonalized, n_days, st_dev_days, monthly_mean_uncertainty


#Accessing and reading ice core data
#Ice core CO2
url = 'https://cdiac.ess-dive.lbl.gov/ftp/trends/co2/vostok.icecore.co2'
icecore = read.delim(url, skip=20,
                     sep="", header=FALSE,
                     col.names = c("Depth_m", "Ice_age_yrBp", "Air_age_mean_yrBp",
                                   "CO2_concentration"))
head(icecore)
tail(icecore)

#icecore_age_converted = data.frame(c(1:363),Ice_age_yr=NA, Air_age_mean_yr=NA)
#head(icecore_age_converted)

#creating columns for the calculated normal yr
icecore$Ice_age_yr=NA
icecore$Air_age_mean_yr=NA # air age mean in yrs = mean air age in yrs

# converting BP yr to normal yr & index testing
Ice_age_yr = 1950 - icecore$Ice_age_yrBp[363]
#iterating to fill in created columns with calculated normal yr
for (i in seq(dim(icecore)[1])){
  icecore$Ice_age_yr[i] =  1950 - icecore$Ice_age_yrBp[i]
  icecore$Air_age_mean_yr[i] = 1950 - icecore$Air_age_mean_yrBp[i]
}
head(icecore)
tail(icecore)

#plotting data and saving plot
#pdf('figures/Icecore_data_plot.pdf', width=7, height=5)
plot(CO2_concentration ~ Air_age_mean_yr, data=icecore, col="purple", type="l",
     xlab = "Year", ylab = "CO2 Concentration (ppmv)",
     main = "Ice Core CO2 Concentration - Vostok")
#dev.off()

# Initializing data frame for results
icecore$rolln_avg = NA # Adds a new column filled with NAs to the data
mvn_window = 10000 #moving window of 10000 yrs, can use 20000, 40000, 50000
for (i in seq(dim(icecore)[1])){
  first_window_year =icecore$Air_age_mean_yr[i]
  second_window_year =first_window_year-mvn_window # wld have been +window but the converted yrs were all-ve 
  icecore$rolln_avg [i] = mean(icecore$CO2_concentration[icecore$Air_age_mean_yr< first_window_year & icecore$Air_age_mean_yr > second_window_year])
}

################################################

icecore$rolln_avg = NA # Adds a new column filled with NAs to the data
mvn_window = 20000 #moving window of 20000 yrs, can use 10000, 40000, 50000

calc_rolln_avg = function (mvn_window){
  for (i in seq(dim(icecore)[1])){
  first_window_year =icecore$Air_age_mean_yr[i]
  second_window_year =first_window_year-mvn_window # wld have been +window but the converted yrs were all-ve 
  icecore$rolln_avg [i] = mean(icecore$CO2_concentration[icecore$Air_age_mean_yr< first_window_year & icecore$Air_age_mean_yr > second_window_year])
  }
  #return(icecore$rolln_avg)
}

calc_rolln_avg(mvn_window=20000)
################################################



#plotting rolling average and original data together and saving
#pdf('figures/Rolling_avg10000_and_Icecore_data_plot.pdf', width=7, height=5)
plot(CO2_concentration ~ Air_age_mean_yr, data=icecore, col="purple", type="l",
     xlab = "Year", ylab = "CO2 Concentration (ppmv)",
     main = "Graph of Ice Core Data")
lines(rolln_avg ~ Air_age_mean_yr, data=icecore, col="cyan4", type="l")
#dev.off()

#Accessing and reading the Mauna co2 Loa data
url = 'ftp://aftp.cmdl.noaa.gov/products/trends/co2/co2_mm_mlo.txt'
co2 = read.table(url, col.names = c("year", "month", "decimal_date",
                                    "monthly_average", "deseasonalized", "n_days",
                                    "st_dev_days", "monthly_mean_uncertainty"))

#creating 2 separate dataframes with only yr and co2 columns for the 2 data sets
Mauna_Loa_co2 = data.frame(year=co2$decimal_date, co2=co2$deseasonalized)
head(Mauna_Loa_co2)
vostok_icecore_co2 = data.frame(year=icecore$Air_age_mean_yr
                                , co2=icecore$CO2_concentration)
head(vostok_icecore_co2)#checking data 
tail(vostok_icecore_co2)

#merging (binding) the 2 dataframes along the rows
merged_co2 = data.frame(rbind(Mauna_Loa_co2, vostok_icecore_co2))
head(merged_co2) # checking created data frame
merged_co2[order(merged_co2$year), ] #making sure the merged data is in the right order

plot(co2 ~ year, data=merged_co2, col="purple", type="l",
     xlab = "Year", ylab = "CO2 Concentration (ppmv)",
     main = "Graph of Ice Core Data - Mauna Loa and Vostok")

#calculating CO2 rate for Vostok data
vostok_icecore_CO2range = max(vostok_icecore_co2$co2) - min(vostok_icecore_co2$co2)
vostok_icecore_yr_range = max(vostok_icecore_co2$year) + min(vostok_icecore_co2$year)
vostok_icecore_rate = vostok_icecore_CO2range / vostok_icecore_yr_range
#0.0002803163

#calculating C02 rate for the Mauna Loa data
Mauna_Loa_CO2_range = max(Mauna_Loa_co2$co2) - min(Mauna_Loa_co2$co2)
Mauna_Loa_yr_range = max(Mauna_Loa_co2$year) - min(Mauna_Loa_co2$year)
Mauna_Loa_rate = Mauna_Loa_CO2_range / Mauna_Loa_yr_range
#1.620326




#Addn_up = 0 # initializing the vector to add up the years
#for (i in seq(dim(icecore)[1])){
#Addn_up [i] = Addn_up[i] + icecore$Air_age_mean_yr[i]
#while (Addn_up[i] > 10000 & Addn_up[i] < 10001){
#mean(icecore$CO2_concentration[icecore$Air_age_mean_yr[Addn_up]])
#}
#rolln_avg$yr40000[i] = rolln_avg$yr40000[i] + mean(icecore$CO2_concentration[icecore$Air_age_mean_yr[i]+[i] == 10000]) 
#}



