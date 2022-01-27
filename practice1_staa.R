# Amadi 01/26/2022 MSCI 758 Unit 2 Practice Script

## Reading the data for Antarctica and Greeenland ice loss (Gt is gigatonnes)
#sigma_Gt is one standard deviation in gigatonnes
ant_ice_loss = read.table("data/antarctica_mass_200204_202111_staa.txt", skip=31,
                          sep="", header = FALSE, col.names = c("decimal_date",
                          "mass_Gt", "sigma_Gt")) # last part gives col names after reading header
grn_ice_loss = read.table("data/greenland_mass_200204_202111_staa.txt", skip=31,
                          sep="", header = FALSE, col.names = c("decimal_date",
                          "mass_Gt", "sigma_Gt")) # Greenland ice mass loss


#Viewing data and getting general info abt data
# View(grn_ice_loss)
head(ant_ice_loss)#returned the first 6 rows of the table/data frame
head(grn_ice_loss)
dim(ant_ice_loss)
summary(ant_ice_loss)



#Plotting data using 'y ~ x'
# Plot Antarctic ice mass loss
plot(mass_Gt ~ decimal_date, data=ant_ice_loss, ylab="Antarctica Mass Loss (Gt)")
# Plot Greenland ice mass loss
plot(mass_Gt ~ decimal_date, data=grn_ice_loss, ylab="Greenland Mass Loss (Gt)")
# Plot both together as lines
plot(mass_Gt ~ decimal_date, data=ant_ice_loss, ylab="Ice Sheet Mass Loss (Gt)", type='l') 
lines(mass_Gt ~ decimal_date, data=grn_ice_loss, col='cyan') 

#Using '+' to link multiple plots and print to the same window
plot(mass_Gt ~ decimal_date, data=ant_ice_loss, ylab="Antarctica Mass Loss (Gt)", type='l', ylim=range(grn_ice_loss$mass_Gt)) +
  lines(mass_Gt ~ decimal_date, data=grn_ice_loss, type='l', col='cyan')

#Create data.frame with an NA between the GRACE missions. Column names must match so it will merge with ant and grn data
data_break = data.frame(decimal_date=2018.0, mass_Gt=NA, sigma_Gt=NA)
data_break



### Plotting to account for years with missing data
#### insert an `NA` into the data.frame at the start of 2018. 
#Then add in grey shaded polygons behind it to show the uncertainty
#Add NA data point to the Antarctica ice trends data frame
ant_ice_loss_with_NA = rbind(ant_ice_loss, data_break) # Merge ant_ice_loss data frame with our NA point
tail(ant_ice_loss_with_NA) # Our NA value in 2018 is now the last row of our data frame
order(ant_ice_loss_with_NA$decimal_date) # Use order() to list the indices of the data after its sorted from smallest to biggest
ant_ice_loss_with_NA = ant_ice_loss_with_NA[order(ant_ice_loss_with_NA$decimal_date),] # Reorder the data frame, sorting by date

#Repeating with Greenland data.frame
grn_ice_loss_with_NA = rbind(grn_ice_loss, data_break) # Merge grn_ice_loss data frame with our NA point
grn_ice_loss_with_NA = grn_ice_loss_with_NA[order(grn_ice_loss_with_NA$decimal_date),]

#Replotting to show the gap
plot(mass_Gt ~ decimal_date, data=ant_ice_loss_with_NA, ylab="Antarctica Mass Loss (Gt)", type='l', ylim=range(grn_ice_loss_with_NA$mass_Gt, na.rm=TRUE)) +
  lines(mass_Gt ~ decimal_date, data=grn_ice_loss_with_NA, type='l', col='cyan')

#


