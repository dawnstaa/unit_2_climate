# Amadi 01/26/2022 MSCI 758 Unit 2 Practice Script
#And some parts with Erin

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
tail(ant_ice_loss)#returned the last 6 rows of the table/data frame
head(grn_ice_loss)
dim(ant_ice_loss)
summary(ant_ice_loss)
summary(grn_ice_loss)



#Plotting data using 'y ~ x'
# Plot Antarctic ice mass loss
plot(mass_Gt ~ decimal_date, data=ant_ice_loss, ylab="Antarctica Mass Loss (Gt)")
# Plot Greenland ice mass loss
#plot(x=decimal_date, y=mass_Gt, data=ant_ice_loss, ylab="Antarctica Mass Loss (Gt)")
plot(mass_Gt ~ decimal_date, data=grn_ice_loss, ylab="Greenland Mass Loss (Gt)")
# Plot both together as lines
plot(mass_Gt ~ decimal_date, data=ant_ice_loss, ylab="Ice Sheet Mass Loss (Gt)", type='l') 
lines(mass_Gt ~ decimal_date, data=grn_ice_loss, col='cyan') 

#Using '+' to link multiple plots and print to the same window
min(grn_ice_loss$mass_Gt)
range(grn_ice_loss$mass_Gt) # can do this or that below
#plot(mass_Gt ~ decimal_date, data=ant_ice_loss, ylab="Antarctica Mass Loss (Gt)", type='l', ylim=c(-5191, 0)) +lines(mass_Gt ~ decimal_date, data=grn_ice_loss, type='l', col='cyan')
plot(mass_Gt ~ decimal_date, data=ant_ice_loss, ylab="Antarctica Mass Loss (Gt)", type='l', ylim=range(grn_ice_loss$mass_Gt)) +
  lines(mass_Gt ~ decimal_date, data=grn_ice_loss, type='l', col='cyan')

#Create data.frame with an NA between the GRACE missions. Column names must match so it will merge with ant and grn data
data_break = data.frame(decimal_date=2018.0, mass_Gt=NA, sigma_Gt=NA)
data_break



### Plotting to account for years with missing data
#### insert an `NA` into the data.frame at the start of 2018. 
#Then add in grey shaded polygons behind it to show the uncertainty
#Add NA data point to the Antarctica ice trends data frame
ant_ice_loss_with_NA = rbind(ant_ice_loss, data_break) # combine alng the rows or Merge ant_ice_loss data frame with our NA point
tail(ant_ice_loss_with_NA) # Our NA value in 2018 is now the last row of our data frame
order(ant_ice_loss_with_NA$decimal_date) # rit showed us how to sort the data i.e. Use order() to list the indices of the data after its sorted from smallest to biggest
ant_ice_loss_with_NA = ant_ice_loss_with_NA[order(ant_ice_loss_with_NA$decimal_date),] # Reorder the data frame, sorting by date



#Repeating with Greenland data.frame
grn_ice_loss_with_NA = rbind(grn_ice_loss, data_break) # Merge grn_ice_loss data frame with our NA point
grn_ice_loss_with_NA = grn_ice_loss_with_NA[order(grn_ice_loss_with_NA$decimal_date),]

#Replotting to show the gap
plot(mass_Gt ~ decimal_date, data=ant_ice_loss_with_NA, ylab="Antarctica Mass Loss (Gt)", type='l',
     ylim=range(grn_ice_loss_with_NA$mass_Gt, na.rm=TRUE)) +
  lines(mass_Gt ~ decimal_date, data=grn_ice_loss_with_NA, type='l', col='cyan')


### Plotting C.I around data
#### Sometimes we show errors as "2-sigma", where 2 X sigma represents 95% uncertainty. 
# We can plot 95% confidence intervals around the data by adding and subtracting 2*sigma 
#from the mass loss estimates

#Antarctica
head(ant_ice_loss_with_NA)
plot(mass_Gt ~ decimal_date, data=ant_ice_loss_with_NA,
     ylab="Antarctica Mass Loss (Gt)", xlab="Year", type='l') +
  lines((mass_Gt+2*sigma_Gt) ~ decimal_date, data=ant_ice_loss_with_NA, type='l', lty='dashed') +
  lines((mass_Gt-2*sigma_Gt) ~ decimal_date, data=ant_ice_loss_with_NA, type='l', lty='dashed') 


#*** Exercise 1.1 Creating the above C.I for Greenland
#Greenland Mass loss with 95% confidence interval
head(grn_ice_loss_with_NA)
plot(mass_Gt ~ decimal_date, data=grn_ice_loss_with_NA, ylab="Greenland Mass Loss (Gt)", xlab="Year", type='l') +
  lines((mass_Gt+2*sigma_Gt) ~ decimal_date, data=grn_ice_loss_with_NA, type='l', lty='dashed') +
  lines((mass_Gt-2*sigma_Gt) ~ decimal_date, data=grn_ice_loss_with_NA, type='l', lty='dashed')

#Combining Antarctic and Greenland plots into one and saving in a pdf
pdf('figures/ice_mass_trends.pdf', width=7, height=5)
plot(mass_Gt ~ decimal_date, data=ant_ice_loss_with_NA, ylab="Ice Sheet Mass Loss (Gt)", xlab="Year", type='l',
     ylim=range(grn_ice_loss_with_NA$mass_Gt, na.rm=TRUE), lwd=2) +
  lines((mass_Gt+2*sigma_Gt) ~ decimal_date, data=ant_ice_loss_with_NA, type='l', lty='dashed') +
  lines((mass_Gt-2*sigma_Gt) ~ decimal_date, data=ant_ice_loss_with_NA, type='l', lty='dashed') +
  lines(mass_Gt ~ decimal_date, data=grn_ice_loss_with_NA, type='l', col='cyan', lwd=2) +
  lines((mass_Gt+2*sigma_Gt) ~ decimal_date, data=grn_ice_loss_with_NA, type='l', lty='dashed', col="cyan") +
  lines((mass_Gt-2*sigma_Gt) ~ decimal_date, data=grn_ice_loss_with_NA, type='l', lty='dashed', col="cyan") 
dev.off()

### Bar plot showing the difference in ice mass loss for Antarctica and Greenland
# Largest observed decrease in ice mass loss in Antarctica:
min(ant_ice_loss$mass_Gt)

# Barplot of largest observed ice loss in Antarctica and Greenland
barplot(height=c(min(ant_ice_loss$mass_Gt), min(grn_ice_loss$mass_Gt)))

# Flip to negative to positive, add x-axis labels, add more tick marks on y-axis, add y-axis title
barplot(height=c(min(ant_ice_loss$mass_Gt)*(-1), min(grn_ice_loss$mass_Gt)*(-1)), names.arg=c("Antarctica","Greenland"), ylim=c(0,5000), ylab="Ice loss in Gt")



