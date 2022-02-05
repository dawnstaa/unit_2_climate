#temp_degC = 36.9  #no fever (Temperature in degrees Celsius)
#temp_degC = 37.5  #low fever
temp_degC = 38.9  #high fever

median_human_temp = 37 # median human temperature

#checking for fever
if (temp_degC > 37.4){
  diff_frm_median = temp_degC - median_human_temp # difference from median temp
  print(diff_frm_median)
  # Checking if fever is high
  if (temp_degC > 38.3){
    print ("Oh no! High fever!")
  } 
}
temp_degC

#2nd method - using the diff 4rm the median
if (temp_degC > 37.4){
  diff_frm_median = temp_degC - median_human_temp  #difference from median temp
  print(diff_frm_median)
  # Checking if fever is high
  if (diff_frm_median > 1.8){
    print ("Oh no! High fever!")
  } 
}
temp_degC