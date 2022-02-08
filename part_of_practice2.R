# boolean logic 02/01/2022

vec = c(1, 0, 2, 1)
vec[c(FALSE, TRUE, TRUE, FALSE)]

x = 5
x %in% vec
world_oceans = data.frame(oceans = c("Atlantic", "Pacific", "Indian", "Arctic", "Southern"),
                          area_km2 = c(77e6, 156e6, 69e6, 14e6, 20e6),
                          avg_depth_m = c(3926, 4028, 3963, 3953, 4500))

head(world_oceans)
world_oceans$avg_depth_m > 4000

world_oceans[world_oceans$avg_depth_m > 4000, 'ocean']
sum(world_oceans$avg_depth_m > 4000) # r assigns 1 true and zero to false

error_threshold = 0.0000001
abs(0.3 - (0.1+0.2 < error_threshold))

world_oceans[world_oceans$avg_depth_m > 4000 & world_oceans$area_km2 <50e6, ]

#Querying NAs in your data
vec = c(1,2,3,NA)
vec==NA
is.na(vec)
any(is.na(vec))
all(is.na(vec))

#if and else statement

