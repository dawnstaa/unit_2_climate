#Amadi 02/05/2020

#For loops
for (i in c(1:4)) {
  print("one run")
}


#or
for (value in c("My", "second", "for", "loop")) {
  print(value)
}

#or another example - initializing the results vector
my_vector = c(1,3,5,2,4)
n = length(my_vector)
my_vector_squared = rep(NA, n)#initialising the results vector
for (i in seq(n)) {
  my_vector_squared[i] = my_vector[i]^2
}
my_vector_squared

#initializing the results variable
vec = c(1,3,5,7)
total = 0 #initializing the results variable
for (i in seq(length(vec))){
  total = total + vec[i]
  print (total)
}

vec[length(vec)]
tail(vec,1)


## Exercise 5.1 - finding the factorial of a number ##
#num = 3
num = 6
vect = seq(num)
num_factorial = 1 #initializing a results variable for the factorial
for (i in vect){
  num_factorial = num_factorial * i
  #print(num_factorial)
}
num_factorial




#Nested for loops
mat = matrix(c(4,0,8,7,3,-2), nrow = 2, ncol = 3)
mat_squared = matrix(rep(NA, length(mat)), nrow = 2, ncol = 3)# results matrix
# writing the for loop to loop through the rows (i) and cols (j) - 2rows,3cols
for(i in c(1:2)){
  for(j in c(1:3)){
    mat_squared[i,j] = mat[i,j]^2
  }
}

# to visualize how R accesses the rows and cols in the loop
for(i in c(1:2)){
  for(j in c(1:3)){
    print(paste("row i=",i," and column j=",j)) 
  }
}

#but more efficient to do this cos mos computations & functions are vectorized
my_vector^2
mat^2

#while loop - the while loop codition must be a logical test, if'TRUE' code runs
#If 'FALSE' loop compltes
num = -2
while(num < 0){
  print("One interation of this loop")
  num = num+1
}

#varying number of iterations - fishing in a pond with 50 pound (22.7kg) catch limit

total_catch_lb = 0  # track the weight of fish caught
n_fish = 0   # track the number of fish caught
while (total_catch_lb < 50){
  n_fish = n_fish+1  # I caught another fish!
  new_fish_weight = rnorm(n=1, mean=2, sd=1) # randomly generate the weight of a fish
  total_catch_lb = total_catch_lb + new_fish_weight # calculate weight of all fish caught so far
}
n_fish
total_catch_lb


#Exercise 5.2
num_loop = seq(7)





