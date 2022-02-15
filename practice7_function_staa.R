#Amadi 02/10/2022 with Erin
#User defined functions

vec = c(1,3,5,7)

avg = function(x){
  sum(x)/length(x)# a function automatically returns the value produced in the last line
  # return(sum/length) # or you can explicitly return an object with the return() function
}

avg(vec)
mean(vec)



avg = function(x){
  sum_x = sum(x)
  length_x = length(x)
  avg = sum_x/length_x
  return(avg)#explicitly return an object with the return() function
}
my_result = avg(vec)
my_result


#add geometric mean to my function
avg = function(x, arithmetic=TRUE){
  length_x = length(x)
  result = ifelse(arithmetic, sum(x)/length_x, prod(x)^(1/length_x))
  return(result)
}

avg(vec)
avg(vec, arithmetic=FALSE) # takes geometric mean



avg = function(x, arithmetic=TRUE){
  if(!is.numeric(x)){stop("avg() failed. x must be numeric.")}
  length_x = length(x)
  result = ifelse(arithmetic, sum(x)/length_x, prod(x)^(1/length_x))
  return(result)
}

avg(FALSE)

dat = c(1,3,5,7)
avg(dat)
avg(dat, TRUE) # Same answer as above since TRUE is the default
avg(x=dat, arithmetic=TRUE)  # here we make our selection of the default parameter explicit
avg(x=dat, arithmetic=FALSE) 


avg()  # didn't include the required parameter x
avg(FALSE, dat)  # reversed the order of the parameters without explicitly naming the parameters
avg(FALSE)

#function name
# Too short
f()
# Not a verb, or descriptive
my_awesome_function()
# Long, but clear
remove_na()
plot_time_series()


p = 96
# Exercise 7.1 grade function
grade = function (percent)
  if (grade >= 90){
    grade = A
    if (grade < 90){
      grade = B
    }
  } 
grade(percent)




