#1. Write a function that takes a numeric vector and calculates the mean of the observations in the vector.
meanFunction <-function(numVector)
{
    mean <- sum(numVector)/length(numVector)
    return(mean)
}
V1 <- c(24,16,33,11,8,5,24)
meanFunction(V1)

#2. Modify your function in the previous question so that it can handle a numeric vector with missing values.
meanFunction <- function(numVector)
{
  numVector <- numVector[!is.na(numVector)]
  mean <- sum(numVector)/length(numVector)
  return(mean)
}

V2 <- c(24,16,NA,11,NA,5,24)
meanFunction(V2)

#3. Write a function that takes two numeric input values and calculates the greatest common divisor of the two numbers.
gcdFunction <- function(num1,num2)
{
  if (num1 > num2)
  {
    dividend <- num1
    divisor <- num2
  }
  else
  {
    dividend <- num2
    divisor <- num1
  } 
  
  remainder = 1
  while (remainder != 0)
  {
    remainder <- dividend%%divisor
    if(remainder == 0)
      gcd = divisor
    dividend <- divisor
    divisor <- remainder 
  }
  
  return(gcd)
}

gcdFunction(14,64)

#4. Write a function that implements Euclid's algorithm (you may need to do a bit of research to find this algorithm) for finding the greatest common divisor of two numeric inputs.
euclidFunction <- function(num1,num2)
{
  while (num1 != num2)
  {
    if (num1 > num2)
      num1 <- num1 ??? num2
    else
      num2 <- num2 ??? num1
  }
  return(num1)
}

euclidFunction(14,64)

#5. Write a function that takes two numeric inputs x and y and calculates.
formulaFunction <- function(x,y)
{
  answer <- ((x*x)*y) + (2*x*y) - (x*(y*y))
  return(answer)
}
formulaFunction(3,4)

#6. Read in the week-3-price-data.csv and week-3-make-model-data.csv files as data frames and then merge them by the ModelNumber key. Leave the "all" parameters as their defaults. How many observations end up in the result? Is this what you would have expected?
location <- "C:/Users/lburford/Documents/Personal/week-3-price-data.csv"
price <- read.table(file = location, header = TRUE, sep = ",")
location <- "C:/Users/lburford/Documents/Personal/week-3-make-model-data.csv"
makeModel <- read.table(file = location, header = TRUE, sep = ",")
carData1 <- merge(price,makeModel,by= "ModelNumber")
#27 obs. of 8 variables
#no, I would expect to see 28 obs. since the price table has 28 obs., but it looks like there is a typo in on of the model number fields so there is no match for 23120

#7. Use the data sets from the previous question, but this time merge them so that the rows from the price-data table all appear, even if there is no match in the make-model table.
carData2 <- merge(price,makeModel,by= "ModelNumber",all=TRUE)

#8. Take your result from question 7 and subset it so that only the 2010 vehicles are included.
carData3 <- subset(carData2, Year == 2010) 

#9. Take your result from question 7 and subset it so that only the red cars that cost more than $10,000 are included.
carData4 <- subset(carData2, Color == "Red" & Price > 10000)
                  
#10. Take your result from question 9 and subset it so that the ModelNumber and Color columns are removed.
carData5 <- subset(carData4, select=c(ID, Mileage, Price, Make, Model, Year))

#11. Write a function that takes as input a character vector and returns a numeric vector with the numbers of characters in each of the elements in the original vector.
charToNum <- function(charVector)
{
  numVector <- as.numeric(nchar(charVector))  
  return(numVector)
}

v3 <- as.character(c("January", "February", "March", "April", "May", "June","August","September","October","November","December"))
charToNum(v3)

#12. Write a function that takes two character vectors of equal length and concatenates them element by element with a space as the separator. Have the function die gracefully if the vectors are the same length.
concatFunction <- function(charVec1,charVec2)
{
  charVec3 = c()
  if(length(charVec1) == length(charVec2))
  {
    for (i in 0:length(charVec1))
      charVec3 <- c(charVec3,paste(charVec1[i],charVec2[i],sep=" "))
    return(charVec3)
  }
  else
    print ("The two vectors given are not the same length.")
}

V4 <- c("Sun","Mon","Tues","Wednes","Thurs","Fri","Satur")
v5 <- c(rep("day", 7))
v6 <- c("Sun","Mon","Tues")
concatFunction(V4,v5)
concatFunction(v6,v5)

#13. Write a function that takes a character vector and returns the substring of three characters that begins with the first vowel in the string. Have the function handle gracefully substrings where this isn't possible.
subString <- function(charVec)
{
  vowel <- c("a","e","i","o","u")
  output <- NA
  k <- 1
  i <- 1
  while(i <= length(charVec))
  {
      split <- strsplit(charVec[i],"")
      while(k <= length(split[[1]]))
      {
          if(split[[1]][k] %in% vowel)
          {
            x <- k
            
            if(length(split[[1]]) > 3)
            {
              add <- substr(charVec[i],x,x+2)
              output <- c(output,add)
              k <- (length(split[[1]]) + 1)
            }
            else
            {
              add <- "NA"
              output <- c(output,add)
              k <- (length(split[[1]]) + 1)
            }
          } 
          else
          {
            k <- k+1
          }
          
      }  
      i <- i + 1
      k <- 1
  }
  output <- output[!is.na(output)]
  return (output)
}

v7 <- c("fish","monkey", "cat", "turtle","elephant","rabbit","giraffe")
subString(v7)

#14. Suppose you have a data frame where one column gives the month (in numeric format), the next gives the day, and the third column gives the year. Use R to create such a data frame (by hand is fine) and then add a fourth column with the date in date format.
months <- c(1:10)
days <- c(15:24)
years <- c(rep(2014,10))
i <- 1
dfDates <- data.frame(month=months,day=days,year=years)
formatDate <- as.Date(NA)
for(i in 1:nrow(dfDates))
{
  formatDate <- c(formatDate, as.Date(paste(months[i],days[i],years[i],sep="-"),"%m-%d-%Y"))
}
dfDates[,"date"] <- formatDate[!is.na(formatDate)]

#15. Illustrate the code necessary to take a string of MM-DD-YYYY format and convert it to a date.
aDate <- as.Date(c("6-5-1980"),"%m-%d-%Y")

#16. Illustrate the code necessary to take a date and extract the month of the date.
aMonth <- format(aDate,format="%B")

#17. Create a sequence of all of the dates from January 1, 2005, to December 31, 2014.
start <- as.Date(c("1-1-2005"),"%m-%d-%Y")
end <- as.Date(c("12-31-2014"),"%m-%d-%Y")
dateList <- seq(start, end,"days")


