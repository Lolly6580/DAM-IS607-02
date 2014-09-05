#1. Create a vector that contains 20 numbers. (You may choose whatever numbers you like, but make sure there are some duplicates.)
v <- c(7,5,3,4,13,24,9,11,12,4,7,9,10,14,3,19,15,18,20,23)

#2. Use R to convert the vector from question 1 into a character vector.
vChar <- as.character(v)

#3. Use R to convert the vector from question 1 into a vector of factors.
vFactor <- as.factor(v)

#4. Use R to show how many levels the vector in the previous question has.
nlevels(vFactor)

#5. Use R to create a vector that takes the vector from question 1 and performs on it the formula 3x^2???4x+1.
fResult <- 3*(v^2)-(4*v)+1

#6. Implement ordinary least-squares regression in matrix form: (t(X)X)^-1)t(X)y. As a useful double check you should be able to run your regression on the matrices X and y to get b below:
X <- matrix(c(1,1,1,1,1,1,1,1,5,4,6,2,3,2,7,8,8,9,4,7,4,9,6,4),8)
y <- matrix(c(45.2,46.9,31.0,35.3,25.0,43.1,41.0,35.1),8)
r <- lm(formula = y ~ X)
r

#7. Create a named list. That is, create a list with several elements that are each able to be referenced by name.
xList <- list(TheFactorVector = vFactor, TheCharacterVector = vChar, TheNumberVector = v)

#8. Create a data frame with four columns - one each character, factor (with three levels), numeric, and date. Your data frame should have at least 10 observations (rows).
aChar <- as.character(c("A","B","C","D","E","F","G","H","I","J"))
aFactor <- as.factor(c("Green","Green","Yellow","Red","Red","Green","Red","Green","Yellow","Yellow"))
aNum <- 1:10
aDate <- as.Date(c("6/5/1980","6/27/74","5/11/1995","6/21/2009","9/20/2011","12/28/2012","7/19/2003","6/19/2001","9/3/2014","1/2/2010"),"%m/%d/%Y")
aDF <- data.frame(aChar, aFactor, aNum, aDate)

#9. Illustrate how to add a row with a value for the factor column that isn't already in the list of levels. (Note: You do not need to accomplish this with a single line of code.)
aChar <- c(aChar,"A")
aFactor <- factor(aFactor, levels = c(levels(aFactor), "Blue"))
aFactor[11] <- factor("Blue")
aNum <- c(aNum,11)
aDate <- as.Date(c(aDate,"6/5/1980"),"%m/%d/%Y")
aDF <- data.frame(aChar, aFactor, aNum, aDate)

#10. Show the code that would read in a CSV file called temperatures.csv from the current working directory.
setwd("/")
temperature <- read.table(file = "temperatures.csv", header = TRUE)

#11. Show the code that would read in a TSV file called measurements.txt from a directory other than the working directory on your local machine.
location <- "C:/Users/lburford/Documents/measurements.txt"
measurement <- read.table(file = location, header = TRUE)

#12. Show the code that will read in a delimited file with a pipe separator (the "|" symbol) from a website location. (You may make up an appropriate URL.)
location <- "http://samplecsvs.s3.amazonaws.com/Sacramentorealestatetransactions.csv"
transactions <- read.table(file = location, header = TRUE, sep = "|")

#13. Write a loop that calculates 12-factorial.
factorial <- 1
for (i in 1:12)
{
  factorial <- factorial * i
}
factorial

#14. Use a loop to calculate the final balance, rounded to the nearest cent, in an account that earns 3.24% interest compounded monthly after six years if the original balance is $1,500.
balance <- 1500
x <- 1
while(x <= (6*12))
{
  balance <- balance * (1 + 0.0324/12)
  x <- x + 1
}
balance <- round(balance,digits=2)
balance

#15. Create a numeric vector of length 20 and then write code to calculate the sum of every third element of the vector you have created.
vNum <- c(1:20)
total <- 0
for(i in 1:length(vNum))
{
  if(vNum[i]%%3 == 0)
    total <- total + vNum[i]
}

#16. Use a for loop to calculate E(10 to i=1) x^i for the value x=2.
x <- 2
total <- 0
for(i in 1:10)
  total <- total + (x^i)
total

#17. Use a while loop to accomplish the same task as in the previous exercise.
x <- 2
total <- 0
i <- 1
while(i <= 10)
{
  total <- total + (x^i)
  i <- i + 1
}
  total

#18. Solve the problem from the previous two exercises without using a loop.
x <- 2
range <- c(1:10)
range <- x^range
total <- sum(range)
total

#19. Show how to create a numeric vector that contains the sequence from 20 to 50 by 5.
fiveSequence <- seq(20, 50,by = 5)
fiveSequence

#20. Show how to create a character vector of length 10 with the same word, "example", ten times.
repeatExample <- c(rep("example", 10))
repeatExample

#21. Show how to take a trio of input numbers a, b, and c and implement the quadratic equation.
a <- 1
b <- 3
c <- -4

x1 <- (-b + sqrt((b^2)-(4*a*c)))/(2*a)
x2 <- (-b - sqrt((b^2)-(4*a*c)))/(2*a)
print(c(x1,x2))
