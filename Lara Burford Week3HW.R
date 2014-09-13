#1. Write a function that takes a vector as input and returns the number of missing values in the vector.
missingCount <- function(vector1)
{
  vector1 <- vector1[is.na(vector1)]
  count <- length(vector1)
  return(count)
}

V1 <- c(24,16,NA,11,NA,5,24)
missingCount(V1)

#2. Write a function that takes a data frame as input and returns a named vector with the number of missing values in each column of the data frame. (The names of the entries should be the corresponding column names of the data frame.) You may use the function from the previous question as part of your solution.
missingCount2 <- function(dataFrame1)
{
  countList <- c()
  for(i in 1:ncol(dataFrame1))
  {
    count <- missingCount(dataFrame1[i])
    countList <- c(countList,count)
  }
  names(countList) <- names(dataFrame1)
  return(countList)
}

v2 <- c(27,19,NA,10,NA,5,24)
v3 <- c(NA,16,NA,11,NA,5,24)
v4 <- c(NA,NA,NA,8,NA,5,24)
df1 <- data.frame(Set1 = v2,Set2 = v3,Set3 = v4)
Missing_Values <- missingCount2(df1)

#3. Write a function that takes a numeric vector as input and uses it to determine the minimum, the maximum, the mean, the median, the first quartile, the third quartile, the standard deviation of the vector, and the number of missing values. Do not use any built-in functions to do this. Return a named list with the eight desired values in an order you deem best. (You may, if you like, use the function you wrote for question 1.)
specFunction <- function(numVector)
{
  missingval <- missingCount(numVector)
  i <- 1
  numVector <- numVector[!is.na(numVector)]
  minimum <- numVector[i]
  maximum <- numVector[i]
  sorted <- numVector
  sum <- 0
  for(i in 1:length(numVector))
  {
    if(numVector[i] < minimum)
    {
      minimum <- numVector[i]
    }
    if(numVector[i] > maximum)
    {
      maximum <- numVector[i]
    }
    sum <- sum + numVector[i]
  }
  mean <- sum/length(numVector)
  j <- 1
  while(j < length(numVector))
  {
    if(sorted[j] > sorted[j+1])
    {
      value <- sorted[j]
      sorted[j] <- sorted[j+1]
      sorted[j+1] <- value
      j <- 1
    }
    else
    {
      j <- j + 1
    }
  }
  median <- sorted[(length(sorted)/2)+1]
  sorted2 <- subset(sorted,sorted <= median)
  first <- sorted2[(length(sorted2)/2)+1]
  sorted3 <- subset(sorted,sorted >= median)
  third <- sorted3[(length(sorted3)/2)+1]
  standev <- mean^(1/2)
  specList <- list(Minimum = minimum, Maximum = maximum, Mean = mean, First_Quartile = first, Third_Quartile = third, Median = median, Standard_Deviation = standev, Missing_Values = missingval)
  return(specList)
}

specFunction(V1)

#4. Write a function that takes a character or factor vector and determines the number of distinct elements in the vector, the most commonly occurring element, the number of times the most commonly occurring element occurs, and the number of missing values. (Be sure to handle ties gracefully.) Have the function return a named list with the desired information in a logical order.
distinctFunction <- function(vector1)
{
  
  distinct <- length(unique(vector1[!is.na(vector1)]))
  missing <- missingCount(vector1)
  counts <- as.data.frame(table(vector1))
  counts2 <- as.matrix(counts[2])
  highest <- max(counts2)
  element <- NA
  if(sum(counts2 == highest) == 1)
  {
    index <- which.max(counts2)
    element <- counts[[1]][index]
    specList2 <- list(Distinct_Elements = distinct, Element = element, Frequency = highest, Missing_Value = missing)
  }
  else
  {
    for(i in 1:length(counts[[1]]))
    {
      if(highest == counts2[i])
      {
        element <- c(element,as.character(counts[[1]][i]))
        element <- element[!is.na(element)]
      }
    }
    specList2 <- list(Distinct_Elements = distinct, Elements = element, Frequency = highest, Missing_Value = missing)
  }
  
  return(specList2)
}

V5 <- c("circle",NA,NA, "square","circle", "square","octagon", "square",NA,"triangle","circle","triangle","square","circle")
distinctFunction(V5)
V6 <- c("circle",NA,NA, "square", "square","octagon", "square",NA,"triangle","circle","triangle","square","circle")
distinctFunction(V6)

#5. Write a function that takes a logical vector and determines the number of true values, the number of false values, the proportion of true values, and the number of missing values. Have the function return a named list with the desired information in a logical order.
booleanFunction <- function(vector1)
{
  missing <- missingCount(vector1)
  vector2 <- vector1[!is.na(vector1)]
  truecount <- table(vector2)["TRUE"]
  falsecount <- table(vector2)["FALSE"]
  trueprop <- truecount/length(vector2)
  specList3 <- list(True_count = truecount, False_Count = falsecount, True_proportion = trueprop, Missing_Value = missing)
  return(specList3)
}

V7 <- c(TRUE, FALSE, FALSE, FALSE, TRUE, NA, NA, TRUE, TRUE, TRUE, TRUE)
booleanFunction(V7)

#6. Write a function that takes as its input a data frame and returns a summary of its columns using the functions you write for questions 3-5. You may assume that all columns will be of the three types in those questions. You are expected to use the functions you have written in the previous questions, so you do not have to write them again by scratch. Return the desired information in a format that you deem best. (One suggestion would be a named list of lists, but I leave it to your judgment.)
summaryFunction <- function(df1)
{
  Vsum <- list()
  summaryList <- list()
  
  for(i in 1:length(df1))
  {
    if(is.numeric(df1[[i]]) == TRUE)
    {
      Vsum <- specFunction(df1[i])
      names(Vsum) <- "Numbers"
      summaryList[[length(summaryList)+1]] <- Vsum
      
    }
    else if(is.logical(df1[[i]]) == TRUE)
    {
      Vsum <- booleanFunction(df1[i])
      names(Vsum) <- "Boolean"
      summaryList[[length(summaryList)+1]] <- Vsum
      
    }
    else
    {
      Vsum <- distinctFunction(df1[i])
      names(Vsum) <- "Character or Factor"
      summaryList[[length(summaryList)+1]] <- Vsum
      
    }
  }
  return(summaryList)
}

V1 <- c(24,16,NA,11,NA,5,24,7,23,20,18,18,11)
V2 <- c(TRUE, FALSE, FALSE, FALSE, TRUE, NA, NA, TRUE, TRUE, TRUE, TRUE,TRUE,TRUE)
V3 <- c("circle",NA,NA, "square", "square","octagon", "square",NA,"triangle","circle","triangle","square","circle")
dataframeX <- data.frame(Numbers = V1, Shapes = V2, Boolean = V3)
final <- summaryFunction(dataframeX)
