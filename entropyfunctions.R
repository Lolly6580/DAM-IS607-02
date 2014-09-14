#1. Create a function entropy()
entropy <- function(inputVector)
{
  disVector <- unique(inputVector[!is.na(inputVector)])
  distinct <- length(disVector)
  count <- 0
  probability <- 0
  entropy <- 0
  for(i in 1:distinct)
  {
    count <- sum(inputVector == disVector[i])
    probability <- count/length(inputVector)
    if(probability == 0)
    {
      entropy <- entropy + 0
    }
    else
    {
      entropy <- entropy + (probability*(log2(probability)))
    }
  } 
  entropy <- entropy*-1
  return(entropy)
}

#2. Create a function infogain()
infogain <- function(inputVector1, inputVector2)
{
  disVector <- unique(inputVector2[!is.na(inputVector2)])
  distinct <- length(disVector)
  count <- 0
  info <- c()
  groupa <- c()
  j <- 1
  k <- 1
  
  for(j in 1:distinct)
  {
    for(k in 1:length(inputVector2))
    {
      if(inputVector2[k] == disVector[j])
      {
        groupa <- c(groupa,inputVector1[k])    
      }
    }
    count <- length(groupa)
    probability <- count/length(inputVector1) 
    entropy <- entropy(groupa)  
    info <- c(info,probability*entropy)
    groupa <- c()
    k <- 1
  }
  information <- (entropy(inputVector1) - sum(info))
  
  return(information)
}

#3. Create a function decide()
decide <- function(inputDF, inputTarget)
{
  decision <- list(max=NA, gains=NA)
  gainList <- data.frame()
  for(i in 1:length(inputDF))
  {
    if(i == inputTarget)
    {
      i <= i + 1
    }
    else
    {
       gainList[1,i] <- infogain(inputDF[,inputTarget],inputDF[,i])
       colnames(gainList)[i] <- names(inputDF)[i]
    }
  }
  max <- which.max(gainList)
  decision["max"] <- max
  decision["gains"] <- list(gainList)
  
  return(decision)
}

##Testing
location <- "C:/Users/lburford/Documents/Personal/entropy-test-file.csv"
dataset <- read.table(file = location, header = TRUE, sep = ",")

entropy(dataset$answer)

infogain(dataset$answer,dataset$attr1)
infogain(dataset$answer,dataset$attr2)
infogain(dataset$answer,dataset$attr3)

decide(dataset,4)

