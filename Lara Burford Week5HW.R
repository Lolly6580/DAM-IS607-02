#1. Write down 3 questions that you might want to answer based on this data.
# 1) What is the total number of people over age 25 that prefer Cullen skink over Partan bree?
# 2) Which soup do most people in Glasgow prefer?
# 3) What percentage of all people polled prefer Partan bree over Cullen skink?

#2. Create an R data frame with 2 observations to store this data in its current "messy" state.  Use whatever method you want to re-create and/or load the data.
answer <- c("Yes","No")
results <- data.frame(answer)

Under25_1 <- c(80100,35900)
Over25_1 <- c(143000,214800)
edinburgh <- data.frame(Under25_1,Over25_1)

Under25_2 <- c(99400,43000)
Over25_2 <- c(150400,207000)
glasgow <- data.frame(Under25_2,Over25_2)

poll_results <- cbind(answer, edinburgh, glasgow)
colnames(poll_results) <- c("Yes_No", "Edinburgh 16-24","Edinburgh 25+", "Glasgow 16-24", "Glasgow 25+")

#3. Use the functionality in the tidyr package to convert the data frame to be "tidy data".
require(tidyr)
require(dplyr)
poll_results2 <- poll_results %>%
  gather('City_Age','Results', 2:5) 
poll_results3 <- poll_results2 %>%
  separate("City_Age", c("City","Age"), sep = " ")

#4. Use the functionality in the plyr package to answer the questions that you asked in step 1.
require(plyr)
ddply(poll_results3, c("Yes_No", "Age"), summarise, Total_Yes = sum(Results))
#1) 293400
ddply(poll_results3, c("City","Yes_No"), summarize, Total = sum(Results))
#2) Partan bree (because more people answered No in Glasgow than yes)
ddply(poll_results3, c("Yes_No"), mutate, Percent = sum(Results)/sum(poll_results3$Results))
#3) 51.4% prefer Partan bree

#5. Having gone through the process, would you ask different questions and/or change the way that you structured your data frame?
#I may have asked more specific questions.  I chose questions that I was sure I could figure out how to answer, but once I figured out 
#how to use the ddply function, I realized that I could have answered much more specific questions about the data just as easily..
#For example: in my third question, I asked the percentage of total people that preferred Partan bree, but I could have used this function:
ddply(poll_results3, c("Yes_No", "City", "Age"), mutate, Percent = sum(Results)/sum(poll_results3$Results))
#and answered the question "What percentage of people polled in Edinburgh over the age of 25 preferred Partan bree?" (Answer: 22%)
#I wouldn't change the way I formatted the data as a data frame.  It seemed very simple to use tidyr and plyr on that format.