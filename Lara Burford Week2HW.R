#1. Suppose that you have five customers - James, Mary, Steve, Alex, and Patricia - in line at a store. Use R operations to perform the following tasks in sequence. 
#a. Assign the five individuals to a vector called queue.
queue <- c('James','Mary', 'Steve', 'Alex', 'Patricia')

#b. Update the queue for the arrival of a new patron named Harold.
queue <- c(queue,'Harold')

#c. Update the queue to reflect the fact that James has finished checking out.
queue <- queue[-1]

#d. Update the queue to reflect the fact that Pam has talked her way in front of Steve with just one item.
queue <- append(queue, "Pam", after = 1)

#e. Update the queue to reflect the fact that Harold has grown impatient and left. 
queue <- queue [!queue %in% "Harold"]

#f. Update the queue to reflect the fact that Alex has grown impatient and left. (Do this as if you do not know what slot Alex currently occupies by number.) 
queue <- queue[-6]

#g. Identify the position of Patricia in the queue.
match("Patricia",queue)

#h. Count the number of people in the queue. 
length(queue)

#2. Modify your answer to quiz exercise 21 so that when you implement the quadratic equation, meaningful output is given whether there are one, two, or no solutions. (Hint: Use the discriminant.) 
#tests 2 solutions
a <- 1
b <- 3
c <- -4
#tests 1 solution
a <- 9
b <- 12
c <- 4
#tests 0 solutions
a <- 9
b <- 2
c <- 4

discriminant <- (b^2)-(4*a*c)
if(discriminant > 0)
{
  x1 <- (-b + sqrt(discriminant))/(2*a)
  x2 <- (-b - sqrt(discriminant))/(2*a)
  print("Two Solutions:")
  print(c(x1,x2))
} else if(discriminant == 0)
{
  x1 <- (-b + sqrt(discriminant))/(2*a)
  print("One Solution:")
  print(x1)
} else
{
  print("No Solutions")
}

#3. Use R to determine how many numbers from 1 to 1000 are not divisible by any of 3,7, and 11.
numbers <- c(1:1000)
numbers <- numbers[!numbers%%3==0&!numbers%%7==0&!numbers%%11==0]
length(numbers)

#4. Write R code that takes three input constants f, g, and h and determines whether they form a Pythagorean Triple (such that the square of the 
#test true
f <- 3
g <- 4
h <- 5
#test false
f <- 2
g <- 4
h <- 5

constants <- c(f,g,h)
largest <- max(constants)
leftover <- constants[!constants %in% largest]
leftover <- leftover * leftover
total <- sum(leftover)

if(largest^2 == total)
{
  print("True")
} else
  print("False")
