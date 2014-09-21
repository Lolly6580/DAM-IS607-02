

require(ggplot2)
data(movies)
head(movies)

#1. Show an appropriate visualization that displays the total number of movies for each decade.
count <- c()
decade1 <- movies$year[movies$year > 1890 & movies$year <= 1900]
count <- c(count, length(decade1))
decade2 <- movies$year[movies$year > 1900 & movies$year <= 1910]
count <- c(count, length(decade2))
decade3 <- movies$year[movies$year > 1910 & movies$year <= 1920]
count <- c(count, length(decade3))
decade4 <- movies$year[movies$year > 1920 & movies$year <= 1930]
count <- c(count, length(decade4))
decade5 <- movies$year[movies$year > 1930 & movies$year <= 1940]
count <- c(count, length(decade5))
decade6 <- movies$year[movies$year > 1940 & movies$year <= 1950]
count <- c(count, length(decade6))
decade7 <- movies$year[movies$year > 1950 & movies$year <= 1960]
count <- c(count, length(decade7))
decade8 <- movies$year[movies$year > 1960 & movies$year <= 1970]
count <- c(count, length(decade8))
decade9 <- movies$year[movies$year > 1970 & movies$year <= 1980]
count <- c(count, length(decade9))
decade10 <- movies$year[movies$year > 1980 & movies$year <= 1990]
count <- c(count, length(decade10))
decade11 <- movies$year[movies$year > 1990 & movies$year <= 2000]
count <- c(count, length(decade11))
decade12 <- movies$year[movies$year > 2000 & movies$year <= 2010]
count <- c(count, length(decade12))

decades <- c("1890-1900"= length(decade1), "1901-1910"= length(decade2),"1911-1920"= length(decade3),"1921-1930"= length(decade4),"1931-1940"= length(decade5),"1941-1950"= length(decade6),"1951-1960"= length(decade7),"1961-1970"= length(decade8),"1971-1980"= length(decade9),"1981-1990"= length(decade10),"1991-2000"= length(decade11),"2001-2010"= length(decade12))
barplot(decades, main = "Total Movies Per Decade", xlab = "Decade", ylab = "Total Movies") 

#2. Show the average IMDB user rating for different genres of movies? Has this changed over time?
#The rating by genre doesn't seem to have changed over time.
genre <- c("Action", "Animation", "Comedy", "Drama", "Documentary", "Romance", "Short")
rating <- c(0,0,0,0,0,0,0)
averages <- data.frame(Genre = genre, Average_Rating = rating)

average <- aggregate(rating ~ Action, movies, mean)
averages[1,2] <- average[2,2]
average <- aggregate(rating ~ Animation, movies, mean)
averages[2,2] <- average[2,2]
average <- aggregate(rating ~ Comedy, movies, mean)
averages[3,2] <- average[2,2]
average <- aggregate(rating ~ Documentary, movies, mean)
averages[5,2] <- average[2,2]
average <- aggregate(rating ~ Drama, movies, mean)
averages[4,2] <- average[2,2]
average <- aggregate(rating ~ Romance, movies, mean)
averages[6,2] <- average[2,2]
average <- aggregate(rating ~ Short, movies, mean)
averages[7,2] <- average[2,2]
averages

averageaction <- aggregate(rating ~ Action + year, movies, mean)
averageaction <- subset(averageaction, Action==1)
averageanimation <- aggregate(rating ~ Animation + year, movies, mean)
averageanimation <- subset(averageanimation, Animation==1)
averagecomedy <- aggregate(rating ~ Comedy + year, movies, mean)
averagecomedy <- subset(averagecomedy, Comedy==1)
averagedocumentary <- aggregate(rating ~ Documentary + year, movies, mean)
averagedocumentary <- subset(averagedocumentary, Documentary==1)
averagedrama <- aggregate(rating ~ Drama + year, movies, mean)
averagedrama <- subset(averagedrama, Drama==1)
averageromance <- aggregate(rating ~ Romance + year, movies, mean)
averageromance <- subset(averageromance, Romance==1)
averageshort <- aggregate(rating ~ Short + year, movies, mean)
averageshort <- subset(average, Short==1)


#3. Is there a relationship between length of movie and movie rating?
#Just based on a simple scatter plot, there doesn't appear to be a relationship between the length and the rating.
plot(length ~ rating, data = movies)

#4. Is there a relationship between length of movie and genre?
#There does seem to be a relationship between length and genre.  It makes sense that shorts and animation films tend to be shorter than other genres on average.
length <- c(0,0,0,0,0,0,0)
averagelength <- data.frame(Genre = genre, Average_Length = rating)

average <- aggregate(length ~ Action, movies, mean)
averagelength[1,2] <- average[2,2]
average <- aggregate(length ~ Animation, movies, mean)
averagelength[2,2] <- average[2,2]
average <- aggregate(length ~ Comedy, movies, mean)
averagelength[3,2] <- average[2,2]
average <- aggregate(length ~ Documentary, movies, mean)
averagelength[5,2] <- average[2,2]
average <- aggregate(length ~ Drama, movies, mean)
averagelength[4,2] <- average[2,2]
average <- aggregate(length ~ Romance, movies, mean)
averagelength[6,2] <- average[2,2]
average <- aggregate(length ~ Short, movies, mean)
averagelength[7,2] <- average[2,2]
averagelength

#5. Which other variable best predicts total number of votes that a movie received.
#I think rating is the best predictor of the number of votes.
plot(rating ~ votes, data = movies)

