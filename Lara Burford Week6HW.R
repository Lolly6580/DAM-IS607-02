#instals devtools and rvest packages
library(devtools)
install_github("hadley/rvest")
library(rvest)

#test installation
lego_movie <- html("http://www.imdb.com/title/tt1490017/")
rating <- lego_movie %>% 
  html_nodes("strong span") %>%
  html_text() %>%
  as.numeric()
rating
cast <- lego_movie %>%
  html_nodes("#titleCast .itemprop span") %>%
  html_text()
cast
poster <- lego_movie %>%
  html_nodes("#img_primary img") %>%
  html_attr("src")
poster

#pull info from website with rvest
url <- html("http://www.reuters.com/finance")
content <- url %>% 
  html_nodes("div p") %>%
  html_text()
content
table <- html_table(url, fill = TRUE)
table

# list all available demos
demo(package="rvest")
# lists code for tripadvisor demo; follow instructions
# in your RStudio console window.
demo("tripadvisor", "rvest")

#pull info from website with xml and rcurl
library(XML)
library(RCurl)
url <- getURL("http://www.reuters.com/finance")
doc = htmlParse(url, asText=TRUE)
plain.text <- xpathSApply(doc, "//p", xmlValue)
cat(paste(plain.text, collapse = "\n"))
tables <- readHTMLTable(url)
tables

#differences between rvest & xml/rcurl
#1) xml/rcurl seems a little more complicated.  
#2) Maybe there's a way to output my text information in a more formatted way with xml/rcurl, but the rvest output is much more readable.
#1) XML puts everything from the tables into the data frame as factors, but rvest keep everything as vectors which I prefer b/c it is easier to read and less confusing.
