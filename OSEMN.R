#Obtain data
tablenum <- 3;
url <- rvest::html("http://www.rottentomatoes.com/top/bestofrt/?category=9")
drama.m <- rvest::html_table(rvest::html_nodes(url,"table")[[tablenum]])
drama.m <- as.data.frame(drama.m)
str(drama.m)
top_drama <- write.csv(drama.m, file="obtainData.csv")

#Scrub data- read
readFile <- read.csv("obtainData.csv") 

#Scrub data- select column
readFile.rank <- readFile[,"Rank"] #integer
rank.Num <- as.numeric(readFile.rank) #numeric
class(rank.Num)

readFile.ratings <- readFile[,"RatingTomatometer"] #factor
ratings.gsub <- gsub(pattern="%", replacement="", x=readFile.ratings) #character
ratings.Num <- as.numeric(ratings.gsub)
class(ratings.Num)

readFile.title <- readFile[,"Title"] #factor
title.Char <- as.character(readFile.title) #character
class(title.Char)

readFile.reviews <- readFile[,"No..of.Reviews"] #integer
reviews.Num <- as.numeric(readFile.reviews) #numeric
class(reviews.Num)

movies.df <- data.frame(reviews.Num, title.Char, ratings.Num, rank.Num)
names(movies.df)
names(movies.df) <- c("Reviews", "Title", "Ratings", "Rank") #rename column

movies.df$Title <- as.character(movies.df$Title) #reformat Title column


#Explore
class(movies.df$Reviews)
class(movies.df$Title)
class(movies.df$Ratings)
class(movies.df$Rank)
class(movies.df)

str(movies.df$Reviews)
str(movies.df$Title)
str(movies.df$Ratings)
str(movies.df$Rank)
str(movies.df)

summary(movies.df$Reviews)
summary(movies.df$Title)
summary(movies.df$Ratings)
summary(movies.df$Rank)
summary(movies.df)

##More Exploring
names(movies.df)

#########SAMPLE FOR ORDERING##################
test1 <- c(1, 2, 4, 6, 2)
test2 <- c("Obi", "dd", "vv", "ds", "dsx")
dataframe.obj <- data.frame(test1, test2)
names(dataframe.obj)

orderTest <- dataframe.obj[order(dataframe.obj$test1),]
################################################

#More Exploring- order reviews column
movies.dfNew <- movies.df[order(movies.df$Reviews),] #dataframe ordered version
class(movies.dfNew)

#More Exploring- select and create two dataframes for top ten reviews and bottom ten reviews
movies.dfNew_LOW <- movies.dfNew[1:10,]
movies.dfNew_HIGHER <- movies.dfNew[91:100,]
names(movies.dfNew_LOW)
names(movies.dfNew_HIGHER)
class(movies.dfNew_LOW$Reviews)
class(movies.dfNew_LOW$Title)
class(movies.dfNew_LOW$Ratings)
class(movies.dfNew_LOW$Rank)
class(movies.dfNew_LOW)

class(movies.dfNew_HIGHER$Reviews)
class(movies.dfNew_HIGHER$Title)
class(movies.dfNew_HIGHER$Ratings)
class(movies.dfNew_HIGHER$Rank)
class(movies.dfNew_HIGHER)

str(movies.dfNew_LOW)

str(movies.dfNew_HIGHER)

summary(movies.dfNew_LOW)
summary(movies.dfNew_HIGHER)

#Results
#Results- Total Rating
lowRating.Sum <- sum(movies.dfNew_LOW$Ratings)
highRating.Sum <- sum(movies.dfNew_HIGHER$Ratings)

#Results- table creation
vectLow <- c("Low Reviews", lowRating.Sum)
vectHigh <- c("High Reviews", highRating.Sum)
dataframeObject <- rbind(vectLow, vectHigh)
dataframeObject <- as.data.frame(dataframeObject)
class(dataframeObject)

#Results- Column and Row rename
row.names(dataframeObject) <- c(1,2)
names(dataframeObject) <- c("Type", "Total.Ratings")
dataframeObject$Type <- as.character(dataframeObject$Type)
dataframeObject$Total.Ratings <- as.numeric(as.character(dataframeObject$Total.Ratings))
type1 <- dataframeObject[1,1]
type2 <- dataframeObject[2,1]
total1 <- dataframeObject[1,2]
total2 <- dataframeObject[2,2]
#Remember to display the above df in tables in Latex################
library(ggplot2)
typeFact <- factor(c(type1,type2), levels=c("Low Reviews","High Reviews"))
dat <- data.frame(
  type = typeFact,
  total_bill = c(total1, total2)
)
ggplot(data=dat, aes(x=type, y=total_bill, fill=type)) +
  geom_bar(stat="identity")

ggplot(data=dat, aes(x=type, y=total_bill, group=1)) + 
  geom_line(colour="red", linetype="dashed", size=1.5) + 
  geom_point(colour="red", size=4, shape=21, fill="white") 
