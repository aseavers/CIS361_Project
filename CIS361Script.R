#Make sure to change your working dirrectory
setwd("C:/Users/Owner/Desktop/CIS361Project_AllieSeavers");
getwd();

install.packages("readxl");
install.packages("dplyr")
install.packages("BBmisc");
install.packages("scatterplot3d");
install.packages("ggplot2");
install.packages("stargazer");
install.packages("jtools");

require("readxl");
require("dplyr");
require("BBmisc");
require("scatterplot3d");
require("ggplot2");
require("stargazer");
require("jtools");

# Here I am loading the excel files into the program
movieRevenueTable <-read_excel("AllMoviesDetailsCleaned.xlsx");
movieRatingsTable <-read_excel("movie_ratings_16_17.xlsx");

#This is where I will begin my cleaning of these dataframes
movieRatingsTable <-select(movieRatingsTable,movie,imdb,tmeter,audience);
movieRevenueTable <- select(movieRevenueTable,revenue,title);

#Combinging the datasets into one based on the movie titles from the year 2016-2017
merged <- merge(movieRatingsTable,movieRevenueTable,by.x = "movie",by.y = "title");

#Get rid of duplicate rows within the movie titles
noDuplicates <- merged[!duplicated(merged$movie), ];

#pull out rating rows to normalize them to a range of 0-5 for a better comparison
justRatings <- select(noDuplicates,imdb,tmeter,audience);
normalizedValues<- normalize(justRatings, method = "range", range = c(0, 5));
moviesInfo <- mutate(noDuplicates,normalizedValues$tmeter, normalizedValues$imdb,normalizedValues$audience);

#getting rid of non-normalized values after remerging table and formatting decimalsplaces to only one
moviesInfo <- select(moviesInfo,movie,imdbN =`normalizedValues$imdb`,rTMCN=`normalizedValues$tmeter`,rTMUN=`normalizedValues$audience`,revenue);
moviesInfo$rTMCN <- round(moviesInfo$rTMCN,digits=1);
moviesInfo$rTMUN<- round(moviesInfo$rTMUN,digits=1);

#Linear Regression
movieReg <- lm(moviesInfo$revenue ~ moviesInfo$imdbN+ moviesInfo$rTMCN+ moviesInfo$rTMUN,data = moviesInfo);
summ(movieReg);

#Plotting the Linear Regression
ggplot(movieReg, aes(x=moviesInfo$imdbN, y=moviesInfo$revenue)) + geom_point() +
  geom_smooth(method="lm") + labs(x="IMdB Normalized Ratings", y="Revenue")
ggplot(movieReg, aes(x=moviesInfo$rTMCN, y=moviesInfo$revenue)) + geom_point() +
  geom_smooth(method="lm") + labs(x="Rotten Tomatoes Critics Normalized Ratings", y="Revenue")
ggplot(movieReg, aes(x=moviesInfo$rTMUN, y=moviesInfo$revenue)) + geom_point() +
  geom_smooth(method="lm") + labs(x="Rotten Tomatoes Users Normalized Ratings", y="Revenue") 

#Simple Plotting
#Reducing Revenue to make graphs cleaner
moviesInfo$revenue <- moviesInfo$revenue/10000000;
plot(moviesInfo$imdbN,moviesInfo$revenue, main="IMdB User Ratings", xlab="Ratings", ylab="Revenue");
plot(moviesInfo$rTMCN,moviesInfo$revenue, main="Rotten Tomato Critic Ratings", xlab="Ratings", ylab="Revenue");
plot(moviesInfo$rTMUN,moviesInfo$revenue, main="Rotten Tomato User Ratings", xlab="Ratings", ylab="Revenue");


#Dashboard ideas:
#DO A COUNT FOR EACH TO SEE WHICH GROUP HAD MORE IN A CERTAIN NUMBER
#Top 5 for revenue
#Interactive that search and display movies
#Pie chart with 3 groups

#CREATE GRAPGH WHERE MOVIE TITLE IS X AND RATINGS IS Y

#Notes:
#imdb = imdb user ratings(normal people)
#tmeter = rotten tomatoes critic ratings
#audience = rotten tomatoes user ratings(users who have bought tickets to see the film)
#https://github.com/mircealex/Movie_ratigs_2016_17