# -----------------------------------------------------------------------------------------------
# Install packages
install.packages("ggplot2")
install.packages("MASS")
install.packages("corrplot")
install.packages("stargazer")
install.packages("VIM")
install.packages("mice")
install.packages("dplyr")
install.packages("plotly")
install.packages("data.table")
install.packages("formattable")
install.packages("RColorBrewer")
install.packages("randomForest")
install.packages("rpart")
install.packages("rpart.plot")
install.packages("GGally")
install.packages("tree")
install.packages("spcov")
install.packages("ade4")
install.packages("gvlma")
install.packages("glmnet")
install.packages("xtable")
install.packages("dprep")
install.packages("nnet")
install.packages("caret")
install.packages("gridExtra")

# Include packages
library(ggplot2)
library(MASS)
library(corrplot)
library(stargazer)
library(VIM)
library(mice)
library(dplyr)
library(plotly)
library(data.table)
library(formattable)
library(RColorBrewer)
library(randomForest)
library(rpart)
library(rpart.plot)
library(GGally)
library(tree)
library(spcov)
library(ade4)
library(gvlma)
library(glmnet)
library(xtable)
library(dprep)
library(nnet)
library(caret)
library(gridExtra)

# -----------------------------------------------------------------------------------------------
# Working directory
setwd("C:/Users/Clara/Documents/UC3M/M??ster/1 Term - 2/Statistical learning")
setwd("/Users/Emilie/Dropbox/Skole/UC3M/Statistical Learning/Project")

# -----------------------------------------------------------------------------------------------
# Load data set
movies = read.csv("movie_metadata.csv")

# -----------------------------------------------------------------------------------------------
# Investigate the data set

# Check the type of object
is(movies);
# Check dimension
dim(movies);
# View variable names 
str(movies);
# Have a look a it
head(movies);

# -----------------------------------------------------------------------------------------------
# Initial cleaning process
# -----------------------------------------------------------------------------------------------
# 1. Remove duplicated movies
movies.distinct = movies[!duplicated(movies[c("movie_title","director_name")]),]

# 2. Filter movies from the US
movies.usa = movies.distinct[movies.distinct$country =="USA", ]
dim(movies.usa)

# 3. Variables not considered
# Categorical variables not significant
movies.usa$color <- NULL
movies.usa$director_name <- NULL
movies.usa$actor_2_name <- NULL
movies.usa$actor_1_name <- NULL
movies.usa$actor_3_name <- NULL
movies.usa$language <- NULL
movies.usa$country <- NULL
# Text variables not used
movies.usa$plot_keywords <- NULL
movies.usa$movie_imdb_link <- NULL

names(movies.usa)


# -----------------------------------------------------------------------------------------------
# Second cleaning process
# -----------------------------------------------------------------------------------------------

movies.new <- movies.usa

# 1. Remove outliers
movies.new = movies.new[!(movies.new$duration > 330),]

# 2. Correct outliers
# Correct the movies with long duration

# print movie title of movies and duration for movies longer than 200 min
movies.long = subset(movies.new, duration > 200, select = c(movie_title, duration));movies.long

# Replace duration value for incorrect duration
movies.new <- within(movies.new, duration[movie_title=='Watchmen\302\240'] <- 162)
movies.new <- within(movies.new, duration[movie_title=='The Wolf of Wall Street\302\240'] <- 180)
movies.new <- within(movies.new, duration[movie_title=='Gangs of New York\302\240'] <- 167)
movies.new <- within(movies.new, duration[movie_title=='Wyatt Earp\302\240'] <- 191)
movies.new <- within(movies.new, duration[movie_title=='Gods and Generals\302\240'] <- 219)
movies.new <- within(movies.new, duration[movie_title=='The Thin Red Line\302\240'] <- 170)
movies.new <- within(movies.new, duration[movie_title=='All the Pretty Horses\302\240'] <- 116)
movies.new <- within(movies.new, duration[movie_title=='Nixon\302\240'] <- 192)
movies.new <- within(movies.new, duration[movie_title=='Blood In, Blood Out\302\240'] <- 180)
movies.new <- within(movies.new, duration[movie_title=='Apocalypse Now\302\240'] <- 153)
movies.new <- within(movies.new, duration[movie_title=='Dances with Wolves\302\240'] <- 181)
movies.new <- within(movies.new, duration[movie_title=='The Godfather: Part II\302\240'] <- 202)
movies.new <- within(movies.new, duration[movie_title=='Gone with the Wind\302\240'] <- 238)
movies.new <- within(movies.new, duration[movie_title=='Woodstock\302\240'] <- 184)

# 3. Remove text variables
movies.new$movie_title <- NULL 

# 4. Analyse missing data
aggr_plot <- aggr(movies.new, col=c('deepskyblue4','cyan'), numbers=TRUE, sortVars=TRUE, labels=names(movies.new), 
                  cex.axis=.7, gap=2, ylab=c("Histogram of missing data","Pattern"))

# Table of missing data
md.pattern(movies.new)

# Investigating missing values in the data set
options(digits=4)
pMiss <- function(x){sum(is.na(x))/length(x)*100}
missing.columns <- apply(movies.new,2,pMiss);missing.columns
missing.rows <- apply(movies.new,1,pMiss);missing.rows

# Highest percentage of missing values
max.column <- max(missing.columns);max.column
max.row <- max(missing.rows);max.row

# Percentage of missing values
length(is.na(movies.new$budget))/length(movies.new)
length(is.na(movies.new$gross))/length(movies.new)

# 5. Correct for missing data

# Remove aspect ratio
movies.new$aspect_ratio <- NULL

# Remove missing values in gross and budget
movies.no.na <- subset(movies.new,!(is.na(movies.new["gross"]) & is.na(movies.new["budget"])))

# Impute missing values in gross and budget
movies.no.na$gross[is.na(movies.no.na$gross)] <- round(mean(movies.no.na$gross, na.rm = TRUE))
movies.no.na$budget[is.na(movies.no.na$budget)] <- round(mean(movies.no.na$budget, na.rm = TRUE))

# Remove all remaining rows with missing data
movies.no.na <- na.omit(movies.no.na)

# 6. Adjust for inflation in gross and budget
i = 0.0318
age = 2016 - movies.no.na$title_year

movies.no.na$budget <- movies.no.na$budget*(1 + i)^age
movies.no.na$gross <- movies.no.na$gross*(1 + i)^age

# 7. Remove dependent variables with high correlation
movies.no.na$num_user_for_reviews <- NULL
movies.no.na$movie_facebook_likes <- NULL
movies.no.na$cast_total_facebook_likes <- NULL 
movies.no.na$content_rating <- NULL

names(movies.no.na)

# 8. Remove categorical variables
movies.no.na.quant <- movies.no.na
movies.no.na.quant$genres <- NULL

names(movies.no.na.quant)

# -----------------------------------------------------------------------------------------------
# New cleaning process for prediction

pred.movies = movies.no.na;

# Make binary variables for each genre
genres.action <- ifelse(grepl("Action", pred.movies$genres, ignore.case = T), 1, 0);
genres.adventure <- ifelse(grepl("Adventure", pred.movies$genres, ignore.case = T), 1, 0);
genres.animation <- ifelse(grepl("Animation", pred.movies$genres, ignore.case = T), 1, 0);
genres.crime <- ifelse(grepl("Crime", pred.movies$genres, ignore.case = T), 1, 0);
genres.comedy <- ifelse(grepl("Comedy", pred.movies$genres, ignore.case = T), 1, 0);
genres.drama <- ifelse(grepl("Drama", pred.movies$genres, ignore.case = T), 1, 0);
genres.fantacy <- ifelse(grepl("Fantasy", pred.movies$genres, ignore.case = T), 1, 0);
genres.family <- ifelse(grepl("Family", pred.movies$genres, ignore.case = T), 1, 0);
genres.history <- ifelse(grepl("History", pred.movies$genres, ignore.case = T), 1, 0);
genres.horror <- ifelse(grepl("Horror", pred.movies$genres, ignore.case = T), 1, 0);
genres.musical <- ifelse(grepl("Musical", pred.movies$genres, ignore.case = T), 1, 0);
genres.mystery <- ifelse(grepl("Mystery", pred.movies$genres, ignore.case = T), 1, 0);
genres.sci.fi <- ifelse(grepl("Sci-Fi", pred.movies$genres, ignore.case = T), 1, 0);
genres.sport <- ifelse(grepl("Sport", pred.movies$genres, ignore.case = T), 1, 0);
genres.war <- ifelse(grepl("War", pred.movies$genres, ignore.case = T), 1, 0);
genres.western <- ifelse(grepl("Western", pred.movies$genres, ignore.case = T), 1, 0);
genres.thriller <- ifelse(grepl("Thriller", pred.movies$genres, ignore.case = T), 1, 0);
genres.documentary <- ifelse(grepl("Documentary", pred.movies$genres, ignore.case = T), 1, 0);

# Combine all categories into a single data frame
genres.dummy <- cbind(genres.action,genres.adventure,genres.crime,genres.comedy,genres.drama,genres.fantacy,
                      genres.family,genres.horror,genres.mystery,genres.sci.fi,genres.thriller)
genres.dummy <- data.frame(genres.dummy)

# Name of categories
names(genres.dummy)

# Delete genres category
pred.movies$genres <- NULL

# Combine content rating dummies and genre dummies with the rest of the data set
pred.movies <- cbind(genres.dummy,pred.movies)
# -----------------------------------------------------------------------------------------------
