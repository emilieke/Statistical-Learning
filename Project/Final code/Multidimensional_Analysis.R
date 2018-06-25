# -----------------------------------------------------------------------------------------------
# Working directory
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
# Exploratory Data Analysis (EDA)
# -----------------------------------------------------------------------------------------------

# Descriptive statistics for quantitative variables
summary(movies);

# Print latex table of descriptive statistics for quantitative variables
stargazer(movies);

# -----------------------------------------------------------------------------------------------
# Descriptive statistics for categorical variables

# Actors
a1 = movies %>% select(actor_1_name) %>% group_by(actor_1_name) %>% summarize(appearance_count=n())
a2 = left_join(movies, a1, by="actor_1_name")
a3 = a2 %>% select(actor_1_name, appearance_count) %>% distinct %>% arrange(desc(appearance_count))
summary(a3$appearance_count)

# Directors
d1 = movies %>% select(director_name) %>% group_by(director_name) %>% summarize(appearance_count=n())
summary(d1$appearance_count)

# Country
c1 = movies %>% select(country) %>% group_by(country) %>% summarize(appearance_count=n())
summary(c1$appearance_count)

# Language
l1 = movies %>% select(language) %>% group_by(language) %>% summarize(appearance_count=n())
summary(l1$appearance_count)

# Genres
g1 = movies %>% select(genres) %>% group_by(genres) %>% summarize(appearance_count=n())
summary(g1$appearance_count)

# Content rating
cr1 = movies %>% select(content_rating) %>% group_by(content_rating) %>% summarize(appearance_count=n())
summary(cr1$appearance_count)

# Color
cl1 = movies %>% select(color) %>% group_by(color) %>% summarize(appearance_count=n())
summary(cl1$appearance_count)

# -----------------------------------------------------------------------------------------------
# Univariate Plots
# -----------------------------------------------------------------------------------------------

abs.freq.color <- table(movies$color)
n.movies <- nrow(movies)
fj.color <- abs.freq.color/n.movies

dev.off()
# Barplot (using relative freq.)
barplot(fj.color,names.arg = c("Missing","B & W","Color"), freq=FALSE,
        col=c("cyan","lightblue","deepskyblue4"), ylab="Density", 
        main = "Barplot for color")

# -----------------------------------------------------------------------------------------------
# HISTOGRAMS

# Histogram showing distribution of IMDB rating
hist(movies.usa.q$imdb_score, main = 'Histogram of IMDB score', xlab = 'IMDB score', breaks=50, 
     col="deepskyblue4", freq=FALSE, xlim=c(0,10))

# Histograms for explanatory variables
dev.off()
par(mfrow=c(3,3))

hist(movies.usa.q$duration, xlab = '', breaks=20, main = 'duration', col="deepskyblue4", 
     xlim=c(0,330), freq=FALSE)

hist(movies.usa.q$title_year, xlab = '', breaks=30, main = 'title_year', col="deepskyblue4", 
     xlim=c(1910,2016), freq=FALSE)

hist(movies.usa.q$aspect_ratio, xlab = '', breaks=40, main = 'aspect_ratio', col="deepskyblue4", 
     xlim=c(0,16), freq=FALSE)

hist(movies.usa.q$budget, xlab = '', breaks=30, main = 'budget', col="deepskyblue4", 
     xlim=c(0,250000000), freq=FALSE)

hist(movies.usa.q$gross, xlab = '', breaks=25, main = 'gross', col="deepskyblue4", 
     freq=FALSE)

hist(movies.usa.q$facenumber_in_poster, xlab = '', breaks=30, main = 'facenumber_in_poster', 
     col="deepskyblue4", freq=FALSE)

hist(movies.usa.q$num_critic_for_reviews, xlab = '', breaks=20, main = 'num_critic_for_reviews', 
     col="deepskyblue4", freq=FALSE)

hist(movies.usa.q$num_user_for_reviews, xlab = '', breaks=30, main = 'num_user_for_reviews', 
     xlim=c(0,4000), col="deepskyblue4", freq=FALSE)

hist(movies.usa.q$num_voted_users, xlab = '', breaks=20, main = 'num_voted_users', 
     xlim=c(0,1700000), col="deepskyblue4", freq=FALSE)

dev.off()
par(mfrow=c(1,3))

hist(log(movies.usa.q$num_critic_for_reviews), xlab = '', breaks=20, main = 'num_critic_for_reviews', 
     col="deepskyblue4", freq=FALSE)

hist(log(movies.usa.q$num_user_for_reviews), xlab = '', breaks=30, main = 'num_user_for_reviews', 
     col="deepskyblue4", freq=FALSE)

hist(log(movies.usa.q$num_voted_users), xlab = '', breaks=15, main = 'num_voted_users', 
     col="deepskyblue4", freq=FALSE)

dev.off()
par(mfrow=c(2,3))

hist(movies.usa.q$movie_facebook_likes, xlab = '', breaks=30, main = 'movie_facebook_likes', 
     col="deepskyblue4", freq=FALSE)

hist(movies.usa.q$director_facebook_likes, xlab = '', breaks=20, main = 'director_facebook_likes', 
     col="deepskyblue4", freq=FALSE)

hist(movies.usa.q$actor_1_facebook_likes, xlab = '', breaks=30, main = 'actor_1_facebook_likes', 
     col="deepskyblue4", freq=FALSE)

hist(movies.usa.q$actor_2_facebook_likes, xlab = '', breaks=20, main = 'actor_2_facebook_likes', 
     col="deepskyblue4", freq=FALSE)

hist(movies.usa.q$actor_3_facebook_likes, xlab = '', breaks=20, main = 'actor_3_facebook_likes', 
     col="deepskyblue4", freq=FALSE)

hist(movies.usa.q$cast_total_facebook_likes, xlab = '', breaks=30, main = 'cast_total_facebook_likes', 
     col="deepskyblue4", freq=FALSE)


# -----------------------------------------------------------------------------------------------
# BARPLOTS 

# Barplot showing color occurences
abs.freq.color <- table(movies$color)
n.movies <- nrow(movies)
fj.color <- abs.freq.color/n.movies

dev.off()
# Barplot (using relative freq.)
barplot(fj.color,names.arg = c("Missing","B & W","Color"), freq=FALSE,
        col=c("cyan","lightblue","deepskyblue4"), ylab="Density", 
        main = "Barplot for color")


# -----------------------------------------------------------------------------------------------
# HISTOGRAMS

# Histogram showing distribution of IMDB rating
hist(movies$imdb_score, main = 'Histogram of IMDB score', xlab = 'IMDB score', breaks=60, col="deepskyblue4")
plot_ly(movies, x = movies$imdb_score, type='histogram', nbinsx = 80)

# Histogram showing distribution of movie year
hist(movies$title_year, main = 'Histogram of movie year', xlab = 'Title year', breaks=60, col="deepskyblue4")

# Histogram showing distribution of number of critics for reviews
hist(movies$num_critic_for_reviews, main = 'Histogram of number of critic for reviews', xlab = 'Number of critics', breaks=30, col="deepskyblue4")

# Histogram showing distribution of log number of critics for reviews
hist(log(movies$num_critic_for_reviews), main = 'Histogram of log number of critic for reviews', xlab = 'Log(critics)', breaks=30, col="deepskyblue4")

# Histogram showing distribution of number of users for reviews
hist(movies$num_user_for_reviews, main = 'Histogram of number of users for reviews', xlab = 'Number of users', breaks=60, col="deepskyblue4")

# Histogram showing distribution of log number of users for reviews
hist(log(movies$num_user_for_reviews), main = 'Histogram of log number of users for reviews', xlab = 'Log(users)', breaks=60, col="deepskyblue4")

# Histogram showing distribution of number of users voted
hist(movies$num_voted_users, main = 'Histogram of number of users voted', xlab = 'Number of users', breaks=60, col="deepskyblue4")

# Histogram showing distribution of log number of users voted
hist(log(movies$num_voted_users), main = 'Histogram of log number of users voted', xlab = 'Log(users)', breaks=60, col="deepskyblue4")

# Histogram showing distribution of duration
hist(movies$duration, main = 'Histogram of duration', xlab = 'Duration', breaks=40, col="deepskyblue4")

# Histogram showing distribution of appearance count for actors
a1 = movies %>% select(actor_1_name) %>% group_by(actor_1_name) %>% summarize(appearance_count=n())
a2 = left_join(movies, a1, by="actor_1_name")
a3 = a2 %>% select(actor_1_name, appearance_count) %>% distinct %>% arrange(desc(appearance_count))
hist(a3$appearance_count, main = 'Histogram of number of appearences for actors', xlab = 'Appearance count', breaks=60, col="deepskyblue4")

# -----------------------------------------------------------------------------------------------
# Bivariate Plots
# -----------------------------------------------------------------------------------------------
# SCATTER PLOTS

movies.num <-movies.no.na.quant

# Duration vs IMDB score
plot1 = ggplot(movies.num, aes(x=duration, y=imdb_score)) + geom_point(size=1.5, colour = "deepskyblue4") + theme(panel.background = element_rect(fill = 'white', colour = 'gray'))

#Year vs IMDB score
plot2 = ggplot(movies.num, aes(x=title_year, y=imdb_score)) + geom_point(size=1.5, colour = "deepskyblue4") + theme(panel.background = element_rect(fill = 'white', colour = 'gray'))
grid.arrange(plot1, plot2, nrow=1, ncol=2)

#Gross vs IMDB score
plot3 = ggplot(movies.num, aes(x=gross, y=imdb_score)) + geom_point(size=1.5, colour = "deepskyblue4") + theme(panel.background = element_rect(fill = 'white', colour = 'gray'))

#Budget vs IMDB score
plot4 = ggplot(movies.num, aes(x=budget, y=imdb_score)) + geom_point(size=1.5, colour = "deepskyblue4") + theme(panel.background = element_rect(fill = 'white', colour = 'gray'))
grid.arrange(plot3, plot4, nrow=1, ncol=2)

# -----------------------------------------------------------------------------------------------
# BOXPLOTS

# Boxplot of IMDB score for movie year
boxplot(imdb_score ~ title_year, data=movies, col="deepskyblue4", las=2)
title("Boxplot of IMDB score for movie year")

# Boxplot of IMDB score for movie year ordered by median
reordered_country = with(movies, reorder(country, -imdb_score, median))
boxplot(imdb_score ~ reordered_country, data=movies, lwd=0.5, col="deepskyblue4", las=2)
title("Boxplot of IMDB score for country ordered by median of IMDB score")

# Boxplot of IMDB score for language ordered by median
reordered_language = with(movies, reorder(language, -imdb_score, median))
boxplot(imdb_score ~ reordered_language, data=movies, lwd=0.5, col="deepskyblue4", las=2)
title("Boxplot of IMDB score for language ordered by median of IMDB score")

# Boxplot of IMDB score for content rating ordered by median
reordered_content_rating = with(movies, reorder(content_rating, -imdb_score, median))
boxplot(imdb_score ~ reordered_content_rating, data=movies, lwd=0.5, col="deepskyblue4", las=2)
title("Boxplot of IMDB score for content rating ordered by median")

# -----------------------------------------------------------------------------------------------
# BOXPLOTS USING PLOTLY

# Boxplot of IMDB score for color
plot_ly(movies, y = movies$imdb_score, type="box", colorRampPalette(brewer.pal(3, "Spectral"))(3))
scale_colour_brewer(palette = "Spectral")

# Boxplot of IMDB score for movie year
# TODO: not working
plot_ly(movies, y = movies$imdb_score, color = movies$title_year, type="box")

# Boxplot of IMDB score for movie year ordered by median
reordered_country = with(movies, reorder(country, -imdb_score, median))
plot_ly(movies, x = movies$imdb_score, color = reordered_country, type = "box", colors = brewer.pal(11, "Spectral"))

# Boxplot of IMDB score for language ordered by median
reordered_language = with(movies, reorder(language, -imdb_score, median))
plot_ly(movies, y = movies$imdb_score, color = reordered_language, type = "box")

# Boxplot of IMDB score and content rating ordered by median
reordered_content_rating = with(movies, reorder(content_rating, -imdb_score, median))
plot_ly(movies, x = movies$imdb_score, color = reordered_content_rating, type = "box")

# -----------------------------------------------------------------------------------------------

# Correlation matrix + correlation plot
R_movies.num = cor(movies.num)
corrplot(R_movies.num,order="FPC",tl.col = 'black', tl.srt=45, method="color")

# -----------------------------------------------------------------------------------------------
# Clustering
movies.num.01 = apply(movies.num, MARGIN = 2, FUN = function(X) (X - min(X))/diff(range(X)))
movies.kmeans.01 <- kmeans(movies.num.01, 2, iter.max = 10)
colors.kmeans.01 <- c("deepskyblue4","cyan")[movies.kmeans.01$cluster]
table(movies.kmeans.01$cluster)
head(movies.num.01)
parcoord(movies.num.01,col=alpha(colors.kmeans.01, 0.3),var.label=TRUE)
parcoord(movies.kmeans.01$centers,col=c("deepskyblue4","cyan"), var.label = TRUE)
movies.kmeans.01$centers
