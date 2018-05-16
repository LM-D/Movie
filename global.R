library(shiny)
library(corrplot)
library(tm)
library(wordcloud)
library(memoise)
library(psych)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(plotly)
library(googleVis)
library(reshape)
library(RColorBrewer)

movie = read.csv("movie.csv",header = T)
#movieo = read.csv("movie_o.csv",header = T)
movie_num = read.csv("movie_num.csv",header = T)
movie_yds<-dplyr::select(movie,movie_title,title_year,director_name,imdb_score,director_facebook_likes,gross,budget,genres,country)

direc_top<-dplyr::select(movie,"director_name","director_facebook_likes")
act1_top<-dplyr::select(movie,"actor_1_name","actor_1_facebook_likes")
prof_top<-dplyr::select(movie,"movie_title","imdb_score" ,"gross","title_year","genres","country","language","content_rating")
gt<-arrange(prof_top,desc(gross),desc(imdb_score),desc(title_year))
dt<-dplyr::distinct(arrange(direc_top,desc(director_facebook_likes)))
a1t<-dplyr::distinct(arrange(act1_top,desc(actor_1_facebook_likes)))


movie <- transform(movie, budget = ifelse(country == "South Korea", budget/1173.49, budget))
movie <- transform(movie, budget = ifelse(country == "Japan", budget/115.33, budget))
movie <- transform(movie, budget = ifelse(country == "Turkey", budget/3.49, budget))
movie <- transform(movie, budget = ifelse(country == "Hungary", budget/298.17, budget))
movie <- transform(movie, budget = ifelse(country == "Thailand", budget/35.67, budget))

movie <- transform(movie, gross = ifelse(country == "South Korea", gross/1173.49, gross))
movie <- transform(movie, gross = ifelse(country == "Japan", gross/115.33, gross))
movie <- transform(movie, gross = ifelse(country == "Turkey", gross/3.49, gross))
movie <- transform(movie, gross = ifelse(country == "Hungary", gross/298.17, gross))
movie <- transform(movie, gross = ifelse(country == "Thailand", gross/35.67, gross))


cor_names<-names(movie[-c(1,2,7,10,11,12,15,17,18,20,21,22,29,30)])
#g<-c("Action","Adventure","Animation","Biography","Comedy","Crime","Documentary","Drama","Family","Fantasy","Film","History","Horror","Music","Musical","Mystery","Noir","Romance","Sci","Sport","Thriller","War","Western")
#ag<-as.data.frame(for (i in g) { a[i]<-sum(gensplit==i)})
#genre_sum<-ag[-1]

genre <- Corpus(VectorSource(movie$genres_2))
genre_dtm <- DocumentTermMatrix(genre)
genre_freq <- colSums(as.matrix(genre_dtm))
freq <- sort(colSums(as.matrix(genre_dtm)), decreasing=TRUE)
genre_wf <- data.frame(word=names(genre_freq), freq=genre_freq)

word_assoc <- function(word)
{
  assoc_1 <- as.data.frame(findAssocs(genre_dtm,c(word),corlimit = 0.1))
  assoc_1$words <- rownames(assoc_1)
  colnames(assoc_1)[1] <- c("score")
  assoc_1$key <- c(word)
  rownames(assoc_1) <- NULL
  return(assoc_1)
}

drama_assoc <- word_assoc("drama")
comedy_assoc <- word_assoc("comedy")
thriller_assoc <- word_assoc("thriller")
action_assoc <- word_assoc("action")
romance_assoc <- word_assoc("romance")
adventure_assoc <- word_assoc("adventure")
crime_assoc <- word_assoc("crime")
assoc <- rbind(drama_assoc,comedy_assoc,thriller_assoc,action_assoc,
               romance_assoc,adventure_assoc,crime_assoc)

