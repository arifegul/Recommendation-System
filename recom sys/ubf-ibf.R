library("rvest")
library("XML")
library("xml2")
library(dplyr)
library(ggplot2)
library(knitr)
library(recommenderlab)

#fetch data from imdb
url = "http://www.imdb.com/chart/top?ref_=nv_wl_img_3"

page = read_html(url)

movie.nodes <- html_nodes(page,'.titleColumn a')

movie.link = sapply(html_attrs(movie.nodes),`[[`,'href')
movie.link = paste0("http://www.imdb.com",movie.link)
movie.cast = sapply(html_attrs(movie.nodes),`[[`,'title')
movie.name = html_text(movie.nodes)

sec <- html_nodes(page,'.secondaryInfo')

# Clean the Data 
year = as.numeric(gsub(")","",# Removing) 
                  gsub("\\(","",# Removing (
                       html_text(sec)# get text of HTML node  
                       )))

rating.nodes = html_nodes(page,'.imdbRating')
# Check One node
xmlTreeParse(rating.nodes[[20]])


rating.nodes = html_nodes(page,'.imdbRating strong')
votes = as.numeric(gsub(',','',
                        gsub(' user ratings','',
                             gsub('.*?based on ','',
                                  sapply(html_attrs(rating.nodes),`[[`,'title')
                             ))))

rating = as.numeric(html_text(rating.nodes))

top250 <- data.frame(movie.name, movie.cast, movie.link,year,votes,rating)


#Recommender with Collaborative Filtering

#conflict_prefer("as.matrix", "proxy")
movie_matrix <- as.matrix(top250)
real_matrix <- as(movie_matrix, "realRatingMatrix")
as(real_matrix, "matrix")


#split data into train test set
set.seed(2020)#seed as year
n_folds <- 10  ## 10 iterations of run
to_keep <- 5  ## given 5 items
threshold <- 3 ## ratings above 3 as the cutoff point

evaluation <- evaluationScheme(real_matrix, method="cross-validation",k = n_folds, train=0.8, given=to_keep,  goodRating=threshold)

print(evaluation)



#USER BASED FILTERING
#I calculated Z score normalization
#I calculated cosine measure similarity


User_Based <- Recommender(getData(evaluation, "train"), "UBCF",
                          param=list(normalize = "Z-score", method="Cosine"))

#conflict_prefer("getData", "recommenderlab")

#predict ratings
prediction <- predict(User_Based, getData(evaluation, "known"), type="ratings")
print(calcPredictionAccuracy(x=prediction, data= getData(evaluation, "unknown"), byUser = FALSE))

boxplot(as.vector(as(prediction, "matrix")), col = "green", main = "UBCF Z-Score/Cosine Model", ylab = "Ratings")
hist(as.vector(as(prediction, "matrix")), main = "UBCF Z-Score/Cosine Model", col = "blue", xlab = "Predicted Ratings")

summary(as.vector(prediction@data@x))

#ITEM BASED FILTERING

#Calculated the similarity

Item_Based <- Recommender(getData(evaluation, "train"), method = "IBCF",parameter = list(method = "Cosine"))

prediction_item <- predict(object = Item_Based, newdata=getData(evaluation, "known"), n=10, type="ratings")

print(calcPredictionAccuracy(x=prediction_item, data= getData(evaluation, "unknown"), byUser = FALSE))

boxplot(as.vector(as(prediction_item, "matrix")), col = "red", main = "IBCF Z-Score/Cosine Model", ylab = "Ratings")
hist(as.vector(as(prediction_item, "matrix")), main = "IBCF Z-Score/Cosine Model", col = "gray", xlab = "Predicted Ratings")

summary(as.vector(prediction_item@data@x))

