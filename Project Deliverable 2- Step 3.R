RNGversion(vstr = 3.6)

rm(list=ls())

library(ISLR); library(ggplot2); library(caret); library(caTools); library(tidyr); library(dplyr); library(lm.beta);
library(glmnet); library(car); library(cluster); library(mclust);
library(leaps); library(car); library(mice);
library(data.table); library(ngram); library(stringr); library(arules);
library(recommenderlab);
library(dplyr);
library(reshape2);
library(Matrix);
library(recommenderlab);

install.packages("reshape2")
install.packages("Matrix")

install.packages("ISLR")
library(ISLR);
install.packages("ggplot2")
library(ggplot2); 

install.packages("caret")
library(caret); 


install.packages("caTools")
library(caTools); 


install.packages("tidyr")
library(tidyr); 


install.packages("dplyr")
library(dplyr); 

install.packages("lm.beta")
library(lm.beta);

install.packages("glmnet")
library(glmnet); 


install.packages("car")
library(car); 


install.packages("cluster")
library(cluster);


install.packages("mclust")
library(mclust);


install.packages("leaps")
library(leaps);


install.packages("car")
library(car); 


install.packages("mice")
library(mice);



install.packages("data.table")
library(data.table);


install.packages("ngram")
library(ngram); 


install.packages("stringr")
library(stringr);


install.packages("arules")
library(arules);


install.packages("recommenderlab")
library(recommenderlab);


install.packages("ISLR")
install.packages("leaps")
install.packages("lm.beta")
install.packages("tidyr")
install.packages("futile.matrix")
install.packages("glmnet")
install.packages("cluster")
install.packages("mclust")


data = read.csv("C:\\yelp_ratings.csv");

str(data)
summary(data)
ratings_matrix = as(data, Class = 'realRatingMatrix')


set.seed(100)
split = sample(nrow(ratings_matrix),size = 0.9*nrow(ratings_matrix))
train = ratings_matrix[split,]
test = ratings_matrix[-split,]


ratings %>% 
  ggplot(aes(x = ratings, fill = factor(ratings))) +
  geom_bar(color = "grey20") + scale_fill_brewer(palette = "YlGnBu") + guides(fill = FALSE)

#######################################################################################

# Question 1

recommenderRegistry$get_entries(data='realRatingMatrix')$UBCF_realRatingMatrix
recommenderRegistry$get_entries("UBCF", type = 'realRatingMatrix') # Shows all the diff functions/options in that command


#recommendation for one user
model  <- Recommender(ratings_matrix,method = 'UBCF', param = list(method = 'pearson',nn = 10))

pred  <- predict(model,ratings_matrix[1,],type = 'ratings')
as(pred,'data.frame') %>% arrange(rating %>% desc)
getList(pred)

#Exploring similar users


sim  <- similarity(ratings_matrix, method = 'pearson') %>% as.matrix() %>% .[,1] %>% as.data.frame()

sim$user <- rownames(sim)
colnames(sim)[1]  <- 'similarity'
sim <- sim %>% arrange(similarity %>% desc())
top10sim <- sim[1:10,]
top10sim

-----------------------
  
  set.seed(1031)
split = sample(nrow(ratings_matrix),size = 0.6*nrow(ratings_matrix))
train = ratings_matrix[split,]
test = ratings_matrix[-split,]

recommenderRegistry$get_entries(data='realRatingMatrix')$UBCF_realRatingMatrix
recommenderRegistry$get_entries("UBCF", type = 'realRatingMatrix') # Shows all the diff functions/options in that command

recom = Recommender(ratings_matrix, method = 'UBCF',param = list(method = 'cosine',nn = 10)) #parameter = list(method = 'cosine', nn = 25, sample = F, normalize='center')
recom

pred = predict(recom, ratings_matrix[8,],type = 'ratings')
as(pred,'data.frame') %>% arrange(rating %>% desc)

############################################################################################################
#Neural Network to determine meaningful review score based on ratings and quality review category

## Creating  variable 
library(neuralnet)
# Read the Data
data <- read.csv("C://yelp.csv", header = TRUE)
str(data)



# Data Partition
set.seed(2)
ind <- sample(2, nrow(data), replace = TRUE, prob = c(0.6, 0.3))
training <- data[ind==1,]
testing <- data[ind==2,]


# Min-Max Normalization
data$cool <- (data$cool - min(data$cool))/(max(data$cool) - min(data$cool))
data$useful <- (data$useful - min(data$useful))/(max(data$useful) - min(data$useful))
data$funny <- (data$funny - min(data$funny))/(max(data$funny)-min(data$funny))
data$stars <- (data$stars - min(data$stars))/(max(data$stars)-min(data$stars))

# Neural Networks
library(neuralnet)
set.seed(333)

n <- neuralnet(useful~stars+cool+funny,
               data = training,
               hidden =3,
               err.fct = "sse",
               linear.output = FALSE)
plot(n)
testing_useful= testing$useful
predict_testNN = compute(n,testing)
predict_testNN = (predict_testNN$net.result * (max(data$useful) - min(data$useful))) + min(data$useful)

plot(testing$useful, predict_testNN, col='blue', pch=16, ylab = "predicted rating NN", xlab = "real rating")

abline(0,1)

####################################################################################################################
#Neural Network code to effect of cool, useful and funny review in future ratings
#predicting ratings

## Creating index variable 
library(neuralnet)
# Read the Data
data <- read.csv("C://yelp.csv", header = TRUE)
str(data)



# Data Partition
set.seed(2)
ind <- sample(2, nrow(data), replace = TRUE, prob = c(0.6, 0.3))
training <- data[ind==1,]
testing <- data[ind==2,]


# Min-Max Normalization
data$cool <- (data$cool - min(data$cool))/(max(data$cool) - min(data$cool))
data$useful <- (data$useful - min(data$useful))/(max(data$useful) - min(data$useful))
data$funny <- (data$funny - min(data$funny))/(max(data$funny)-min(data$funny))
data$stars <- (data$stars - min(data$stars))/(max(data$stars)-min(data$stars))

# Neural Networks
library(neuralnet)
set.seed(333)

n <- neuralnet(useful~stars+cool+funny,
               data = training,
               hidden =5,
               err.fct = "sse",
               linear.output = FALSE)
plot(n)
testing_useful= testing$useful
predict_testNN = compute(n,testing)
predict_testNN = (predict_testNN$net.result * (max(data$useful) - min(data$useful))) + min(data$useful)

plot(testing$useful, predict_testNN, col='blue', pch=16, ylab = "predicted rating NN", xlab = "real rating")

abline(0,1)
#############################################################################################################



