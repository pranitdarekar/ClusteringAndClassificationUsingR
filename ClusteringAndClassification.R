# Project 2

# read the data set file and convert it to data frame
abalone <- read.table("C:/Masters/gwu/big data/abalone.data", sep = ",")
abalone

# checking the column names
colnames(abalone)

# checking number of rows and columns in the dataframe
dim(abalone)

# displaying all the columns and 10 rows from the dataset
abalone[1:10, ]

# assigning the column names
abalone.names = c("sex", "length", "diameter", "height", "whole_weight", "shucked_weight", "viscera_weight", "shell_weight", "rings")

# checking the column name assignment
abalone.names

# using str to view the structure of the abalone object
str(abalone)

# associating column names with the columns in the dataset
names(abalone) <- abalone.names
abalone[1:10, ]

# converting the chr value of column sex to numeric values
abalone$sex <- as.numeric(factor(abalone$sex))
abalone[1:10, ]
str(abalone)

# there are no more chr values in the dataset so we don't need to convert anything
# summary of the dataset
summary(abalone)

# checking if there are any NA values in the dataset
sum(is.na(abalone))

# describing the statistics of the dataset
install.packages("psych")
library(psych)
describe(abalone)

# plotting the variables in a pairwise manner
pairs(abalone)

# "sex" column can be stripped off as it is a categorical variable that is not relevant for predicting the age of the specimen
# it's also seen from pairwise plot that "sex" is not related with any other variable
abalone.df <- abalone[, -1]
abalone.df[1:10, ]

# checking the structure again
str(abalone.df)

# using other packages to do pairwise plotting to understand relation between the variables
install.packages("ggplot2")
install.packages("GGally")
library(ggplot2)
library(GGally)
ggpairs(abalone.df)

# Creating a new plot with length on the x-axis, diameter on the y-axis, and height on the z-axis
install.packages("scatterplot3d")
library(scatterplot3d)
s3d <- scatterplot3d(abalone.df[, c("length", "diameter", "height")], pch = 20, main = "Abalone Measurements", xlab = "Length", ylab = "Diameter", zlab = "Height")

# Add a legend to the plot
legend("topright", legend = c("Abalone"), pch = 20, col = "black", bty = "n")


# normalization
normalize <- function(x) {((x-min(x)) / (max(x)-min(x)))}
normalize

# apply normalization on the data
abalone.norm <- as.data.frame(lapply(abalone.df, normalize))
abalone.norm[1:10, ]

# finding the smallest value in length column 
abalone.df.length<- min(abalone.df$length)
abalone.df.length

# finding the smallest value in shucked_weight column 
abalone.df.shucked_weight<- min(abalone.df$shucked_weight)
abalone.df.shucked_weight

# finding the smallest value in viscera_weight
abalone.df.viscera_weight<- min(abalone.df$viscera_weight)
abalone.df.viscera_weight

# finding the largest value in length column 
abalone.df.max.length<- max(abalone.df$length)
abalone.df.max.length

# finding the largest value in shucked_weight column 
abalone.df.max.shucked_weight<- max(abalone.df$shucked_weight)
abalone.df.max.shucked_weight

# finding the largest value in viscera_weight
abalone.df.max.viscera_weight<- max(abalone.df$viscera_weight)
abalone.df.max.viscera_weight

# to get back to the original values, later:
# x = norm(x) * (max(x)-min(x) ) + min(x)
# it is working correctly
length<- 0.5135135*(abalone.df.max.length-abalone.df.length)+ abalone.df.length
length

# Z-Score Normalization
zscore<- function(x){(x-mean(x))/sd(x)}
zscore(c(10,20,30,40,50))


abalone.znorm<- as.data.frame(lapply(abalone.df, scale))
abalone.znorm[1:10,]

# to get back to x, we use x = sd(x) + mean(x)

# correlation
library(corrplot)
abalone.cor <- cor(abalone.df)
corrplot(abalone.cor)
corrplot(abalone.cor, type = "upper", method = "color", tl.col = "black")
corrplot(abalone.cor, type = "lower", method = "color", tl.col = "black")

# k-means clustering
install.packages("factoextra")
library(factoextra)

# with k=2
abalone.kmeans2 <- kmeans(abalone.znorm, centers = 2, nstart = 10)
str(abalone.kmeans2)
abalone.kmeans2

# k-means cluster plot for different k values
fviz_cluster(abalone.kmeans2, data = abalone.znorm, geom = "point")
abalone.kmeans3 <- kmeans(abalone.znorm, centers = 3, nstart = 10)
fviz_cluster(abalone.kmeans3, data = abalone.znorm, geom = "point")
abalone.kmeans4 <- kmeans(abalone.znorm, centers = 4, nstart = 10)
fviz_cluster(abalone.kmeans4, data = abalone.znorm, geom = "point")
abalone.kmeans5 <- kmeans(abalone.znorm, centers = 5, nstart = 10)
fviz_cluster(abalone.kmeans5, data = abalone.znorm, geom = "point")

# calculate appropriate number of clusters
wssplot <- function(data, nc=15, seed=1234) {
  wss <- (nrow(data)-1)*sum(apply(data, 2, var))
  for (i in 2:nc) {
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers = i)$withinss)
  }
  plot(1:nc, wss, type="b", xlab="Number of clusters", ylab="Within group sum of squares")
}
wssplot(abalone.znorm, 10, 12342)

# finding number of clusters using fviz_nbclust function
fviz_nbclust(abalone.znorm, kmeans, method = "wss", k.max = 10, verbose = TRUE)


# KNN

# splitting data into 70-30
abalone.znorm.rows <- nrow(abalone.znorm)
abalone.znorm.sample_70 <- 0.7
abalone.rows_70 <- abalone.znorm.rows * abalone.znorm.sample_70
abalone.rows_70
abalone.train.index_70 <- sample(abalone.znorm.rows, abalone.rows_70)
length(abalone.train.index_70)
abalone.train_70 <- abalone.znorm[abalone.train.index_70, ]
abalone.train_70[1:10, ]
abalone.test_70 <- abalone.znorm[-abalone.train.index_70, ]
abalone.test_70[1:10, ]
dim(abalone.test_70)
dim(abalone.train_70)

# splitting data into 60-40
abalone.znorm.rows <- nrow(abalone.znorm)
abalone.znorm.sample_60 <- 0.6
abalone.rows_60 <- abalone.znorm.rows * abalone.znorm.sample_60
abalone.rows_60
abalone.train.index_60 <- sample(abalone.znorm.rows, abalone.rows_60)
length(abalone.train.index_60)
abalone.train_60 <- abalone.znorm[abalone.train.index_60, ]
abalone.train_60[1:10, ]
abalone.test_60 <- abalone.znorm[-abalone.train.index_60, ]
abalone.test_60[1:10, ]
dim(abalone.test_60)
dim(abalone.train_60)

# splitting data into 50-50
abalone.znorm.rows <- nrow(abalone.znorm)
abalone.znorm.sample_50 <- 0.5
abalone.rows_50 <- abalone.znorm.rows * abalone.znorm.sample_50
abalone.rows_50
abalone.train.index_50 <- sample(abalone.znorm.rows, abalone.rows_50)
length(abalone.train.index_50)
abalone.train_50 <- abalone.znorm[abalone.train.index_50, ]
abalone.train_50[1:10, ]
abalone.test_50 <- abalone.znorm[-abalone.train.index_50, ]
abalone.test_50[1:10, ]
dim(abalone.test_50)
dim(abalone.train_50)


# knn with k=5 and 70-30 split
abalone.train_70_k5 <- kmeans(abalone.train_70, centers = 5)
abalone.train_70_k5

install.packages("class")
library(class)
abalone.test_70_k5 <- knn(abalone.train_70, abalone.test_70, abalone.train_70_k5$cluster, k = 5)
abalone.test_70_k5

# apply kmeans test data to generate labels for the test records
abalone.test_70_kmeans_k5 <- kmeans(abalone.test_70, centers = 5)
abalone.test_70_kmeans_k5

abalone.test_70_k5.labels <- abalone.test_70_kmeans_k5$cluster
length(abalone.test_70_k5.labels)
abalone.test_70_k5.labels

# linear modelling using GLM
abalone.train_70.glm <- glm(formula = rings ~ length + diameter + height + whole_weight + shucked_weight +  viscera_weight + shell_weight, family = gaussian, data = abalone.train_70)
summary(abalone.train_70.glm)

# anova - analysis of variance on the model result
abalone.train_70.glm.anova <- anova(abalone.train_70.glm, test = "Chisq")
abalone.train_70.glm.anova

# plotting graphs
plot(abalone.train_70.glm)

# predict
abalone.test_70.predict <- predict(abalone.train_70.glm, newdata = abalone.test_70)
length(abalone.test_70.predict)
summary(abalone.test_70.predict)

# confidence intervals
confint(abalone.train_70.glm)

# comparing actual vs prediction
abalone.test_70.predict.k5 <- kmeans(abalone.test_70.predict, centers = 5)
abalone.test_70.predict.k5

install.packages("gmodels")
library(gmodels)
abalone.test_70.ct.k5 <- CrossTable(abalone.test_70.predict.k5$cluster, abalone.test_70_kmeans_k5$cluster, prop.chisq = TRUE)

# confusion matrix
abalone.test_70.cm_k5 <- abalone.test_70.ct.k5$t
abalone.test_70.cm_k5

# calculate the metrics
abalone.test_70.accuracy <- sum(diag(abalone.test_70.cm_k5)) / sum(abalone.test_70.cm_k5)
abalone.test_70.precision <- diag(abalone.test_70.cm_k5) / colSums(abalone.test_70.cm_k5)
abalone.test_70.recall <- diag(abalone.test_70.cm_k5) / rowSums(abalone.test_70.cm_k5)
abalone.test_70.specificity <- sapply(1:nrow(abalone.test_70.cm_k5), function(i){
  TN <- sum(abalone.test_70.cm_k5[-i, -i])
  FP <- sum(abalone.test_70.cm_k5[-i, i])
  TN / (TN + FP)
})
abalone.test_70.error <- 1 - abalone.test_70.accuracy
# print results
abalone.test_70.accuracy
abalone.test_70.precision
abalone.test_70.recall
abalone.test_70.specificity
abalone.test_70.error

# knn with k=5 and 50-50 split
abalone.train_50_k5 <- kmeans(abalone.train_50, centers = 5)
abalone.train_50_k5

abalone.test_50_k5 <- knn(abalone.train_50, abalone.test_50, abalone.train_50_k5$cluster, k = 5)
abalone.test_50_k5

# apply kmeans test data to generate labels for the test records
abalone.test_50_kmeans_k5 <- kmeans(abalone.test_50, centers = 5)
abalone.test_50_kmeans_k5

abalone.test_50_k5.labels <- abalone.test_50_kmeans_k5$cluster
length(abalone.test_50_k5.labels)
abalone.test_50_k5.labels

# linear modelling using GLM
abalone.train_50.glm <- glm(formula = rings ~ length + diameter + height + whole_weight + shucked_weight +  viscera_weight + shell_weight, family = gaussian, data = abalone.train_70)
summary(abalone.train_50.glm)

# anova - analysis of variance on the model result
abalone.train_50.glm.anova <- anova(abalone.train_50.glm, test = "Chisq")
abalone.train_50.glm.anova

# plotting graphs
plot(abalone.train_50.glm)

# predict
abalone.test_50.predict <- predict(abalone.train_50.glm, newdata = abalone.test_50)
length(abalone.test_50.predict)
summary(abalone.test_50.predict)

# confidence intervals
confint(abalone.train_50.glm)

# comparing actual vs prediction
abalone.test_50.predict.k5 <- kmeans(abalone.test_50.predict, centers = 5)
abalone.test_50.predict.k5


abalone.test_50.ct.k5 <- CrossTable(abalone.test_50.predict.k5$cluster, abalone.test_50_kmeans_k5$cluster, prop.chisq = TRUE)

# confusion matrix
abalone.test_50.cm_k5 <- abalone.test_50.ct.k5$t
abalone.test_50.cm_k5

# calculate the metrics
abalone.test_50.accuracy <- sum(diag(abalone.test_50.cm_k5)) / sum(abalone.test_50.cm_k5)
abalone.test_50.precision <- diag(abalone.test_50.cm_k5) / colSums(abalone.test_50.cm_k5)
abalone.test_50.recall <- diag(abalone.test_50.cm_k5) / rowSums(abalone.test_50.cm_k5)
abalone.test_50.specificity <- sapply(1:nrow(abalone.test_50.cm_k5), function(i){
  TN <- sum(abalone.test_50.cm_k5[-i, -i])
  FP <- sum(abalone.test_50.cm_k5[-i, i])
  TN / (TN + FP)
})
abalone.test_50.error <- 1 - abalone.test_50.accuracy
# print results
abalone.test_50.accuracy
abalone.test_50.precision
abalone.test_50.recall
abalone.test_50.specificity
abalone.test_50.error

# knn with k=5 and 60-40 split
abalone.train_60_k5 <- kmeans(abalone.train_60, centers = 5)
abalone.train_60_k5

abalone.test_60_k5 <- knn(abalone.train_60, abalone.test_60, abalone.train_60_k5$cluster, k = 5)
abalone.test_60_k5

# apply kmeans test data to generate labels for the test records
abalone.test_60_kmeans_k5 <- kmeans(abalone.test_60, centers = 5)
abalone.test_60_kmeans_k5

abalone.test_60_k5.labels <- abalone.test_60_kmeans_k5$cluster
length(abalone.test_60_k5.labels)
abalone.test_60_k5.labels

# linear modelling using GLM
abalone.train_60.glm <- glm(formula = rings ~ length + diameter + height + whole_weight + shucked_weight +  viscera_weight + shell_weight, family = gaussian, data = abalone.train_70)
summary(abalone.train_60.glm)

# anova - analysis of variance on the model result
abalone.train_60.glm.anova <- anova(abalone.train_60.glm, test = "Chisq")
abalone.train_60.glm.anova

# plotting graphs
plot(abalone.train_60.glm)

# predict
abalone.test_60.predict <- predict(abalone.train_60.glm, newdata = abalone.test_60)
length(abalone.test_60.predict)
summary(abalone.test_60.predict)

# confidence intervals
confint(abalone.train_60.glm)

# comparing actual vs prediction
abalone.test_60.predict.k5 <- kmeans(abalone.test_60.predict, centers = 5)
abalone.test_60.predict.k5

abalone.test_60.ct.k5 <- CrossTable(abalone.test_60.predict.k7$cluster, abalone.test_60_kmeans_k5$cluster, prop.chisq = TRUE)

# confusion matrix
abalone.test_60.cm_k5 <- abalone.test_60.ct.k5$t
abalone.test_60.cm_k5

# calculate the metrics
abalone.test_60.accuracy <- sum(diag(abalone.test_60.cm_k5)) / sum(abalone.test_60.cm_k5)
abalone.test_60.precision <- diag(abalone.test_60.cm_k5) / colSums(abalone.test_60.cm_k5)
abalone.test_60.recall <- diag(abalone.test_60.cm_k5) / rowSums(abalone.test_60.cm_k5)
abalone.test_60.specificity <- sapply(1:nrow(abalone.test_60.cm_k7), function(i){
  TN <- sum(abalone.test_60.cm_k5[-i, -i])
  FP <- sum(abalone.test_60.cm_k5[-i, i])
  TN / (TN + FP)
})
abalone.test_60.error <- 1 - abalone.test_60.accuracy
# print results
abalone.test_60.accuracy
abalone.test_60.precision
abalone.test_60.recall
abalone.test_60.specificity
abalone.test_60.error

# knn with k=7 and 70-30 split
abalone.train_70_k7 <- kmeans(abalone.train_70, centers = 7)
abalone.train_70_k7

abalone.test_70_k7 <- knn(abalone.train_70, abalone.test_70, abalone.train_70_k7$cluster, k = 7)
abalone.test_70_k7

# apply kmeans test data to generate labels for the test records
abalone.test_70_kmeans_k7 <- kmeans(abalone.test_70, centers = 7)
abalone.test_70_kmeans_k7

abalone.test_70_k7.labels <- abalone.test_70_kmeans_k7$cluster
length(abalone.test_70_k7.labels)
abalone.test_70_k7.labels

# linear modelling using GLM
abalone.train_70.glm <- glm(formula = rings ~ length + diameter + height + whole_weight + shucked_weight +  viscera_weight + shell_weight, family = gaussian, data = abalone.train_70)
summary(abalone.train_70.glm)

# anova - analysis of variance on the model result
abalone.train_70.glm.anova <- anova(abalone.train_70.glm, test = "Chisq")
abalone.train_70.glm.anova

# plotting graphs
plot(abalone.train_70.glm)


# predict
abalone.test_70.predict <- predict(abalone.train_70.glm, newdata = abalone.test_70)
length(abalone.test_70.predict)
summary(abalone.test_70.predict)

# confidence intervals
confint(abalone.train_70.glm)

# comparing actual vs prediction
abalone.test_70.predict.k7 <- kmeans(abalone.test_70.predict, centers = 7)
abalone.test_70.predict.k7


abalone.test_70.ct.k7 <- CrossTable(abalone.test_70.predict.k7$cluster, abalone.test_70_kmeans_k7$cluster, prop.chisq = TRUE)

# confusion matrix
abalone.test_70.cm_k7 <- abalone.test_70.ct.k7$t
abalone.test_70.cm_k7

# calculate the metrics
abalone.test_70.accuracy <- sum(diag(abalone.test_70.cm_k7)) / sum(abalone.test_70.cm_k7)
abalone.test_70.precision <- diag(abalone.test_70.cm_k7) / colSums(abalone.test_70.cm_k7)
abalone.test_70.recall <- diag(abalone.test_70.cm_k7) / rowSums(abalone.test_70.cm_k7)
abalone.test_70.specificity <- sapply(1:nrow(abalone.test_70.cm_k7), function(i){
  TN <- sum(abalone.test_70.cm_k7[-i, -i])
  FP <- sum(abalone.test_70.cm_k7[-i, i])
  TN / (TN + FP)
})
abalone.test_70.error <- 1 - abalone.test_70.accuracy
# print results
abalone.test_70.accuracy
abalone.test_70.precision
abalone.test_70.recall
abalone.test_70.specificity
abalone.test_70.error

# knn with k=7 and 60-40 split
abalone.train_60_k7 <- kmeans(abalone.train_60, centers = 7)
abalone.train_60_k7

abalone.test_60_k7 <- knn(abalone.train_60, abalone.test_60, abalone.train_60_k7$cluster, k = 7)
abalone.test_60_k7

# apply kmeans test data to generate labels for the test records
abalone.test_60_kmeans_k7 <- kmeans(abalone.test_60, centers = 7)
abalone.test_60_kmeans_k7

abalone.test_60_k7.labels <- abalone.test_60_kmeans_k7$cluster
length(abalone.test_60_k7.labels)
abalone.test_60_k7.labels

# linear modelling using GLM
abalone.train_60.glm <- glm(formula = rings ~ length + diameter + height + whole_weight + shucked_weight +  viscera_weight + shell_weight, family = gaussian, data = abalone.train_60)
summary(abalone.train_60.glm)

# anova - analysis of variance on the model result
abalone.train_60.glm.anova <- anova(abalone.train_60.glm, test = "Chisq")
abalone.train_60.glm.anova

# plotting graphs
plot(abalone.train_60.glm)

# predict
abalone.test_60.predict <- predict(abalone.train_60.glm, newdata = abalone.test_60)
length(abalone.test_60.predict)
summary(abalone.test_60.predict)

# confidence intervals
confint(abalone.train_60.glm)

# comparing actual vs prediction
abalone.test_60.predict.k7 <- kmeans(abalone.test_60.predict, centers = 7)
abalone.test_60.predict.k7

abalone.test_60.ct.k7 <- CrossTable(abalone.test_60.predict.k7$cluster, abalone.test_60_kmeans_k7$cluster, prop.chisq = TRUE)

# confusion matrix
abalone.test_60.cm_k7 <- abalone.test_60.ct.k7$t
abalone.test_60.cm_k7
25

# calculate the metrics
abalone.test_60.accuracy <- sum(diag(abalone.test_60.cm_k7)) / sum(abalone.test_60.cm_k7)
abalone.test_60.precision <- diag(abalone.test_60.cm_k7) / colSums(abalone.test_60.cm_k7)
abalone.test_60.recall <- diag(abalone.test_60.cm_k7) / rowSums(abalone.test_60.cm_k7)
abalone.test_60.specificity <- sapply(1:nrow(abalone.test_60.cm_k7), function(i){
  TN <- sum(abalone.test_60.cm_k7[-i, -i])
  FP <- sum(abalone.test_60.cm_k7[-i, i])
  TN / (TN + FP)
})
abalone.test_60.error <- 1 - abalone.test_60.accuracy
# print results
abalone.test_60.accuracy
abalone.test_60.precision
abalone.test_60.recall
abalone.test_60.specificity
abalone.test_60.error

# knn with k=7 and 50-50 split
abalone.train_50_k7 <- kmeans(abalone.train_50, centers = 7)
abalone.train_50_k7

abalone.test_50_k7 <- knn(abalone.train_50, abalone.test_50, abalone.train_50_k7$cluster, k = 7)
abalone.test_50_k7

# apply kmeans test data to generate labels for the test records
abalone.test_50_kmeans_k7 <- kmeans(abalone.test_50, centers = 7)
abalone.test_50_kmeans_k7

abalone.test_50_k7.labels <- abalone.test_50_kmeans_k7$cluster
length(abalone.test_50_k7.labels)
abalone.test_50_k7.labels

# linear modelling using GLM
abalone.train_50.glm <- glm(formula = rings ~ length + diameter + height + whole_weight + shucked_weight +  viscera_weight + shell_weight, family = gaussian, data = abalone.train_70)
summary(abalone.train_50.glm)

# anova - analysis of variance on the model result
abalone.train_50.glm.anova <- anova(abalone.train_50.glm, test = "Chisq")
abalone.train_50.glm.anova

# plotting graphs
plot(abalone.train_50.glm)

# predict
abalone.test_50.predict <- predict(abalone.train_50.glm, newdata = abalone.test_50)
length(abalone.test_50.predict)
summary(abalone.test_50.predict)

# confidence intervals
confint(abalone.train_50.glm)

# comparing actual vs prediction
abalone.test_50.predict.k7 <- kmeans(abalone.test_50.predict, centers = 7)
abalone.test_50.predict.k7

abalone.test_50.ct.k7 <- CrossTable(abalone.test_50.predict.k7$cluster, abalone.test_50_kmeans_k7$cluster, prop.chisq = TRUE)

# confusion matrix
abalone.test_50.cm_k7 <- abalone.test_50.ct.k7$t
abalone.test_50.cm_k7

# calculate the metrics
abalone.test_50.accuracy <- sum(diag(abalone.test_50.cm_k7)) / sum(abalone.test_50.cm_k7)
abalone.test_50.precision <- diag(abalone.test_50.cm_k7) / colSums(abalone.test_50.cm_k7)
abalone.test_50.recall <- diag(abalone.test_50.cm_k7) / rowSums(abalone.test_50.cm_k7)
abalone.test_50.specificity <- sapply(1:nrow(abalone.test_50.cm_k7), function(i){
  TN <- sum(abalone.test_50.cm_k7[-i, -i])
  FP <- sum(abalone.test_50.cm_k7[-i, i])
  TN / (TN + FP)
})
abalone.test_50.error <- 1 - abalone.test_50.accuracy
# print results
abalone.test_50.accuracy
abalone.test_50.precision
abalone.test_50.recall
abalone.test_50.specificity
abalone.test_50.error

# knn with k=9 and 70-30 split
abalone.train_70_k9 <- kmeans(abalone.train_70, centers = 9)
abalone.train_70_k9

abalone.test_70_k9 <- knn(abalone.train_70, abalone.test_70, abalone.train_70_k9$cluster, k = 9)
abalone.test_70_k9

# apply kmeans test data to generate labels for the test records
abalone.test_70_kmeans_k9 <- kmeans(abalone.test_70, centers = 9)
abalone.test_70_kmeans_k9

abalone.test_70_k9.labels <- abalone.test_70_kmeans_k9$cluster
length(abalone.test_70_k9.labels)
abalone.test_70_k9.labels

# linear modelling using GLM
abalone.train_70.glm <- glm(formula = rings ~ length + diameter + height + whole_weight + shucked_weight +  viscera_weight + shell_weight, family = gaussian, data = abalone.train_70)
summary(abalone.train_70.glm)

# anova - analysis of variance on the model result
abalone.train_70.glm.anova <- anova(abalone.train_70.glm, test = "Chisq")
abalone.train_70.glm.anova

# plotting graphs
plot(abalone.train_70.glm)

# predict
abalone.test_70.predict <- predict(abalone.train_70.glm, newdata = abalone.test_70)
length(abalone.test_70.predict)
summary(abalone.test_70.predict)

# confidence intervals
confint(abalone.train_70.glm)

# comparing actual vs prediction
abalone.test_70.predict.k9 <- kmeans(abalone.test_70.predict, centers = 9)
abalone.test_70.predict.k9

abalone.test_70.ct.k9 <- CrossTable(abalone.test_70.predict.k9$cluster, abalone.test_70_kmeans_k9$cluster, prop.chisq = TRUE)

# confusion matrix
abalone.test_70.cm_k9 <- abalone.test_70.ct.k9$t
abalone.test_70.cm_k9
25

# calculate the metrics
abalone.test_70.accuracy <- sum(diag(abalone.test_70.cm_k9)) / sum(abalone.test_70.cm_k9)
abalone.test_70.precision <- diag(abalone.test_70.cm_k9) / colSums(abalone.test_70.cm_k9)
abalone.test_70.recall <- diag(abalone.test_70.cm_k9) / rowSums(abalone.test_70.cm_k9)
abalone.test_70.specificity <- sapply(1:nrow(abalone.test_70.cm_k9), function(i){
  TN <- sum(abalone.test_70.cm_k9[-i, -i])
  FP <- sum(abalone.test_70.cm_k9[-i, i])
  TN / (TN + FP)
})
abalone.test_70.error <- 1 - abalone.test_70.accuracy
# print results
abalone.test_70.accuracy
abalone.test_70.precision
abalone.test_70.recall
abalone.test_70.specificity
abalone.test_70.error

# knn with k=9 and 60-40 split
abalone.train_60_k9 <- kmeans(abalone.train_60, centers = 9)
abalone.train_60_k9

abalone.test_60_k9 <- knn(abalone.train_60, abalone.test_60, abalone.train_60_k9$cluster, k = 9)
abalone.test_60_k9

# apply kmeans test data to generate labels for the test records
abalone.test_60_kmeans_k9 <- kmeans(abalone.test_60, centers = 9)
abalone.test_60_kmeans_k9

abalone.test_60_k9.labels <- abalone.test_60_kmeans_k9$cluster
length(abalone.test_60_k9.labels)
abalone.test_60_k9.labels

# linear modelling using GLM
abalone.train_60.glm <- glm(formula = rings ~ length + diameter + height + whole_weight + shucked_weight +  viscera_weight + shell_weight, family = gaussian, data = abalone.train_60)
summary(abalone.train_60.glm)

# anova - analysis of variance on the model result
abalone.train_60.glm.anova <- anova(abalone.train_60.glm, test = "Chisq")
abalone.train_60.glm.anova

# plotting graphs
plot(abalone.train_60.glm)

# predict
abalone.test_60.predict <- predict(abalone.train_60.glm, newdata = abalone.test_60)
length(abalone.test_60.predict)
summary(abalone.test_60.predict)

# confidence intervals
confint(abalone.train_60.glm)

# comparing actual vs prediction
abalone.test_60.predict.k9 <- kmeans(abalone.test_60.predict, centers = 9)
abalone.test_60.predict.k9

abalone.test_60.ct.k9 <- CrossTable(abalone.test_60.predict.k9$cluster, abalone.test_60_kmeans_k9$cluster, prop.chisq = TRUE)

# confusion matrix
abalone.test_60.cm_k9 <- abalone.test_60.ct.k9$t
abalone.test_60.cm_k9
25

# calculate the metrics
abalone.test_60.accuracy <- sum(diag(abalone.test_60.cm_k9)) / sum(abalone.test_60.cm_k9)
abalone.test_60.precision <- diag(abalone.test_60.cm_k9) / colSums(abalone.test_60.cm_k9)
abalone.test_60.recall <- diag(abalone.test_60.cm_k9) / rowSums(abalone.test_60.cm_k9)
abalone.test_60.specificity <- sapply(1:nrow(abalone.test_60.cm_k9), function(i){
  TN <- sum(abalone.test_60.cm_k9[-i, -i])
  FP <- sum(abalone.test_60.cm_k9[-i, i])
  TN / (TN + FP)
})
abalone.test_60.error <- 1 - abalone.test_60.accuracy
# print results
abalone.test_60.accuracy
abalone.test_60.precision
abalone.test_60.recall
abalone.test_60.specificity
abalone.test_60.error

# knn with k=9 and 60-40 split
abalone.train_60_k9 <- kmeans(abalone.train_60, centers = 9)
abalone.train_60_k9

abalone.test_60_k9 <- knn(abalone.train_60, abalone.test_60, abalone.train_60_k9$cluster, k = 9)
abalone.test_60_k9

# apply kmeans test data to generate labels for the test records
abalone.test_60_kmeans_k9 <- kmeans(abalone.test_60, centers = 9)
abalone.test_60_kmeans_k9

abalone.test_60_k9.labels <- abalone.test_60_kmeans_k9$cluster
length(abalone.test_60_k9.labels)
abalone.test_60_k9.labels

# linear modelling using GLM
abalone.train_60.glm <- glm(formula = rings ~ length + diameter + height + whole_weight + shucked_weight +  viscera_weight + shell_weight, family = gaussian, data = abalone.train_70)
summary(abalone.train_60.glm)

# anova - analysis of variance on the model result
abalone.train_60.glm.anova <- anova(abalone.train_60.glm, test = "Chisq")
abalone.train_60.glm.anova

# plotting graphs
plot(abalone.train_60.glm)

# predict
abalone.test_60.predict <- predict(abalone.train_60.glm, newdata = abalone.test_60)
length(abalone.test_60.predict)
summary(abalone.test_60.predict)

# confidence intervals
confint(abalone.train_60.glm)

# comparing actual vs prediction
abalone.test_60.predict.k9 <- kmeans(abalone.test_60.predict, centers = 9)
abalone.test_60.predict.k9

abalone.test_60.ct.k9 <- CrossTable(abalone.test_60.predict.k7$cluster, abalone.test_60_kmeans_k9$cluster, prop.chisq = TRUE)

# confusion matrix
abalone.test_60.cm_k9 <- abalone.test_60.ct.k9$t
abalone.test_60.cm_k9

# calculate the metrics
abalone.test_60.accuracy <- sum(diag(abalone.test_60.cm_k9)) / sum(abalone.test_60.cm_k9)
abalone.test_60.precision <- diag(abalone.test_60.cm_k9) / colSums(abalone.test_60.cm_k9)
abalone.test_60.recall <- diag(abalone.test_60.cm_k9) / rowSums(abalone.test_60.cm_k9)
abalone.test_60.specificity <- sapply(1:nrow(abalone.test_60.cm_k9), function(i){
  TN <- sum(abalone.test_60.cm_k9[-i, -i])
  FP <- sum(abalone.test_60.cm_k9[-i, i])
  TN / (TN + FP)
})
abalone.test_60.error <- 1 - abalone.test_60.accuracy
# print results
abalone.test_60.accuracy
abalone.test_60.precision
abalone.test_60.recall
abalone.test_60.specificity
abalone.test_60.error


# knn with k=9 and 50-50 split
abalone.train_50_k9 <- kmeans(abalone.train_50, centers = 9)
abalone.train_50_k9

abalone.test_50_k9 <- knn(abalone.train_50, abalone.test_50, abalone.train_50_k9$cluster, k = 9)
abalone.test_50_k9

# apply kmeans test data to generate labels for the test records
abalone.test_50_kmeans_k9 <- kmeans(abalone.test_50, centers = 9)
abalone.test_50_kmeans_k9

abalone.test_50_k9.labels <- abalone.test_50_kmeans_k9$cluster
length(abalone.test_50_k9.labels)
abalone.test_50_k9.labels

# linear modelling using GLM
abalone.train_50.glm <- glm(formula = rings ~ length + diameter + height + whole_weight + shucked_weight +  viscera_weight + shell_weight, family = gaussian, data = abalone.train_70)
summary(abalone.train_50.glm)

# anova - analysis of variance on the model result
abalone.train_50.glm.anova <- anova(abalone.train_50.glm, test = "Chisq")
abalone.train_50.glm.anova

# plotting graphs
plot(abalone.train_50.glm)

# predict
abalone.test_50.predict <- predict(abalone.train_50.glm, newdata = abalone.test_50)
length(abalone.test_50.predict)
summary(abalone.test_50.predict)

# confidence intervals
confint(abalone.train_50.glm)

# comparing actual vs prediction
abalone.test_50.predict.k9 <- kmeans(abalone.test_50.predict, centers = 9)
abalone.test_50.predict.k9


abalone.test_50.ct.k9 <- CrossTable(abalone.test_50.predict.k9$cluster, abalone.test_50_kmeans_k9$cluster, prop.chisq = TRUE)

# confusion matrix
abalone.test_50.cm_k9 <- abalone.test_50.ct.k9$t
abalone.test_50.cm_k9

# calculate the metrics
abalone.test_50.accuracy <- sum(diag(abalone.test_50.cm_k9)) / sum(abalone.test_50.cm_k9)
abalone.test_50.precision <- diag(abalone.test_50.cm_k9) / colSums(abalone.test_50.cm_k9)
abalone.test_50.recall <- diag(abalone.test_50.cm_k9) / rowSums(abalone.test_50.cm_k9)
abalone.test_50.specificity <- sapply(1:nrow(abalone.test_50.cm_k9), function(i){
  TN <- sum(abalone.test_50.cm_k9[-i, -i])
  FP <- sum(abalone.test_50.cm_k9[-i, i])
  TN / (TN + FP)
})
abalone.test_50.error <- 1 - abalone.test_50.accuracy
# print results
abalone.test_50.accuracy
abalone.test_50.precision
abalone.test_50.recall
abalone.test_50.specificity
abalone.test_50.error


# linear modelling on the data
# 70-30 split
abalone.train_70.glm <- glm(formula = rings ~ length + diameter + height + whole_weight + shucked_weight +  viscera_weight + shell_weight, family = gaussian, data = abalone.train_70)
summary(abalone.train_70.glm)

# 60-40 split
abalone.train_60.glm <- glm(formula = rings ~ length + diameter + height + whole_weight + shucked_weight +  viscera_weight + shell_weight, family = gaussian, data = abalone.train_60)
summary(abalone.train_60.glm)

# 50-50 split
abalone.train_50.glm <- glm(formula = rings ~ length + diameter + height + whole_weight + shucked_weight +  viscera_weight + shell_weight, family = gaussian, data = abalone.train_50)
summary(abalone.train_50.glm)

# prediction using dependent variables
# 70-30 split
summary(abalone.test_70.predict)
summary(abalone.test_70$rings)


# 60-40 split
summary(abalone.test_60.predict)
summary(abalone.test_60$rings)

# 50-50 split
summary(abalone.test_50.predict)
summary(abalone.test_50$rings)


# analysis of variance
# 70-30 split
abalone.train_70.glm.anova <- anova(abalone.train_70.glm, test = "Chisq")
abalone.train_70.glm.anova

# 60-40 split
abalone.train_60.glm.anova <- anova(abalone.train_60.glm, test = "Chisq")
abalone.train_60.glm.anova


# 50-50 split
abalone.train_50.glm.anova <- anova(abalone.train_50.glm, test = "Chisq")
abalone.train_50.glm.anova


# confidence intervals
# 70-30 split
confint(abalone.train_70.glm)

# 60-40 split
confint(abalone.train_60.glm)

# 50-50 split
confint(abalone.train_50.glm)






