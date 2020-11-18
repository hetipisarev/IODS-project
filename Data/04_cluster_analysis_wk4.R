# Author: Heti Pisarev
# Date: 18.11.2020
# Comment: 
# Cluster and classification
# 

##------------------
## Analysis part
##------------------
rm(list = ls())
library(MASS); library(corrplot); library(tidyverse)

#### 4.2.2 Load and describe data

# Load the data
data("Boston")

# Explore the dataset
str(Boston)                                  
dim(Boston)


#### 4.2.3 Overview of the data

# descriptive statistics
summary(Boston) 

# histograms
par(mfrow=c(4,4), mai=c(0.6, 0.6, 0.2, 0.2), cex=0.7)
for (i in 1:14) hist(Boston[,i], main=names(Boston)[i]) 

# calculate the correlation matrix and round it
cor_matrix<-cor(Boston, method="spearman") 
par(mfrow=c(1,1))
# print the correlation matrix
round(cor_matrix, 2)

# visualize the correlation matrix
corrplot(cor_matrix, method="circle")

# More fancy correlation graph - just for memorize
# corrplot(cor_matrix, method="circle", type="upper", cl.pos = "b", tl.pos = "d" , tl.cex = 0.6)

### 4.2.4 prepare dataset for discrminant analysis

# center and standardize variables
boston_scaled <- as.data.frame(scale(Boston))

# print out summaries of the scaled variables
summary(boston_scaled)

# create a quantile vector of crim and print it
bins <- quantile(boston_scaled$crim)
bins

# create a categorical variable 'crime'
boston_scaled$crime <- cut(boston_scaled$crim, breaks = bins, 
             include.lowest = TRUE, 
             label=c("low", "med_low", "med_high", "high"))

# look at the table of the new factor crime, compare original one (not scaled)
table(boston_scaled$crime)

# visualize new vs old crime
par(mai=c(0.6,0.6,0.2,0.2))
plot(log(Boston$crim), boston_scaled$crime, 
     main = "New categorical variable vs original", 
     las=1, cex=0.7)

# remove original crim from the dataset
boston_scaled <- dplyr::select(boston_scaled, -crim)


# Divide dataset to training (80%) and testing (20%)
# number of rows in the Boston dataset 
n <- dim(boston_scaled)[1]

# choose randomly 80% of the rows
ind <- sample(n,  size = n * 0.8)

# create train set
train <- boston_scaled[ind,]

# create test set 
test <- boston_scaled[-ind,]



### 4.2.5 Fit the linear discriminant analysis

# Linear discriminant analysis
lda.fit <- lda(crime ~ ., data = train)

# Print the lda.fit object
lda.fit

# the function for lda biplot arrows
lda.arrows <- function(x, myscale = 1, arrow_heads = 0.1, color = "red", tex = 0.75, choices = c(1,2)){
  heads <- coef(x)
  arrows(x0 = 0, y0 = 0, 
         x1 = myscale * heads[,choices[1]], 
         y1 = myscale * heads[,choices[2]], col=color, length = arrow_heads)
  text(myscale * heads[,choices], labels = row.names(heads), 
       cex = tex, col=color, pos=3)
}

# target classes as numeric
classes <- as.numeric(train$crime)

# plot the lda results
plot(lda.fit , dimen = 2, col=classes, pch=classes)
lda.arrows(lda.fit, myscale = 1)

### 4.2.6 Test the model

# save the correct classes from test data
test$correct_classes <- test$crime

# remove the crime variable from test data
test <- dplyr::select(test, -crime)

# predict classes with test data
lda.pred <- predict(lda.fit, newdata = test)

# cross tabulate the results
a = table(correct = test$correct_classes, predicted = lda.pred$class)
a
# correct predictions
sum(a[1,1]+a[2,2]+a[3,3]+a[4,4]) / sum(a)
 
### 4.2.7 Cluster analysis

# euclidean distance matrix
dist_eu <- dist(boston_scaled)

# look at the summary of the distances
summary(dist_eu)

# manhattan distance matrix
dist_man <- dist(boston_scaled, method="manhattan")
# look at the summary of the distances
summary(dist_man)

# determine the number of clusters
set.seed(123)
k_max <- 10

# calculate the total within sum of squares
twcss <- sapply(1:k_max, function(k){kmeans(dist_eu, k)$tot.withinss})

# visualize the results
qplot(x = 1:k_max, y = twcss, geom = 'line')


# k-means clustering
km <-kmeans(dist_eu, centers = 2)

# plot the Boston dataset with clusters
pairs(Boston, col = km$cluster)


# k-means clustering
km <-kmeans(dist_eu, centers = 2)

# plot the Boston dataset with clusters
pairs(Boston, col = km$cluster)



boston_scaled <- as.data.frame(scale(Boston))
dist_eu <- dist(boston_scaled)
boston_scaled$target<-kmeans(dist_eu, centers = 3)$cluster
lda.fit <- lda(target  ~ ., data = boston_scaled)
classes <- as.numeric(boston_scaled$target)
plot(lda.fit , dimen = 2, col=classes, pch=classes)
lda.arrows(lda.fit, myscale = 1)
lda.pred <- predict(lda.fit, newdata = boston_scaled)
a = table(correct = boston_scaled$target, predicted = lda.pred$class)
a
sum(a[1,1]+a[2,2]+a[3,3]) / sum(a)
