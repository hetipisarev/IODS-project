# Author: Heti Pisarev
# Date: 2.11.2020
# Comment: 
# This script reads data from local sub folder *data*, 
# desbribe data, make some descritive plot.
# Estimates regression model,
# describes and evaluates the model.


# change working directory
setwd("~/kursusIODS2020/IODS-project")

# read data from .csv
learning2014 =read.csv(file = (paste0(getwd(), "/data/learning2014.csv"))) 

### 1. Description of data structure
dim(learning2014)
head(learning2014, n=3)

### 2. Description of data

# Descriptive statistics of data
summary(learning2014)
table(learning2014$gender)

library(GGally)
library(ggplot2)

# create a more advanced plot matrix with ggpairs()
p <- ggpairs(learning2014, mapping = aes(col = gender, alpha = 0.3), lower = list(combo = wrap("facethist", bins = 20)))
p



### 3. Regression model

summary(lm(points ~ attitude + stra + surf , data=learning2014))

m = lm(points ~ attitude , data=learning2014)
summary(m)

### 4. Summary of fitted model

qplot(attitude,points, data = learning2014) + geom_smooth(method = "lm")

### 5. Diagnostic plots

par(mfrow = c(2,2), mai=c(0.6, 0.6, 0.4, 0.1), cex=0.7)
plot(m, which=c(1,2,5))

