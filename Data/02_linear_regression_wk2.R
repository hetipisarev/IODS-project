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

### Data holds 7 variables and 166 rows.
# gender -- M (Male), F (Female)
# age -- measured in years
# points   Exam points
# attitude -- global attitude toward statistics, expressed as mean of questions Da-Dj (see the link below) 
# deep  -- deep learning score (mean score of questions D03-D31 (see the link below)) 
# stra -- strategic learning score (mean of questions ST*)
# surf -- surf-style learning score (mean of questions SU*)

# More exact desription of the data is on this link
https://www.mv.helsinki.fi/home/kvehkala/JYTmooc/JYTOPKYS3-meta.txt

### 2. Description of data

# Descriptive statistics of data
summary(learning2014)
table(learning2014$gender)

# I present here nice owerview graph.

library(GGally)
library(ggplot2)

# create a more advanced plot matrix with ggpairs()
p <- ggpairs(learning2014, mapping = aes(col = gender, alpha = 0.3), lower = list(combo = wrap("facethist", bins = 20)))
p

# From the graph we can see
# * we have 2 times more females
# * Ages are between 17 and 55 years, median age is 22 and 
# mean is 25 years, distribution has right tie. Median age 
# of males are little higher.
# * All score-variables  (attitude, deep, stra, surf, points)
# are continius variables, more or less bell-shaped.
# * Males have little higher attitude. Deep learning score 
# and points are similar among males and females. Median of
# startegic and surfing learning score is higher among females  
# * Top correlation are btw points and attitude (r=0.4), 
# surf and deep learning style (r=-0.3), surf-learning 
# style and attitud (-0.17).
# * Correlations are related witg gender - males have higher 
# correlation among surf and deep learning style (M: r=-0.6, F: r=-0.08)
# and surf learning style and attitude (M: r=-0.37, F: r=-0.01)

### 3. Regression model

# By last figure I use as descriptive variables attitude, 
# stra and surf. As these variables doesn't act very differently 
# among genders, I exclude gender.  
summary(lm(points ~ attitude + stra + surf , data=learning2014))

# From this output:
# * Attitude and stategic learning scores are positively related to
# output variable, higher surf-style learning score is related with 
# lower exam results. P-value (tests H0: that
# regression coefficient is equal to 0) is significant only on attitude.

# So I estimate new model btw points and attitude
m = lm(points ~ attitude , data=learning2014)
summary(m)

### 4. Summary of fitted model

# Now we have significant model between points and attitude.
# Average exam point score is 11.6 if attitude is 0, each 
# attitude point is related average 3.5 points higher exam result. 
# Anyway -- Adj R-square is 18.5, so the prognostic 
# value of this model is poor.
# On the figure below we can see the regression line with 
# 95% confidence interval, variability of data is quite high.

qplot(attitude, points, data = learning2014) + geom_smooth(method = "lm")

### 5. Diagnostic plots
par(mfrow = c(2,2), mai=c(0.6, 0.6, 0.4, 0.1), cex=0.7)
plot(m, which=c(1,2,5))

# Model residualas vs fitted values. Between fitted values 
# 24 and 27 are bunch of values with big residuals - 
# there are persons who have quite high attitude values, 
# but low exam point scores. Model gives them too high prognosis.  
# From Q-Q plot we see the same observations - they have higher 
# standardized residuls than expected.
# From last graph we can see that these persons have influence 
# to regression coefficient - they are outside of Cook's distance line.