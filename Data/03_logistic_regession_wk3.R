# Author: Heti Pisarev
# Date: 11.11.2020
# Comment: 
# Logistic regression 
# Choose variables, describe them, do adjusted logistic regression model
# describe results
# estimate prognostic ability of the model


##------------------
## Analysis part
##------------------

library(dplyr); library(tidyr); library(ggplot2); library(Epi)

## ------------------
# 3.2.2 Read the data
## ------------------

alc = read.csv(file = "https://github.com/rsund/IODS-project/raw/master/data/alc.csv", sep=",") 
dim(alc)
glimpse(alc)

## ------------------
### 3.2.4 Selected variables vs high alcohol consumption
## ------------------

table(alc$high_use)

# Sex
pctab(table(alc$sex, alc$high_use))
chisq.test(alc$high_use, alc$sex)$p.value

# Study time
ggplot(data = alc, aes(x = as.factor(studytime), fill = high_use)) + 
  geom_bar(position = "fill") +
  scale_fill_manual(values = c("darkolivegreen", "gold")) + 
  scale_x_discrete(name = "Weekly study time (hours)", 
                  labels = c("<2", "2-5", "3/5-10" ,">10") ) +
  scale_y_continuous(name = "Persons frequency")
  
# Absences
par( mfrow=c(1,1), mai=c(0.6, 0.6, 0.4, 0.1), cex=0.7)
boxplot(alc$absences ~ alc$high_use, ylab="No of absences", xlab="High alcohol consumption")

# Reason to choose school
ggplot(data = alc, aes(x = reason, fill = high_use)) + 
  geom_bar(position = "fill") +
  scale_fill_manual(values = c("darkolivegreen", "gold")) + 
  scale_x_discrete(name = "Reason to choose school") +
  scale_y_continuous(name = "Persons frequency") 

## ------------------
## 3.2.5 The model
## ------------------

expcoef = function(model)           { 
  coeffs <- coef(summary(model)) 
  lci <- exp(coeffs[ ,1] - 1.96 * coeffs[ ,2]) 
  or <- exp(coeffs[ ,1]) 
  uci <- exp(coeffs[ ,1] + 1.96 * coeffs[ ,2]) 
  p<-round(coeffs[ ,4], 5)
  expcoef <- cbind("OR"=round(or,2), "lci"=round(lci,2), "uci"=round(uci,2), p)         
  expcoef 
}

m <- glm(high_use ~ relevel(as.factor(studytime), 3) + absences + 
           relevel(as.factor(reason), "reputation") +  sex, data = alc, family = "binomial")
expcoef(m)


## ------------------
# 3.2.6 Predictions
## ------------------

# add the predicted probabilities to 'alc'
alc <- mutate(alc, probability = predict(m, type = "response"))

# use the probabilities to make a prediction of high_use
alc <- mutate(alc, prediction = probability>0.5)

# tabulate the target variable versus the predictions>
confusion = function(ref) {
  table(high_use = alc$high_use, prediction = alc$probability>ref)}

confusion(0.5) 

# tabulate the target variable versus the predictions
confusion(0.5) %>% prop.table %>% addmargins

# percent of incorrect
sum(confusion(0.5)[1,2], confusion(0.5)[2,1])/sum(confusion(0.5))



# Simple testing:
confusion(0)
confusion(1)

## ------------------
# 3.2.7 10-fold cross-validation
# 3.2.8 Find the best model
## ------------------
