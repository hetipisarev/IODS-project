# Author: Heti Pisarev
# Date: 18.11.2020
# Comment: 
# Dimensionality reduction techniques

# Read data
human = read.csv(file = (paste0(getwd(), "/data/human.csv")), row.names=1)
dim(human)

# Today's libraries
library(ggplot2); library(GGally)
library(tidyr); library(dplyr)
library(corrplot)

library(MASS)
dplyr::select()


### 5.2.1 Overview of data

names(human)
str(human)
summary(human)

# visualize the human variables
ggpairs(human)


?ggpairs

# compute the correlation matrix and visualize it with corrplot
cor(human) %>% corrplot


### 5.2.2 PCA, not standardized data

pca_human <- prcomp(human)
summary(pca_human)
biplot(pca_human, choices = 1:2, cex=c(0.8, 1),col = c("grey40", "deeppink2"))


### 5.2.3 PCA, not standardized data
human_std <- scale(human)
pca_human_std <- prcomp(human_std)
summary(pca_human_std)
str(pca_human_std$x)

biplot(pca_human_std, choices = 1:2, cex=c(0.8, 1),col = c("grey70", "deeppink2"))
biplot(pca_human_std, choices = c(1,3), cex=c(0.8, 1),col = c("grey70", "deeppink2"))

### 5.2.4 Interpretation
### 5.2.5 Multiple Corresponde



# the tea dataset and packages 
library(FactoMineR)
data(tea)

# column names to keep in the dataset
keep_columns <- c("Tea", "How", "how", "sugar", "where", "lunch")

# select the 'keep_columns' to create a new dataset
tea_time <- select(tea, one_of(keep_columns))

# look at the summaries and structure of the data
str(tea_time)
summary(tea_time)

# visualize the dataset
gather(tea_time) %>% ggplot(aes(value)) + facet_wrap("key", scales = "free") + geom_bar() + theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8))

# multiple correspondence analysis
mca <-   MCA(tea_time, graph = TRUE)

# summary of the model
summary(mca)
mca$eig

plot(mca, , invisible=c("ind"), habillage = "quali")
plot(mca, invisible=c("var"))


