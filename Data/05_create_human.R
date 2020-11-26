# Author: Heti Pisarev
# Date: 26.11.2020
# Comment: 
# Continue data wrangling from last week
# Change non-numeric data to numeric data
# Remove rows with NA-s
# Keep only country-based data
# Keep selected variables


library(stringr)
library(dplyr)
library(tidyr)

### 5.1.0 Read the data
human <- read.csv("http://s3.amazonaws.com/assets.datacamp.com/production/course_2218/datasets/human1.txt", stringsAsFactors = F)

str(human)
summary(human)
dim(human)

### 5.1.1 Mutate data

# Change 12,000-style variable to numeric 12000-style
human = mutate(human, GNI=as.numeric(str_replace(human$GNI, pattern=",", replace ="")))
summary(human$GNI)


### 5.1.2 Keep selected variables

# Columns to keep
keep <- c("Country", "Edu2.FM", "Labo.FM", "Edu.Exp", "Life.Exp", "GNI", "Mat.Mor", "Ado.Birth", "Parli.F")
human <- select(human, one_of(keep))

names(human)
dim(human)


### 5.1.3 Remove all rows with missing values

human <- filter(human, complete.cases(human))
dim(human)


### 5.1.4 Remove all rows with region data

# last rows on human data
tail(human,n=10)

# choose everything until the last 7 observations
human <- human[1:(nrow(human) - 7), ]

dim(human)

### 5.1.5 Adapt row names from variable country  and save the data

rownames(human) <- human$Country
human <- select(human, -Country)

dim(human)

write.csv(human, file = (paste0(getwd(), "/data/human.csv")), row.names=T)


