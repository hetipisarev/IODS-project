# Author: Heti Pisarev
# Date: 11.11.2020
# Comment: 
# Data source: https://archive.ics.uci.edu/ml/datasets/Student+Performance
# Merge downloaded datasets and keep students existing both datasets
# define high alcohol consumption 

##----------------
## Data wrangling
##----------------


# libraries
library(dplyr)


# Change working directory
setwd("~/kursusIODS2020/IODS-project")


# Read data from .csv
math = read.csv(file = (paste0(getwd(), "/Data/student-mat.csv")), sep=";") 
por = read.csv(file = (paste0(getwd(), "/Data/student-por.csv")), sep=";") 

dim(math)
dim(por)
str(math)
str(por)


# Merge datasets 

# common columns to use as identifiers
join_by <- c("school","sex","age","address","famsize",
             "Pstatus","Medu","Fedu","Mjob","Fjob",
             "reason","nursery","internet")

# join the two datasets by the selected identifiers
math_por <- inner_join(math, por, by = join_by, 
                       suffix = c(".math", ".por"))
# check new datased
dim(math_por)
glimpse(math_por)
#----------------


# Next part will combine two possibly different 
# answers to the same questions for each student. 
# To fix this, you'll use programming to combine these
# 'duplicated' answers by taking mean/first answer

# create a new data frame with only the joined columns
alc <- select(math_por, one_of(join_by))

# the columns in the datasets which were not used for joining the data
notjoined_columns <- colnames(math)[!colnames(math) %in% join_by]

# print out the columns not used for joining
notjoined_columns

# for every column name not used for joining...
for(column_name in notjoined_columns) {
  # select two columns from 'math_por' with the same original name
  two_columns <- select(math_por, starts_with(column_name))
  # select the first column vector of those two columns
  first_column <- select(two_columns, 1)[[1]]
  
  # if that first column vector is numeric...
  if(is.numeric(first_column)) {
    # take a rounded average of each row of the two columns and
    # add the resulting vector to the alc data frame
    alc[column_name] <- round(rowMeans(two_columns))
  } else { # else if it's not numeric...
    # add the first column vector to the alc data frame
    alc[column_name] <- first_column
  }
}

# glimpse at the new combined data
glimpse(alc)
#----------------


# Create ew columns - 
# alc_use as average of weekday and weekend alcohol consumption
# high_use - logical if average consuption is more than 2 units

alc <- mutate(alc, alc_use = (Dalc + Walc) / 2)
alc <- mutate(alc, high_use = alc_use > 2)

dim(alc)
# [1] 382  35
str(alc)

#----------------

# Save data to local folder as .csv
write.csv(alc, file = (paste0(getwd(), "/data/alc.csv")), row.names=F)
