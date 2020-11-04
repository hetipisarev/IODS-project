# Author: Heti Pisarev
# Date: 2.11.2020
# Comment: This script reads data from web, 
# creates new variables deep, stra, surf,
# renames all headings to small letters
# saves data as csv to project's subfolder "data"
# reads data from the subfolder

# Data import
learning2014 = 
   read.table("http://www.helsinki.fi/~kvehkala/JYTmooc/JYTOPKYS3-data.txt", dec=".", sep="\t", header=T) 

# First overview
View(learning2014)
dim(learning2014)
str(learning2014)
head(learning2014, n=4)

# Create analysis dataset
# small letters: gender, age, points
# create deep, stra, surf, attitude
# exclude points==0
learning2014$age=learning2014$Age
learning2014$points=learning2014$Points

library(dplyr)  # attach library for _select_

learning2014$deep =
  rowMeans(
    select(learning2014, 
       one_of(
         c("D03", "D06", "D07", "D11", 
           "D14", "D15", "D19", "D22", 
           "D23", "D27", "D30", "D31")
         )))

learning2014$stra = 
  rowMeans(
    select(learning2014, 
           one_of(
             c("ST01", "ST04", "ST09", 
               "ST12", "ST17", "ST20",
               "ST25", "ST28")
          )))

learning2014$surf = 
  rowMeans(
    select(learning2014, 
           one_of(
             c("SU02", "SU05",  "SU08", 
               "SU10", "SU13", "SU16",  
               "SU18", "SU21", "SU24", 
               "SU26",  "SU29", "SU32") 
           )))
learning2014$attitude=rowMeans(select(learning2014, 
                               one_of(
                                c("Da","Db","Dc","Dd","De",
                               "Df","Dg","Dh","Di","Dj"))))

# keep selected columns and rows
learning2014 <- select(learning2014, 
      one_of(c("gender","age","attitude", 
               "deep", "stra", "surf", "points")))
learning2014 <- filter(learning2014, points>0)
dim(learning2014)   # [1] 166   7 

# Save the data

# change working directory
setwd("~/kursusIODS2020/IODS-project")

# save learning2014 to subfolder "data" as .csv file
write.csv(learning2014, file = (paste0(getwd(), "/data/learning2014.csv")), row.names=F)

# cleaning work
rm(list = ls())

# read data from .csv
learning2014 =read.csv(file = (paste0(getwd(), "/data/learning2014.csv"))) 

# data check
dim(learning2014)
str(learning2014)
head(learning2014, n=3)


