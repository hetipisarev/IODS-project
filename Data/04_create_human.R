# Author: Heti Pisarev
# Date: 18.11.2020
# Comment: 
# Read and join the “Human development” and “Gender inequality” 
# create new dataset ´human´


### 4.1.2 Read the data

hd <- read.csv("http://s3.amazonaws.com/assets.datacamp.com/production/course_2218/datasets/human_development.csv", stringsAsFactors = F)
ii <- read.csv("http://s3.amazonaws.com/assets.datacamp.com/production/course_2218/datasets/gender_inequality.csv", stringsAsFactors = F, na.strings = "..")

### 4.1.3 Explore the datasets

dim(hd)
dim(ii)

names(hd)
names(ii)

summary(hd)
summary(ii)

### 4.1.4 Rename variables
names(hd) = c("hdi.rank", "country", "hdi", "life.exp.birth",
              "exp.educ", "mean.educ", "gni", "gni.minus.hdi")

names(ii) = c("gii.rank", "country", "gii", "maternal.mort.ratio",
              "adoles.birth.rate", "parliament" ,"sec.edu.f",
              "sec.edu.m", "labour.rate.f", "labour.rate.m"  )
  
###  4.1.5 Add variables
# Add to “Gender inequality” ratio of Female and Male populations 
# * secondary education in each country.
# * labour force participation

ii = mutate(ii, edu.ratio = sec.edu.f/sec.edu.m)
ii = mutate(ii, lab.ratio = labour.rate.f/labour.rate.m)

head(ii, n=3)

### 4.1.6 Merge datasets
human <- inner_join(hd, ii, by = "country")

names(human)
dim(human)

write.csv(human, file = (paste0(getwd(), "/data/human.csv")), row.names=F)

