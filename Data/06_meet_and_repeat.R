# Author: Heti Pisarev
# Date: 05.12.2020
# Comment: 
# Read the datasets and reshape them long format

# Read datasets, see the dimentions and structure 

library(tidyr)
library(dplyr)

# 1. Read the data and give brief summary
# BPRS
BPRS <- read.csv("https://raw.githubusercontent.com/KimmoVehkalahti/MABS/master/Examples/data/BPRS.txt", sep  =" ", header = T)
# dimentions and structure
dim(BPRS)
str(BPRS)
names(BPRS)
summary(BPRS)
# frequencies of treatment
table(BPRS$treatment)

attach(BPRS)
par(mfrow=c(2,1), mai=c(0.6,0.5,0.05,0.05), cex=0.7)
boxplot(BPRS[treatment==1,3:11], ylim=c(20,100), las=1)
boxplot(BPRS[treatment==2,3:11], ylim=c(20,100), las=1)
detach(BPRS)

rats <- read.csv("https://raw.githubusercontent.com/KimmoVehkalahti/MABS/master/Examples/data/rats.txt", sep  ="\t", header = T)
dim(rats)
names(rats)
str(rats)
summary(rats)
# frequencies of treatment
table(rats$Group)

attach(rats)
par(mfrow=c(3,1), mai=c(0.6,0.5,0.05,0.05), cex=0.7)
boxplot(rats[Group==1,3:13])
boxplot(rats[Group==2,3:13])
boxplot(rats[Group==3,3:13])
detach(rats)

# 2. Replace subject indentificator and group variable to factor

BPRS$treatment <- factor(BPRS$treatment)
BPRS$subject <- factor(BPRS$subject)
str(BPRS)

rats$Group <- factor(rats$Group)
rats$ID <- factor(rats$ID)
str(rats)

#3. Convert to long form, extract time variable as numeric 
BPRSL <-  BPRS %>% gather(key = weeks, value = bprs, -treatment, -subject)
head(BPRSL, n=3)
str(BPRSL)
# Extract the week number
BPRSL <-  BPRSL %>% mutate(week = as.integer(substr(BPRSL$weeks, 5, 5)))


ratsl <-  rats %>% gather(key = WD, value = weight, -Group, -ID)
str(ratsl)
# Extract the week number
ratsl <-  ratsl %>% mutate(Time = as.integer(substr(ratsl$WD, 3, 5)))
#Check results
table(ratsl$WD, ratsl$Time)

#4. Overview
dim(rats)
length(table(rats$ID))  # 16 subjects
## 11 timepoints measures

11*16
dim(ratsl) # 176 rows  
names(ratsl)
length(table(ratsl$ID))  # still 16 subjects
boxplot(ratsl$weight ~ ratsl$Time + ratsl$Group)


dim(BPRS)
str(BPRS)
length(table(BPRS$subject))  # 2x200 subjects
11-2 ## 9 timepoints measures

9*40
dim(BPRSL) # 360 rows  
names(BPRSL)
length(table(BPRSL$subject))  # still 2x20 subjects
boxplot(BPRSL$bprs ~ BPRSL$week + BPRSL$treatment)


write.csv(ratsl, file = (paste0(getwd(), "/data/ratsl.csv")), row.names=T)
write.csv(BPRSL, file = (paste0(getwd(), "/data/bprsl.csv")), row.names=T)
