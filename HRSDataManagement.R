
###
# File name:     HRS16&18_training_test.R
# Purpose:       Create test and training data for predicting the future number 
#                  of call attempts to finalization
# Programmer:    Xinyu Zhang
# Date:          2/17/2023
###

library(haven)
library(tidyverse)

# Raw data: 
#  call recall data: hrs16long & hrs18long
#  HRS data:         hrs14 and hrs16 

# This R program shows an example of the cutoff point set at call attempt 3. 
cut <- 3

###
# Continuous time approach
###

## training set 

# read data
# 2016 HRS call record data (at the call attempt level)
hrs16long <- read_sas(".path/longcallsurv16.sas7bdat")
names(hrs16long) <- tolower(names(hrs16long)) 
# 2014 HRS data (at the case level)
hrs14 <- read_sas(".path/callsurv14.sas7bdat")
names(hrs14) <- tolower(names(hrs14)) 

# Identify all eligible cases (e.g., cases that were contacted at least 4 times)
hrs16long2 <- subset(hrs16long, ncallinfoid == cut+1)
train <- merge(hrs16long2, hrs14, by = c("vsamplelineid"))

## test set 

# read data
# 2018 HRS call record data (at the call attempt level)
hrs18long <- read_sas(".path/longcallsurv18.sas7bdat")
names(hrs18long) <- tolower(names(hrs18long)) 
# 2016 HRS data (at the case level)
hrs16 <- read_sas(".path/callsurv16.sas7bdat")
names(hrs16) <- tolower(names(hrs16)) 

# Identify all eligible cases (e.g., cases that were contacted at least 4 times)
hrs18long2 <- subset(hrs18long, ncallinfoid == cut+1)
test <- merge(hrs18long2, hrs16, by = c("vsamplelineid"))

save(train, file="hrs16_tel_call3.RData")
save(test, file="hrs18_tel_call3.RData")

###
# Discrete time approach
###

# read data 
# 2016 HRS call record data (at the call attempt level)
hrs16long <- read_sas(".path/longcallsurv16.sas7bdat")
names(hrs16long) <- tolower(names(hrs16long)) 
# 2014 HRS data (at the case level)
hrs14 <- read_sas(".path/callsurv14.sas7bdat")
names(hrs14) <- tolower(names(hrs14)) 
hrs16longa <- merge(hrs16long, hrs14, by = c("vsamplelineid"))

# read data
# 2018 HRS call record data (at the call attempt level)
hrs18long <- read_sas(".path/longcallsurv18.sas7bdat")
names(hrs18long) <- tolower(names(hrs18long)) 
# 2016 HRS data (at the case level)
hrs16 <- read_sas(".path/callsurv16.sas7bdat")
names(hrs16) <- tolower(names(hrs16)) 
hrs18longa <- merge(hrs18long, hrs16, by = c("vsamplelineid"))

# training and test sets
hrs18longb <- subset(hrs18longa, ncallinfoid < cut + 1)
traina <- rbind(train, train2)
testa <- subset(hrs18longa, ncallinfoid > cut)

save(traina, file="hrs16_tel_long_call3.RData")
save(testa, file="hrs18_tel_long_call3.RData")
