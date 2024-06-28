############################################
#### File name:   call_attempts_time_to_event.R
#### Purpose:     Predict the number of call attempts required until an interview or refusal after some call attempts
#### Programmer:  Xinyu Zhang
#### Date:        09/04/2023
############################################

library(survival)
library(rpart) 
library(partykit) 
library(BART)
library(dplyr)

# This R program shows an example of the cutoff point set at call attempt 3. 
# The cutoff point can be modified based on researcher's need.
cut = 3

#########################
# 1. Discrete time approach
#########################
# Data requirement: long format (each row represents one call attempt per case)
# train: data from the 2016 HRS
# test: data from the 2018 HRS

#########################
# 1.1. Discrete Time Hazard Regression
#########################
# Add incoming data from the 2018 HRS into the 2016 HRS
train2 <- subset(test, ncallinfoid < cut + 1)
train3 <- rbind(train, train2)
# discrete time-to-event model
model_comp <- glm(status ~ age16 + female + raceeth + degree5 + curworkpay + reginternetuse + medicaidcov + functionlmt + bselfratedhealth + bdiabetes + bimpairlmtwrk + bownhome + lastrefusal + lastni + ncallinfoid, family = binomial(link = "cloglog"), data = train3)
###
# make predictions for all unresolved cases in the 2018 HRS after 3 call attempts
###
# unresolved cases
hrs182 <- subset(test, ncallinfoid == cut + 1) 
numcase <- nrow(hrs182)
# create 200 rows for each unresolved case; number of rows is subject to change 
hrs183<- hrs182[rep(seq(numcase), each = 200),]
# create 200 future call attempts for each unresolved case
hrs183$ncallinfoid <- rep(seq(cut+1,cut + 200,1), numcase)
hrs183$pcomp <- predict(model_comp, hrs183, type = "response")
hrs183$pfnl <- 1-(1-hrs183$pcomp)
hrs184 <- hrs183 %>% 
  group_by(vsamplelineid) %>% 
  mutate(pfnl2 = 1- lag(pfnl, order_by = vsamplelineid))
# recode NA into 1 (the survival probability for the next call attempt)
hrs184$pfnl2[is.na(hrs184$pfnl2)] <- 1
hrs184 <- hrs184 %>%  
  arrange(vsamplelineid) %>% 
  group_by(vsamplelineid) %>% 
mutate(psurvprob = cumprod(pfnl2))
# obtain all future call attempt numbers that pass a cumulative probability of 0.5
hrs185 <- subset(hrs184, psurvprob >= 0.5)
# obtain the first future call attempt number that passes a cumulative probability of 0.5
hrs186 <- hrs185 %>% 
  group_by(vsamplelineid) %>% 
   filter(row_number() == n())
hrs186$ncallinfoid

#########################
# 2. Continuous time approach
#########################
# Data requirement: wide format (each row represents one case); e.g., transform the call record data from the long format to the wide format
# train: data from the 2016 HRS
# test: data from the 2018 HRS
# model specification for a time-to-event model
mf <- Surv(ncallinfoid,status) ~ (age16 + female + raceeth + degree5 + curworkpay + reginternetuse + medicaidcov + functionlmt + bselfratedhealth + bdiabetes + bimpairlmtwrk + bownhome + lastrefusal + lastni + pcalloutcome2)

#########################
# 2.1. LN-AFT
#########################
aftmod1 <- survreg(mf, train, dist="lognormal") 
pred.aft <- predict(aftmod1, test, type="response") 

#########################
# 2.2. Survival Tree
#########################
tfit <- rpart(mf, data = train, control = rpart.control(minsplit=250, cp = 0.001)) 
tfit2 <- as.party(tfit)
pred.rpart <- predict(tfit2, newdata = test, type = "response")

#########################
# 2.3. LN-AFT-BART
#########################
ncallinfoid <- train$ncallinfoid
status <- train$status
bartaft <- abart(x.train = train, times = ncallinfoid, delta = status, ntree=300, x.test = test, k = 3, sigdf = 9, sigquant = 0.85, nskip=500, printevery=100, keepevery=1, ndpost=1000)
bartaft$yhat.test
