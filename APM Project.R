library(caret)
library(tidyverse)
library(rms)
library(e1071)
#1. I chose a logistic regression.  The dependent variable is bivariate, meaning any output represents the percent
#   chance an observation has of being the true, in this case, having heart disease.

#-------------------------------------------------------------------

#2. Separate the data into training and test sets

HDdata <- read_csv("~/HDdata.csv")

# Filtering out NAs, which represent 3% of total observations.
HDdata %>%
  filter(!is.na(age)) -> HDdata

#Breaking out Categorical variables into Binary, Dummy Variables
#Baselines set as cp (asymptomatic) and restecg (normal)
HDdata %>%
  mutate(typ=cp==1) %>%
  mutate(atyp=cp==2) %>%
  mutate(nonang=cp==3) %>%
  mutate(abnormecg=restecg==1) %>%
  mutate(hypertrophy=restecg==2) %>%
  select(-cp,-restecg)-> HDdata2

#Converting logical columns to numeric dummy variables

cols <- sapply(HDdata2, is.logical)
HDdata2[,cols] <- lapply(HDdata2[,cols], as.numeric) 

## 75% of the sample size
smp_size <- floor(0.75 * nrow(HDdata2))

## set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(HDdata2)), size = smp_size)

train <- HDdata2[train_ind, ]
test <- HDdata2[-train_ind, ]

#-------------------------------------------------------------------

# In the data, are there:
  #NAs?:  Yes, there are several NAs in the age column. They only represent 3% of data, so were removed.
  
  #Categorical Variables?: Yes, there are two: cp, restecg.  
  #These need to be split into binary variables for the categories.
  #Because there needs to be a baseline for dummy variables, I chose aymptomatic cp and a normal restecg as the baseline.  Dummy variables measure any abnormalities.

  #Binary Variables?: Yes, sex, fbs, exang, num

  #Numeric?: Yes, age, trestbps, chol, thalach, oldpeak

#-------------------------------------------------------------------

#The Model
train$num<-factor(train$num)
lm<-glm(num~.,data=train,family=binomial)

#threshold
th<-0

#Creates predictions
test$predict<-predict(lm,test)
test$predict<-ifelse(test$predict>th,1,0)
test$predict<-factor(test$predict)
test$num<-factor(test$num)
confusionMatrix(data=test$num,
                reference = test$predict)

#Accuracy:  79.73%
#PPV:       79.49%
#NPV:       80.00%
#Sensitivity: .8158
#Specificity: .7778

