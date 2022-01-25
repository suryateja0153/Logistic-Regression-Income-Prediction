#Author: Suryateja Chalapati

#Importing required libraries
rm(list=ls())
library(rio)
library(moments)
library(dplyr)
library(tidyverse)
library(magrittr)

#Setting the working directory and importing the dataset
setwd("C:/Users/surya/Downloads")

df = import("Income Data.xlsx", sheet = "Sheet1")
colnames(df)=tolower(make.names(colnames(df)))
str(df)

#Converting to factor variables and Re-levelling
cols <- c("sector", "education", "marital.status", "race", "gender", "bracket")

df %<>% mutate_at(cols, funs(factor(.)))
str(df)

df$sector <- relevel(df$sector, "Unemployed")
df$education <- relevel(df$education, "Primary School")
df$marital.status <- relevel(df$marital.status, "Never Married")
df$race <- relevel(df$race, "White")

#Setting seed and data sampling for equal distribution on brackets
set.seed(36991670)
data_sample = data.frame(df[sample(1:nrow(df), 1600, replace = FALSE),])
data_sample = df %>% group_by(bracket) %>% sample_n(800)
table(data_sample$bracket)
attach(data_sample)

#Analysis_1
log.out = glm(bracket~., data = data_sample, family = "binomial")
log.out = glm(bracket~.-index, data = data_sample, family = "binomial")

#Analysis_2
#Logistics Regression
summary(log.out)

#Analysis_3
#Null Deviance vs Residual Deviance

#Analysis_4

#Analysis_5
log.out1 = glm(bracket~age+education+marital.status+hours.per.week, data = data_sample, family = "binomial")
summary(log.out1)

#Analysis_6
pred.sample = expand.grid(age = quantile(data_sample$age, c(.25,.50,.75,1)),
                          education = unique(data_sample$education),
                          marital.status = unique(data_sample$marital.status),
                          hours.per.week = quantile(data_sample$hours.per.week, c(.25,.50,.75,1)))

pred.sample$pred.prob = predict(log.out1, newdata=pred.sample, type='response')
table(pred.sample$pred.prob)

pred.sample$pred.prob = round(pred.sample$pred.prob,5)
head(pred.sample,5)

#Analysis_7

#print(paste("Minimum value of predicted probability is ", min(pred.sample$pred.prob)))
#print(paste("Maximum value of predicted probability is ", max(pred.sample$pred.prob)))

max_row = pred.sample[pred.sample$pred.prob == max(pred.sample$pred.prob),]
max_row
min_row = pred.sample[pred.sample$pred.prob == min(pred.sample$pred.prob),]
min_row



