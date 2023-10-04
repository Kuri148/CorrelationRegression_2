# In doing so, it allows us to perform regression analysis. Since we have 
# learned correlation and regression by now, I would like you to work with this
# data set to find out (1) what variables are associated with comprehensibility
# scores, and (2) what is the best regression model to predict comprehensibility.
# 
# In this assignment tab, you will see the explanations of the variables in the data set.
# 
# Your tasks are:
#   
#   1. perform both descriptive statistics on the independent and dependent 
# variables. Please create tables to show your analysis results.
# 
# 2. Anwer the follow questions by performing relevant analysis. Write one 
# paragraph to answer each question, following the recommended format of reporting 
# statistical results for the methods you choose to use.
# 
# 2.1. Which variables have meaningful correlations with comprehensibility scores?
#   2.2. What is the best fitting model to predict comprehensibility score? 
# How good is this model (e.g., R2 or adjusted R2)? How should we interpret the model?
#   Feel free to read this article before you do your HW, to gain a better study context. 
# 
# At the end of the homework, make sure to show me your R codes. 


install.packages("ggplot2") ##for data visualization
install.packages("ggpubr")
install.packages("PerformanceAnalytics")
install.packages("dplyr")

library(dplyr)
library(ggplot2)
library(ggpubr) 
library(PerformanceAnalytics)

Sys.setenv(LANG = "en")

MainData <- read.csv("D:\\R\\CorrelationRegression\\HomeworkTwo\\data\\HomeworkTwo.csv")


RemovePerson <- MainData %>%
  select(Comprehensibility, Intelligibility, Syllables, SpeechRate, Corrections, 
         Repetitions, FilledPauses, EIT)

#--------------------------------------------------------------------Assumptions

# , (2) homoskedasticity, (3) independence of errors, , 
# and (5) independence of independent variables

# (4) normality
hist(MainData$Comprehensibility)
qqnorm(MainData$Comprehensibility)

#Not very normal, but Regression is strong against this.

# (1) linearity
plot(Comprehensibility ~ Intelligibility, data=MainData) #linear
plot(Comprehensibility ~ Syllables, data=MainData) # random
plot(Comprehensibility ~ SpeechRate, data=MainData) # linear
plot(Comprehensibility ~ Corrections, data=MainData) #not linear at all
plot(Comprehensibility ~ Repetitions, data=MainData) # not linear at all
plot(Comprehensibility ~ FilledPauses, data=MainData) #linear
plot(Comprehensibility ~ EIT, data=MainData) #linear
plot(RemovePerson) #What the heck do I do with this?

cor(RemovePerson, use = "pairwise.complete.obs")

chart.Correlation(RemovePerson, histogram=TRUE, pch=19) #This is fucking awesome!
# Intelligibility, Speech Rate, EIT 

MultiModel.lm = lm(Comprehensibility ~ SpeechRate + EIT, data = RemovePerson)

  summary(MultiModel.lm)

(mfrow=c(2,2))
plot(MultiModel.lm)
par(mfrow=c(1,1))

#No relationship between the residuals and fitted