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
# 2. Answer the follow questions by performing relevant analysis. Write one 
# paragraph to answer each question, following the recommended format of reporting 
# statistical results for the methods you choose to use.
# 
# 2.1. Which variables have meaningful correlations with comprehensibility scores?
#   2.2. What is the best fitting model to predict comprehensibility score? 
# How good is this model (e.g., R2 or adjusted R2)? How should we interpret the model?
#   Feel free to read this article before you do your HW, to gain a better study context. 
# 
# At the end of the homework, make sure to show me your R codes. 

#Grand Total, Average, Standard Deviation

install.packages("ggplot2") ##for data visualization
install.packages("ggpubr")
install.packages("PerformanceAnalytics")
install.packages("dplyr")

library(dplyr)
library(ggplot2)
library(ggpubr) 
library(PerformanceAnalytics)
library(car)

Sys.setenv(LANG = "en")

MainData <- read.csv("D:\\R\\CorrelationRegression\\data\\HomeworkTwo.csv")

MainDataTransform <- read.csv("D:\\R\\CorrelationRegression\\data\\HomeworkTwo.csv")#Time to give nonlinear data a chance
MainDataTransform$Corrections <- log(MainDataTransform$Corrections + .00000001)#Maybe personality isn't everything...
plot(Comprehensibility ~ Corrections, data=MainDataTransform) # Yep I tried a second date, but things only got worse


RemovePerson <- MainData %>%
  select(Comprehensibility, Intelligibility, Syllables, SpeechRate, Corrections, 
         Repetitions, FilledPauses, EIT)

#--------------------------------------------------------Descriptive Statistics
colMeans(RemovePerson) 

stdev <- apply(RemovePerson, 2, sd)
stdev

medi <- apply(RemovePerson, 2, median)
medi
#--------------------------------------------------------------------Assumptions

# , , (3) independence of errors, , 
# and (5) independence of independent variables

# (4) normality
hist(MainData$Comprehensibility)
ggqqplot(MainData$Comprehensibility)
shapiro.test(MainData$Comprehensibility)


# (1) linearity in Independent Variables
plot(Comprehensibility ~ Intelligibility, data=MainData) #linear
plot(Comprehensibility ~ Syllables, data=MainData) # random
plot(Comprehensibility ~ SpeechRate, data=MainData) # linear
plot(Comprehensibility ~ Corrections, data=MainData) #not linear at all
plot(Comprehensibility ~ Repetitions, data=MainData) # not linear at all
plot(Comprehensibility ~ FilledPauses, data=MainData) #linear
plot(Comprehensibility ~ EIT, data=MainData) #linear
plot(RemovePerson) #What the heck do I do with this?

cor(RemovePerson, use = "pairwise.complete.obs")

compInt <- cor.test(RemovePerson$Comprehensibility, RemovePerson$Intelligibility)
compInt

compSpeechRate <- cor.test(RemovePerson$Comprehensibility, RemovePerson$SpeechRate)
compSpeechRate

compEIT <- cor.test(RemovePerson$Comprehensibility, RemovePerson$EIT)
compEIT

chart.Correlation(RemovePerson, histogram=TRUE, pch=19) #This is fucking awesome!
# Intelligibility, Speech Rate, EIT 

MultiModel.lm = lm(Comprehensibility ~ SpeechRate + EIT, data = RemovePerson)

  summary(MultiModel.lm)

(mfrow=c(2,2))
plot(MultiModel.lm)
par(mfrow=c(1,1))

#No relationship between the residuals and fitted.  Nice flat line


#define intercept-only model: null model
tomForward = lm(Comprehensibility ~ 1, data=RemovePerson)

#define model with all predictors
jerryForward = lm(Comprehensibility ~ ., data=RemovePerson)

#perform forward stepwise regression
forward = step(tomForward, direction='forward', scope=formula(jerryForward), trace=0)

#view results of forward stepwise regression
forward$anova

summary(forward)


################################################
###Backward elimination#####
############################

#define intercept-only model
bartBackward <- lm(Comprehensibility ~ 1, data=RemovePerson)

#define model with all predictors
lisaBackward <- lm(Comprehensibility ~ ., data=RemovePerson)

#perform backward stepwise regression
backward <- step(lisaBackward, direction='backward', scope=formula(tom), bartBackward=0)

#view results of backward stepwise regression
backward$anova

summary(backward)

################################################
###Stepwise regression#####
############################

#define intercept-only model
velmaBoth <- lm(Comprehensibility ~ 1, data=RemovePerson)

#define model with all predictors
daphneBoth <- lm(Comprehensibility ~ ., data=RemovePerson)

#perform backward stepwise regression
both <- step(velmaBoth, direction='both', scope=formula(daphneBoth), trace=0)

#view results of backward stepwise regression
both$anova

summary(both)

durbinWatsonTest(both)

