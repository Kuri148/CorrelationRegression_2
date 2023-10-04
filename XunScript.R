setwd("/Users/xunyan/Desktop/r_practice/")

###install all required packages

install.packages("ggplot2") ##for data visualization
install.packages("ggpubr")
install.packages("PerformanceAnalytics")
install.packages("dplyr")

####now call all the packages

library(dplyr)
library(ggplot2)
library(ggpubr) #'ggpubr' provides some easy-to-use functions for creating and customizing 'ggplot2'- based publication ready plots
library(PerformanceAnalytics)

####load the data

intelligibility.data = read.csv(file = "intelligibility.data.csv", header = T)  #simple linear regression

fluency.data = read.csv(file = "fluency.data.csv", header = T) #multiple linear regression


###starting with simple linear regression

##check all assumptions

#To check whether the dependent variable follows a normal distribution, use the hist() function.

hist(intelligibility.data$intelligibility)

#You can also do a qq plot to check normality 

qqnorm(intelligibility.data$intelligibility)


#To check if the relationship between the independent and dependent variable is roughly linear. 

plot(intelligibility ~ pronunciation.accuracy, data = intelligibility.data)

#Let's run the regression model

intelligibility.lm = lm(intelligibility ~ pronunciation.accuracy, data = intelligibility.data)

summary(intelligibility.lm)

########################################################################################################
#We also need to check the constant variance assumption of the linear model. Our goal is to see the lack 
#of a relationship between residuals and the fitted values.
########################################################################################################

par(mfrow=c(2,2)) #this is to arrange all the graph output into a 2 by 2 matrix
plot(intelligibility.lm)
par(mfrow=c(1,1)) #this returns the graph organization to normal

########################################################################################################
#Leverage refers to the extent to which the coefficients in the regression model would change 
#if a particular observation was removed from the dataset.

#Standardized residuals refer to the standardized difference between a predicted value for an observation 
#and the actual value of the observation.

#Cook's D is calculated by removing the ith data point from the model and recalculating the regression. 
#It summarizes how much all the values in the regression model change when the ith observation is removed. 
########################################################################################################

#Let's visualize the regression line

intelligibility.graph = ggplot(intelligibility.data, aes(x=pronunciation.accuracy, y=intelligibility)) + 
  geom_point()

intelligibility.graph

#We can also add regression line

intelligibility.graph <- intelligibility.graph + geom_smooth(method="lm", col="black")

intelligibility.graph

#Add regression equation, if you want

intelligibility.graph <- intelligibility.graph +
  stat_regline_equation(label.x = 3, label.y = 7)

intelligibility.graph

#You can also make everything in one code

intelligibility.graph = ggplot(intelligibility.data, aes(x=pronunciation.accuracy, y=intelligibility)) + 
  geom_point() + 
  geom_smooth(method="lm", col="black") +
  stat_regline_equation(label.x = 3, label.y = 7)

intelligibility.graph


##Now that we are familiar with simple linear regression, let's venture into multiple linear regression

#check the first couple rows of your fluency data set using the head() function. It allows you to see your data without pulling out all the observations.
head(fluency.data)

#normality

hist(fluency.data$fluency.score)
qqnorm(fluency.data$fluency.score)

#Linearity: checking scatter plot for each independent variable

plot(fluency.score ~ pause.length, data=fluency.data)
plot(fluency.score ~ speech.duration, data=fluency.data)

# You can also check linearity by plotting all variables together

plot(fluency.data)

# We can also get the correlation matrix among all variables to check for multicolinearity (Week 4!)

cor(fluency.data, use = "pairwise.complete.obs")

# Or using the PerformanceAnalytics that we used in Week 4

chart.Correlation(fluency.data, histogram=TRUE, pch=19)

# Now let's run the model

fluency.data.lm = lm(fluency.score ~ pause.length + speech.duration, data = fluency.data)

summary(fluency.data.lm)

# Check homoscedasticity (constant variance) assumption

par(mfrow=c(2,2))
plot(fluency.data.lm)
par(mfrow=c(1,1))

#Since we have more than one variables, it is not always easy to visualize our data for multiple linear regression.



#######Stepwise regression#####################

#step(intercept-only model, direction, scope)

#where:

#intercept-only model: the formula for the intercept-only model
#direction: the mode of stepwise search, can be either "both", "backward", or "forward"
#scope: a formula that specifies which predictors we'd like to attempt to enter into the model

################################################
###Forward selection#####
#########################


#view first six rows of mtcars
head(mtcars)

####contengency tables

library(dplyr)

mtcars %>% 
  group_by(cyl) %>%
  summarize(
    sample.size = n(),mean.value = mean(mpg),standard.deviation = sd(mpg)
  )



mtcars %>%
  group_by(cyl, vs) %>%
  summarize(Freq=n())


mtcars %>%
  group_by(cyl) %>%
  summarize(Freq=n())


#define intercept-only model: null model
tom = lm(mpg ~ 1, data=mtcars)

#define model with all predictors
jerry = lm(mpg ~ ., data=mtcars)

#perform forward stepwise regression
forward = step(tom, direction='forward', scope=formula(jerry), trace=0)

#view results of forward stepwise regression
forward$anova

summary(forward)


################################################
###Backward elimination#####
############################

#define intercept-only model
tom <- lm(mpg ~ 1, data=mtcars)

#define model with all predictors
jerry <- lm(mpg ~ ., data=mtcars)

#perform backward stepwise regression
backward <- step(jerry, direction='backward', scope=formula(tom), trace=0)

#view results of backward stepwise regression
backward$anova

summary(backward)

################################################
###Stepwise regression#####
############################

#define intercept-only model
tom <- lm(mpg ~ 1, data=mtcars)

#define model with all predictors
jerry <- lm(mpg ~ ., data=mtcars)

#perform backward stepwise regression
both <- step(tom, direction='both', scope=formula(jerry), trace=0)

#view results of backward stepwise regression
both$anova

summary(both)