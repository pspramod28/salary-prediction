
library(dplyr)
library(tidyverse)
library(caret)
library(caTools)
library(car)
theme_set(theme_bw())

getwd()
##Importing data to the R
sal <- read.csv("salary.csv")
View(sal)

str(sal)
cor(sal[-1])

sal$Edu <- as.character(sal$Edu)
sal$Mngt <- as.character(sal$Mngt)

##Missing value analysis##
sum(is.na(sal=="?"))
sum(is.na(sal))
sapply(sal,function(x)sum(is.na(x)))

##Removing missing value observation
sal <- sal %>% na.omit()	
dim(sal)

##Finding the duplicates
sum(duplicated(sal))

##split the data into train and test data. 
library(caTools)
set.seed(123)
sal_sam <- sample.split(sal$Salary, SplitRatio = 0.7)
sal_train <- subset(sal, sal_sam==TRUE)
sal_test <- subset(sal, sal_sam==FALSE)

fit1 <- lm(Salary~Exp+Edu+Mngt, data = sal_train)
summary(fit1)

##Plotting data to check outlayers and otherthings
plot(fit1)

##To check is there any MULTICOLLINEARITY in data
vif(fit1)
##Since VIF values <5 no multicollinearity exists

fit2 <- lm(Salary~Exp+Edu+Mngt+(Edu*Mngt), data = sal_train)
summary(fit2)

##Plotting data to check outlayers and otherthings
plot(fit2)

##To check is there any MULTICOLLINEARITY in data
vif(fit2)

#removing outlayer of row 33
sal1 <- sal_train[c(-33),]
fit3 <- lm(Salary~Exp+Edu+Mngt+(Edu*Mngt), data = sal1)
fit3 <- lm(Salary~Exp+Edu+Mngt+(Edu*Mngt), data = sal[-33,])

summary(fit3)

##Plotting data to check outlayers and otherthings
plot(fit3)

##To check is there any MULTICOLLINEARITY in data
vif(fit3)

##Shows the relationship in graphical representation 
ggplot(sal1, aes(x = Exp, y = Salary)) +
  geom_point() +
  stat_smooth()

##Applying model to test dataset
predictions <- fit3 %>% predict(sal_test)
RMSE(predictions, sal_test$Salary)##177.99/mean(sal_train$Salary)*100=1.048679 only 1% error in model or output
R2(predictions, sal_test$Salary)##0.9982745(it has 99.9% accuracy) 

##Applying to new data(to predict the salary)
New_emp <- read.csv("New_emp.csv")
str(New_emp)

New_emp$Edu <- as.character(New_emp$Edu)
New_emp$Mngt <- as.character(New_emp$Mngt)
New_emp$Name <- as.character(New_emp$Name)
New_emp$Predited_sal <- predict(fit3,New_emp)


a <- summary(fit1)
b <- summary(fit3)

