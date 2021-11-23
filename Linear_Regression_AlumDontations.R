#Installing essential Packages and Importing Data

library(DT)
library(ggplot2)
library(car)
library(MASS)
library(lmtest)
library(tseries)
library(ggfortify)
url <- "https://bgreenwell.github.io/uc-bana7052/data/alumni.csv"
alumni <- read.csv(url)
DT::datatable(alumni) 

#Exploratory Data Analysis
str(alumni)
head(alumni)
summary(alumni)

colnames (alumni)[1]<-"school" #Simplifying column name
alumni$school<-as.character(alumni$school)#Transforming factor into character
alumni$private<-as.factor(alumni$private)# Transforming integer variable into binary factor variable 

#Histograms and Boxplots to visualize shape and distribution of the data

hist(alumni$percent_of_classes_under_20,main="Percentage of classes offered with fewer than 20 students",freq = FALSE)
lines(density(alumni$percent_of_classes_under_20), lwd=5, col='blue')

hist(alumni$student_faculty_ratio,main="Student Faculty Ratio",freq = FALSE)
lines(density(alumni$student_faculty_ratio), lwd=5, col='blue')

hist(alumni$alumni_giving_rate,main="Percentage of alumni that made a donation to the university",freq = FALSE)
lines(density(alumni$alumni_giving_rate), lwd=5, col='blue')

plot(alumni$private)

boxplot(alumni$percent_of_classes_under_20,main="Percentage of classes offered with fewer than 20 students")
boxplot(alumni$student_faculty_ratio,main="Student Faculty Ratio")
boxplot(alumni$alumni_giving_rate,main="Percentage of alumni that made a donation to the university")

data<-alumni[,-1] #Removing Character Variable for effective numerical analysis

data$private<-as.numeric(data$private)#Retransforming for numerical analysis

#Correlation Analysis Matrix

library(corrplot)
res <- cor(data)
round(res,2)
cor.vis <- round(res,2)
corrplot(cor.vis, method = "number")
cor.vis
# improved correlation matrix
corrplot(cor(data),
         method = "number",
         type = "upper" # show only upper side
)

#Scatter Plot

p1 <- ggplot(data, aes(x =percent_of_classes_under_20 , y =alumni_giving_rate)) +
  geom_point(alpha = 2/3) 
p1

p2 <- ggplot(data, aes(x =student_faculty_ratio , y =alumni_giving_rate)) +
  geom_point(alpha = 2/3) 
p2

p3 <- ggplot(data, aes(x =private , y =alumni_giving_rate)) +
  geom_point(alpha = 2/3) 
p3


model_1 <- lm(alumni_giving_rate~percent_of_classes_under_20,student_faculty_ratio ,private,data=data)
summary(model_1)
residualPlot(model_1)
qqPlot(model_1)
yhat <- fitted(model_1)
plot(residuals(model_1))
plot(rstandard(model_1))  
plot(rstudent(model_1))  
plot(rstandard(model_1, type = "predict"))
autoplot(model_1)

y=data$alumni_giving_rate
x=data$percent_of_classes_under_20

model <- lm(y ~ x)
newx = seq(min(x),max(x),by = 0.05)
conf_interval <- predict(model, newdata=data.frame(x=newx), interval="confidence",
                         level = 0.95)
plot(x, y, xlab="percent_of_classes_under_20", ylab="alumni_giving_rate", main="Regression")
abline(model, col="lightblue")
lines(newx, conf_interval[,2], col="blue", lty=2)
lines(newx, conf_interval[,3], col="blue", lty=2)

model_2 <- lm(alumni_giving_rate~student_faculty_ratio ,data=data)
summary(model_2)
residualPlot(model_2)
qqPlot(model_2)
yhat <- fitted(model_2)
plot(residuals(model_2))
plot(rstandard(model_2))  
plot(rstudent(model_2))  
plot(rstandard(model_2, type = "predict"))
autoplot(model_2)

y=data$alumni_giving_rate
x=data$student_faculty_ratio

model <- lm(y ~ x)
newx = seq(min(x),max(x),by = 0.05)
conf_interval <- predict(model, newdata=data.frame(x=newx), interval="confidence",
                         level = 0.95)
plot(x, y, xlab="percent_of_classes_under_20", ylab="alumni_giving_rate", main="Regression")
abline(model, col="lightblue")
lines(newx, conf_interval[,2], col="blue", lty=2)
lines(newx, conf_interval[,3], col="blue", lty=2)

model_3 <- lm(alumni_giving_rate~private ,data=data)
summary(model_3)
residualPlot(model_3)
qqPlot(model_3)
yhat <- fitted(model_3)
plot(residuals(model_3))
plot(rstandard(model_3))  
plot(rstudent(model_3))  
plot(rstandard(model_3, type = "predict"))
autoplot(model_3)

y=data$alumni_giving_rate
x=data$private

model <- lm(y ~ x)
newx = seq(min(x),max(x),by = 0.05)
conf_interval <- predict(model, newdata=data.frame(x=newx), interval="confidence",
                         level = 0.95)
plot(x, y, xlab="percent_of_classes_under_20", ylab="alumni_giving_rate", main="Regression")
abline(model, col="lightblue")
lines(newx, conf_interval[,2], col="blue", lty=2)
lines(newx, conf_interval[,3], col="blue", lty=2)

#Essentially all three variables are capturing the same piece of information, explains high correlation as well


