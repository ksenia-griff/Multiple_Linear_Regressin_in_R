
#Note:  in R \ is an escape key.  C:\Users\Rodolfo\Documents\UofT\marioKart.csv will not work
#alternatively you could have used \\, the first \ escapes the second \
#we are using a "." to sepparate a file name from a data frame
#read is a built in function
#Please change the file path in the command below to coincide with where you have stored the data files.
#In this case the notebook and the data are in the same folder

CreditRisk <- read.csv("C:/Users/Rodolfo/Documents/UofT/CreditRisk.csv", header=TRUE)
attach(CreditRisk)

names(CreditRisk)

#Ottawa and Vancouver are dummy variables of the field City
str(CreditRisk)

pairs(CreditRisk)

#We can combine scatter plots and histograms in one graph
#install.packages("car")
library(car)
scatterplotMatrix(CreditRisk[,1:7],
  diagonal="histogram",
  smooth=FALSE)

#We can combine scatter plots and histograms in one graph and plot each city
#with a different colour
scatterplotMatrix(CreditRisk[,2:7],
  diagonal="histogram",
  smooth=FALSE,groups=City)


#We can combine scatter plots and histograms in one graph and plot each city
#with a different colour

scatterplotMatrix(~CC_Payments+Wage+Cost_Living+Mtg+Vacations|City, data=CreditRisk,
   main="Matrix Scatterplot")


library(Hmisc)
tempdata <- CreditRisk [, c(2,3,4,5,7)]  #I want to build correlations for continuous variables
rcorr(as.matrix(tempdata))
#notice colinearity
#you can run both correlation and p values simultaneously using the Hmisc package

par(mfrow=c(2,3))
boxplot(Wage~City, main = "Boxplot: Wage by City")
boxplot(Mtg~City, main = "Boxplot: Mortgage by City")
boxplot(Cost_Living~City, main = "Boxplot: Cost of Living by City")
boxplot(Vacations~City, main = "Boxplot: Vacations by City")
boxplot(CC_Payments~City, main = "Boxplot: CC Payments by City")

m1 <- lm(CC_Payments~City+Wage+Cost_Living+Mtg+Vacations)
summary(m1)
StanRes1 <- rstandard(m1)
par(mfrow=c(2,3))
plot(City,StanRes1, ylab="Standardized Residuals")
plot(Wage,StanRes1, ylab="Standardized Residuals")
plot(Cost_Living,StanRes1, ylab="Standardized Residuals")
plot(Mtg,StanRes1, ylab="Standardized Residuals")
plot(Vacations,StanRes1, ylab="Standardized Residuals")
#the full model reflects well how the data was generated

#Plot predicted against fitted values
par(mfrow=c(1,1))
plot(m1$fitted.values,CC_Payments,xlab="Fitted Values:  CC_Payments", ylab="Original Data: CC_Payments")
abline(lsfit(m1$fitted.values,CC_Payments))

#stepwise backwards
 #trace = 0 mean that you are not tracing the steps
 m1_b<- step(m1, direction = "backward", trace = 0)
 summary(m1_b)
#the selected parameters reflect how the data was generated

# a forward regression starts with no predictors
 # let's start with the average price only, no independent variable
 m1_0f = lm(CC_Payments ~1, data = CreditRisk)
 summary(m1_0f)

m1_f = step(m1_0f, direction = "forward", 
                      scope = (~ City+Wage+Cost_Living+Mtg+Vacations), 
                
                            data = CreditRisk, 
                      trace = 0)
 summary(m1_f)
#again the model reflects how the data was generated, however notice that the parameters are 
#slightly different than in the previous calculations

#model selection by exact AIC
m1_f = step(lm(CC_Payments ~ 1, data=CreditRisk), direction='forward', 
                  scope=~ City+Wage+Cost_Living+Mtg+Vacations)

summary(m1_f)

#However looking at city in isolation, it gives p-value <0.05 and R2 = 0.762. - collinearity
#strange things happen when there is collinearity
m2 <- lm(CC_Payments~City)
summary(m2)

m3 <- lm(CC_Payments~City+Mtg)
summary(m3)
#City is now significant

m4 <- lm(CC_Payments~City+Mtg+Cost_Living)
summary(m4)

m5 <- lm(CC_Payments~Wage+City)
summary(m5)
#notice how city is significant and not wage

m6 <- lm(CC_Payments~Wage+City+Mtg)
summary(m6)

m7 <- lm(CC_Payments~Wage+City+Mtg+Cost_Living)
summary(m7)

#Compare Models
#Plot predicted against fitted values, there is no indication of misfit in the charts
par(mfrow=c(3,3))
plot(m1$fitted.values,CC_Payments,xlab="m1 Fitted Values:  CC_Payments", ylab="Original Data: CC_Payments")
abline(lsfit(m1$fitted.values,CC_Payments))

plot(m2$fitted.values,CC_Payments,xlab="m2 Fitted Values:  CC_Payments", ylab="Original Data: CC_Payments")
abline(lsfit(m2$fitted.values,CC_Payments))

plot(m3$fitted.values,CC_Payments,xlab="m3 Fitted Values:  CC_Payments", ylab="Original Data: CC_Payments")
abline(lsfit(m3$fitted.values,CC_Payments))

plot(m4$fitted.values,CC_Payments,xlab="m4 Fitted Values:  CC_Payments", ylab="Original Data: CC_Payments")
abline(lsfit(m4$fitted.values,CC_Payments))

plot(m5$fitted.values,CC_Payments,xlab="m5 Fitted Values:  CC_Payments", ylab="Original Data: CC_Payments")
abline(lsfit(m5$fitted.values,CC_Payments))

plot(m6$fitted.values,CC_Payments,xlab="m6 Fitted Values:  CC_Payments", ylab="Original Data: CC_Payments")
abline(lsfit(m6$fitted.values,CC_Payments))

plot(m7$fitted.values,CC_Payments,xlab="m7 Fitted Values:  CC_Payments", ylab="Original Data: CC_Payments")
abline(lsfit(m7$fitted.values,CC_Payments))

m8 <- lm(CC_Payments~City + Vacations)
summary(m8)
#As city was used to calculate the mtg field, if we don't use Mtg, city is significant (aliased)

#vacation is not significant in any model (which is correct) however the coefficients of correlated variables
#depend on the choice and method used
