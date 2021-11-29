### Linear models for dummies

#The packages we need
library(ggplot2)
library(car)
library(dplyr)

#The datasets we need
iris <-iris # This is a built-in dataset that we'll use for our first model

setwd("C:/Users/535388/OneDrive - UMONS/R folders/Course-statitstics") #We'll pick our own dataset from here

read.csv("linear.csv", sep = ";")-> dataset #This will be used later

#We want to make a model describing the relationship between sepal and petal length
#First of all, let's take a look at the data using a good old ggplot:
ggplot(data = iris, aes(x = Sepal.Length,y=Petal.Length,fill=Species )) +
  geom_point()

#Now, let's make the model
Model1 <-lm(data=iris, Petal.Length~ Sepal.Length) # The formula is Y ~ X. Y is the response, X is the explanation.
Model1 # We have the slope and the intercept...but are they significant ?
summary(Model1) # summary is used to get a closer look at our model.

# Bam ! That's a lot of stuff ! Here is the explanation, line by line:

#Call: it's the formula of the model
#Residuals: the distribution of the residuals. It should be almost symmetrical around 0.
#Coefficients: Estimate =  the slope and the intercept, Std.error = errors for slope and intercept,
#t value = values used for significance test, Pr(>|t|) = p value, the result of the test
# ***, it's great ! We can reject H0 with an alpha error smaller than 0.1% for both parameters!
#Degrees of freedom and stuff: used to compute F
#R? and adjusted R?: how much variance do we explain ? 75% 
#Here is the F statistic and the result of the test. p is really small: our model is relevant !

#If you need to access some parts of the model, use $ to open it. 
Model1$residuals# Open the model and get the residuals
Model1$fitted.values# Get the Y values predicted by the model for each X value.
Model1$coefficients# Get the slope and the intercept

#It's all fun and games, but did you check the assumptions ? YOU HAVE TO

#Assumption n?1: residuals follow a normal distribution
#Method 1: Shapiro-Wilk test. H0 is normality of the data. 
shapiro.test(Model1$residuals)# p = 83%, we can't reject H0 using a 5% threshold. 
#Method 2: QQplot using plot()
plot(Model1, which = 2) # If the points are on the line or close to it, residuals distribution is normal.
#Method 3: QQplot using the car package
qqPlot(Model1$residuals,envelope = 0.95) #Here we have a 95% confidence envelope. All points are in.


#Assumption n?2: homoscedasticity (variance is homogenous across observations)
plot(Model1, which = 1)# If the relationship is linear, the red line should be flat (no trends).
# If the data is homoscedastic, the dispersion of residuals should be the same across the plot.

# Wait a minute, our dataset...it is heteroscedastic !
#Let's get back to our ggplot...
ggplot(data = iris, aes(x = Sepal.Length,y=Petal.Length,fill=Species )) +
  geom_point(aes(color = Species)) #Flowers were not independent! We have different species and  different variances !
#Here we go again, with Iris virginica only.
virginica=filter(iris, Species == "virginica")
Model2<- lm(data=virginica, Petal.Length~ Sepal.Length)
summary(Model2)#Note: the intercept is not different from 0 using a 5% threshold.
qqPlot(Model2$residual, envelope = 0.95)# OK
plot(Model2, which = 1)#Everything is ok ! Points 1, 9 and 42 are a bit far from the others.
plot(Model2, which = 4)#Let's check for outliers using Cook's distance. 
#For each observation, Cook's distance is proportional to sum(f(X)-f(X)'). 
#f(X) is the predicted value of Y in a model with all observations, f(X)' is the same without the observation we are looking at.
#Once again 1, 9 and 42 are a bit influential in our model. We could inspect them but removing them is dangerous without a good reason!

ggplot(data = virginica, aes(x = Sepal.Length,y=Petal.Length)) +
  geom_point()+
  geom_smooth(method='lm')



#Expanding LMs with polynomials and interactions 

#Exemple of polynomial LM
Model3 = lm(data=virginica, Petal.Length~ Sepal.Length+ I(Sepal.Length^2))# Use I() to add powers of X as new variables.
summary(Model3)# It is really a bad model.Even Sepal.Length is not significant anymore.
poly2.ortho <- poly(virginica$Sepal.Length, degree=2)#Let's make an orthognal polynomial to remove correlation between X and X?.
Model3 = lm(data=virginica, Petal.Length~ poly2.ortho)#Compute the model once again.
summary(Model3)# X is not affected by X? anymore.

#Exemple of a model with interactions.
Model4= lm(data=virginica, Petal.Length~ Sepal.Length+Sepal.Width+ Sepal.Length*Sepal.Width)
summary(Model4)# Not a good one.


