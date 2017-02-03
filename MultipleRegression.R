# load MASS package
library(MASS)

data(Boston)

#split the data by using the first 400 observations as the training data and the remaining as the testing data
train = 1:400
test = -train

# we are keeping all variables in training and testing data
training_data = Boston[train,]
testing_data = Boston[test,]


#use cor() function to find correlations between variables
cor(training_data$age, training_data$medv)


#plot both varaiables
plot(training_data$age, 
     training_data$medv, 
     xlab = "Proportion of owner-occupied units built prior to 1940", 
     ylab = "Median house value")


#linear model
lm.fit = lm(training_data$medv~training_data$age)
summary(lm.fit)
plot(lm.fit)


lm.fit = lm(training_data$medv~training_data$lstat)
summary(lm.fit)
plot(lm.fit)


plot(training_data$lstat, training_data$medv, main ="Scatterplot", xlab="Lstat", ylab="Median Value")
abline(lm.fit, col="red", lwd=6)

# Multiple Regression
pairs(Boston)
pairs(Boston[,c(1,3,7)])



#Verifier si le modèle lionaire entre medv, lstat et age
model1 = lm(medv~ lstat + age, data = training_data)
summary(model1)

#use the training dataset to tarin the model. lm() allows you to specify the data you want to use. 
model2 = lm(medv~ log(lstat) + age, data = training_data)
summary(model2)

#avec toutes les autres variables
model3 = lm(medv~., data = training_data)
summary(model3)

#Exclure les variables non significatives
model4 = lm(medv~.-age-indus-black, data = training_data)
summary(model4)

#interaction entre variable
model4 = lm(medv~lstat*age, data = training_data)
summary(model4)

model5 = lm(medv~lstat:age, data = training_data)
summary(model5)

#Non linear transformation
model6 = lm(medv~poly(lstat,2), data = training_data)
summary(model6)

model6 = lm(medv~poly(lstat,3), data = training_data)
summary(model6)

model6 = lm(medv~poly(lstat,4), data = training_data)
summary(model6)

model6 = lm(medv~poly(lstat,5), data = training_data)
summary(model6)

model6 = lm(medv~poly(lstat,6), data = training_data)
summary(model6)

model6 = lm(medv~poly(lstat,7), data = training_data)
summary(model6)

