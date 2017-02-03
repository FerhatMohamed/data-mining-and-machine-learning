# import the Lang Cap Data into R
LungCapData <- read.table(file.choose(), header = T, sep="\t")

#attach the data
attach(LungCapData)

# check varibale names, types, etc.
names(LungCapData)


# type of the variables
class(Age)
class(Smoke)
levels(Smoke)

# fit a model using Age and Height 
model1 <- lm(LungCap ~ Age + Height)

# ask fo a summary for a model1
summary(model1)

# Calculate Pearson's correlation between Age and Height
cor(Age, Height, method="pearson")

# ask for confidence intervals for the model coefficients
confint(model1, coef.level=0.95)

# fit a model using all X variables
model2 <- lm(LungCap ~ Age + Height + Smoke + Gender + Caesarean)

# ask fo a summary for a model1
summary(model2)

# check the regression diagnostic plots for this model
plot(model2)




