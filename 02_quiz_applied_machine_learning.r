
# coursera practical machine learning quizz 2

library(AppliedPredictiveModeling)
library(caret)
data(AlzheimerDisease)



adData = data.frame(diagnosis,predictors)
trainIndex = createDataPartition(diagnosis, p = 0.50)
training = adData[trainIndex,]
testing = adData[-trainIndex,]

# No funciona: Error in `[.default`(xj, i) : invalid subscript type 'list'

adData = data.frame(diagnosis,predictors)
train = createDataPartition(diagnosis, p = 0.50,list=FALSE)
test = createDataPartition(diagnosis, p = 0.50,list=FALSE)

# NO FUNCIONA! buen truco: porque genera misma lista de train que de test

adData = data.frame(diagnosis,predictors)
trainIndex = createDataPartition(diagnosis, p = 0.50,list=FALSE)
training = adData[trainIndex,]
testing = adData[-trainIndex,]

head(training)
head(testing)

nrow(training); nrow(testing)
# Por eso dice "approximately 50%, son 166 y 167


# Question 2

library(AppliedPredictiveModeling)
data(concrete)
library(caret)
set.seed(975)
inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
training = mixtures[ inTrain,]
testing = mixtures[-inTrain,]

# Make a plot of the outcome (CompressiveStrength) versus the index of the samples. 
# Color by each of the variables in the data set (you may find the cut2() function 
# in the Hmisc package useful for turning continuous covariates into factors). 
# What do you notice in these plots? 

caret::featurePlot(x=training[,c(1:8)],
                   y = training$CompressiveStrength,
                   plot="pairs")

# Parece que age es el que muestra el efecto "escalera" y no 

ggplot2::qplot(training$Age,
               training$CompressiveStrength)

ggplot2::qplot(training$CompressiveStrength,
               training$Age)

ggplot2::qplot(training$FlyAsh,
               training$CompressiveStrength)

# Igualmente con qplot: FlyAsh parece que no predice nada

# Pero lo que te pide es COLOREAR POR CADA UNO DE LOS PREDICTORES!!!!

ggplot2::qplot(sort(as.integer(rownames(training))),
               training$CompressiveStrength,
               colour = cut2(training$Age, g = 5))

# ¡¡¡PERFECTO SE VE PERFECTO!!

ggplot2::qplot(sort(as.integer(rownames(training))),
               training$CompressiveStrength,
               colour = cut2(training$FlyAsh, g = 5))

# para nada se ve ese efecto, mezcla total de colores
# más simple con dos intervalos nada más

ggplot2::qplot(sort(as.integer(rownames(training))),
               training$CompressiveStrength,
               colour = cut2(training$FlyAsh, g = 2))

ggplot2::qplot(sort(as.integer(rownames(training))),
               training$CompressiveStrength,
               colour = cut2(training$Age, g = 2))


# Q3

library(AppliedPredictiveModeling)
data(concrete)
library(caret)
set.seed(975)
inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
training = mixtures[ inTrain,]
testing = mixtures[-inTrain,]

# Make a histogram and confirm the SuperPlasticizer variable is skewed. 
# Normally you might use the log transform to try to make the data more symmetric. 
# Why would that be a poor choice for this variable?

summary(training)
hist(training$Superplasticizer)

# There are values of zero so when you take the log() transform those values will be -Inf.
# The log transform does not reduce the skewness of the non-zero values of SuperPlasticizer
# The log transform produces negative values which can not be used by some classifiers.
# The log transform is not a monotone transformation of the data. 

summary(log(training))
hist(log(training$Superplasticizer))
qqnorm(training$Superplasticizer)


#Question 4
# Load the Alzheimer's disease data using the commands:

set.seed(3433)
library(AppliedPredictiveModeling)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

# Find all the predictor variables in the training set that begin with IL. 
# Perform principal components on these variables with the preProcess() 
# function from the caret package. Calculate the number of principal components 
# needed to capture 80% of the variance. How many are there?
# 12
# 10
# 11
#  7

names(training)
grep(glob2rx("IL*"), names(training))

# Extraemos estas variables

training2 <- training[, grep(glob2rx("IL*"), names(training))]
names(training2)

pp <- preProcess(training2, 
                 method="pca",
                 thresh = 0.8) # buenísimo esto: pones tu objetivo de varianza explicada
pp


# Question 5
# Load the Alzheimer's disease data using the commands:

set.seed(3433)
library(AppliedPredictiveModeling)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

# Create a training data set consisting of only the predictors with variable names 
# beginning with IL and the diagnosis. 
# Build two predictive models, one using the predictors as they are and one using PCA 
# with principal components explaining 80% of the variance in the predictors. 
# Use method="glm" in the train function. What is the accuracy of each method in the 
# test set? Which is more accurate?

# Non-PCA Accuracy: 0.72 # PCA Accuracy: 0.71
# Non-PCA Accuracy: 0.74 # PCA Accuracy: 0.74
# Non-PCA Accuracy: 0.65 # PCA Accuracy: 0.72
# Non-PCA Accuracy: 0.72   PCA Accuracy: 0.71

names(training)
grep(glob2rx("IL*"), names(training))

training2 <- training[, c(1,grep(glob2rx("IL*"), names(training)))] # grep saca índices de columnas
names(training2)

inTrain <- createDataPartition(y = training2$diagnosis,
                               ## the outcome data are needed
                               p = .80,
                               ## The percentage of data in the
                               ## training set
                               list = FALSE) ## The format of the results
  
nrow(inTrain)
nrow(training2)

training <- training2[ inTrain,]
testing <- training2[-inTrain,]
nrow(training)
nrow(testing)

# Proceso *SIN* PCA 
glmFit <- train(diagnosis ~ .,
                data = training,
                method = "glm")
glmFit

pred <- predict(glmFit, newdata = testing)
confusionMatrix(pred, testing$diagnosis)
#               Accuracy : 0.6735 (para 80/20) 

# Proceso *CON* PCA 
glmFit <- train(diagnosis ~ .,
                data = training,
                method = "glm",
                preProcess = "pca")
glmFit

pred <- predict(glmFit, newdata = testing)
confusionMatrix(pred, testing$diagnosis)
#               Accuracy : 0.71   (para 80/20)
