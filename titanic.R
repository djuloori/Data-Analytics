dir()
#Reading the file from directory after setting change dir to Titanic_data.csv file location
data = read.csv("Titanic_data.csv")
data$X = NULL
summary(data)
data = data[sample(nrow(data)),T]
#Split data to Train data (80%) and Test data(20%)
select.data = sample (1:nrow(data), 0.8*nrow(data))
train.data = data[select.data,]
test.data = data[-select.data,]
dim(test.data)
colnames(train.data)
#Load data to corresponding variables from CSV files
PassengerId = train.data$PassengerId
Survived = train.data$Survived
Pclass = train.data$Pclass
Sex = train.data$Sex
Age = train.data$Age
SibSp = train.data$SibSp
Parch = train.data$Parch
Ticket = train.data$Ticket
Fare = train.data$Fare
Embarked = train.data$Embarked
#Backward Elimination
fit = glm(as.factor(Survived) ~ ., family = binomial(), data = train.data)
summary(fit) 
fit1 = glm(as.factor(Survived) ~ PassengerId + Pclass + Sex + Age + SibSp + Parch + Ticket + Fare, family = binomial(), data = train.data)
summary(fit1)
fit2 = glm(as.factor(Survived) ~ PassengerId + Pclass + Sex + Age + SibSp + Ticket + Fare, family = binomial(), data = train.data)
summary(fit2)
fit3 = glm(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Ticket + Fare, family = binomial(), data = train.data)
summary(fit3)
fit4 = glm(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Ticket, family = binomial(), data = train.data)
summary(fit4)
fit5 = glm(as.factor(Survived) ~ Pclass + Sex + Age + SibSp, family = binomial(), data = train.data)
summary(fit5)
#Psuedo R square
nullmod = glm(Survived~1,data=data,family=binomial())
1-logLik(fit5)/logLik(nullmod)
#Stepwise Regression
step(fit, direction = "both")
fit6 = glm(as.factor(Survived) ~ Pclass + Sex + Age + SibSp, family = binomial(), data = train.data)
#Psuedo R Square for Step-fit
1-logLik(fit6)/logLik(nullmod)
Prediction = predict(fit6,test.data)
Prediction[Prediction > 0.421] = 1
Prediction[Prediction < 0.421] = 0
Prediction
#Accuracy of The Model
Accuracy = function (y_pred, y_true) {
    Accuracy = mean(y_true == y_pred)
    return(Accuracy)
  }
Accuracy(y_pred = Prediction, y_true = newdata$Survived)
confusionMatrix(data=Prediction, reference=newdata$Survived)
-------------------------------------------------------------------------------------------------
#RandomForest Algorithm
library(randomForest)
set.seed(415)
fit7 = randomForest(as.factor(Survived) ~ ., data = train.data, importance = TRUE, ntree=2000)
varImpPlot(fit7,main = "Variable Importance")
library(caret)
plot(fit7)
response = predict(fit7, test.data)
response
library(e1071)
library(caret)
confusionMatrix(data=response, reference=test.data$Survived)
print(fit7)
--------------------------------------------------------------------------------------------------
#Calculating the threshold value
install.packages("ROCR")
library(ROCR)
Prediction = predict(fit6,test.data)
Pred = prediction(Prediction,test.data$Survived)
perf = performance(Pred, measure = "sens", x.measure = "cutoff")
plot(perf)
perf = performance(Pred, measure = "spec", x.measure = "cutoff")
acc.perf = performance(Pred, measure = "acc")
plot(acc.perf)
ind = which.max( slot(acc.perf, "y.values")[[1]] )
acc = slot(acc.perf, "y.values")[[1]][ind]
cutoff = slot(acc.perf, "x.values")[[1]][ind]
print(c(cutoff = cutoff))
