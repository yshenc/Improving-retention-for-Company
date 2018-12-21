#read dataset
data = read.csv("HR-Employee-Attrition.csv",header=TRUE)
dim(data)
str(data)

#clean data
Recoding <- function(data) {
  ## Recoding, turning all categorical data into numeric
  data$Attrition = as.character(data$Attrition)
  data$Attrition = sapply(data$Attrition, switch,
                                    "No"=0,
                                    "Yes"=1)
  data$BusinessTravel = as.character(data$BusinessTravel)
  data$BusinessTravel = sapply(data$BusinessTravel, switch,
                                "Non-Travel"=0,
                                "Travel_Rarely"=1,
                                "Travel_Frequently"=2)
  data$Department = as.character(data$Department)
  data$Department = sapply(data$Department, switch,
                                     "Human Resources"=0,
                                     "Research & Development"=1,
                                     "Sales"=2)
  data$EducationField = as.character(data$EducationField)
  data$EducationField = sapply(data$EducationField, switch,
                                         "Human Resources"=0,
                                         "Life Sciences"=1,
                                         "Marketing"=2,
                                         "Medical"=3,
                                         "Technical Degree"=4,
                                         "Other"=5)
  data$Gender = as.character(data$Gender)
  data$Gender = sapply(data$Gender, switch,
                                 "Female"=0,
                                 "Male"=1)
  unique(data$JobRole)
  data$JobRole = as.character(data$JobRole)
  data$JobRole = sapply(data$JobRole, switch,
                                  "Sales Executive"=0,
                                  "Research Scientist"=1,
                                  "Laboratory Technician"=2,
                                  "Manufacturing Director"=3,
                                  "Healthcare Representative"=4,
                                  "Manager"=5,
                                  "Sales Representative"=6,
                                  "Research Director"=7,
                                  "Human Resources"=8)
  data$MaritalStatus = as.character(data$MaritalStatus)
  data$MaritalStatus = sapply(data$MaritalStatus, switch,
                                        "Divorced"=0,
                                        "Married"=1,
                                        "Single"=2)
  data$OverTime = as.character(data$OverTime)
  data$OverTime = sapply(data$OverTime, switch,
                                   "No"=0,
                                   "Yes"=1)
  return(data)
}
data = Recoding(data)
summary(data)

#divide dataset
set.seed(1)
n = dim(data)[1]
hand = sample(1:n,1200)
hand.set = data[hand,]
hand.set$Attrition = hand.set$Attrition
test.set = data[-hand,]

#divide hand.set
set.seed(1)
tun = sample(1:1200,200)
train.set = hand.set[-tun,]
tun.set = hand.set[tun,]

#divide train.set
train.no = train.set[which(train.set$Attrition==0),]
train.yes = train.set[which(train.set$Attrition==1),]
idx1 = train.no[c(1:168),]
idx2 = train.no[c(169:336),]
idx3 = train.no[c(337:504),]
idx4 = train.no[c(505:672),]
idx5 = train.no[c(673:841),]
train1 = rbind(idx1,train.yes)
train2 = rbind(idx2,train.yes)
train3 = rbind(idx3,train.yes)
train4 = rbind(idx4,train.yes)
train5 = rbind(idx5,train.yes)

#linear logisitc
myFitLog1 = glm(Attrition ~.,train1,family = "binomial")
myFitLog2 = glm(Attrition ~.,train1,family = "binomial")
myFitLog3 = glm(Attrition ~.,train1,family = "binomial")
myFitLog4 = glm(Attrition ~.,train1,family = "binomial")
myFitLog5 = glm(Attrition ~.,train1,family = "binomial")
myFitLogPred1 = predict(myFitLog1,test.set,type="response")
myFitLogPred2 = predict(myFitLog2,test.set,type="response")
myFitLogPred3 = predict(myFitLog3,test.set,type="response")
myFitLogPred4 = predict(myFitLog4,test.set,type="response")
myFitLogPred5 = predict(myFitLog5,test.set,type="response")

pred1 = rep(0,270)
pred2 = rep(0,270)
pred3 = rep(0,270)
pred4 = rep(0,270)
pred5 = rep(0,270)
predT = rep(0,270)
pred1[myFitLogPred1 >= 0.50] = 1
pred1[myFitLogPred1 < 0.50] = 0
pred2[myFitLogPred2 >= 0.50] = 1
pred2[myFitLogPred2 < 0.50] = 0
pred3[myFitLogPred3 >= 0.50] = 1
pred3[myFitLogPred3 < 0.50] = 0
pred4[myFitLogPred4 >= 0.50] = 1
pred4[myFitLogPred4 < 0.50] = 0
pred5[myFitLogPred5 >= 0.50] = 1
pred5[myFitLogPred5 < 0.50] = 0
pred = pred1+pred2+pred3+pred4+pred5

predT[pred >= 3] = 1
predT[pred < 3] = 0
table(predT, test.set$Attrition)

Accuracy = (164+40)/270
precision = 40/(40+10)
recall = 40/(40+56)
f1 = 2*precision*recall/(precision+recall)
Accuracy
precision
recall
f1


#PCA + linear Logistic regression

pca1 <- prcomp(train1[,-2],center=TRUE, scale=TRUE)
plot(pca1$sdev,type = 'o')
newdata1 = pca1$x[, 1:5]
newdata1 = data.frame(newdata1,"Attrition"=train1$Attrition)
myFitLog1 = glm(Attrition ~. ,newdata1,family = "binomial")
test.p1 = predict(pca1,newdata = test.set[,-2])[,1:5]
myFitLogPred1 = predict(myFitLog1,newdata = data.frame(test.p1),type="response")

pca2 <- prcomp(train2[,-2],center=TRUE, scale=TRUE)
plot(pca2$sdev,type = 'o')
newdata2 = pca2$x[, 1:6]
newdata2 = data.frame(newdata2,"Attrition"=train2$Attrition)
myFitLog2 = glm(Attrition ~. ,newdata2,family = "binomial")
test.p2 = predict(pca2,newdata = test.set[,-2])[,1:6]
myFitLogPred2 = predict(myFitLog2,newdata = data.frame(test.p2),type="response")

pca3 <- prcomp(train3[,-2],center=TRUE, scale=TRUE)
plot(pca3$sdev,type = 'o')
newdata3 = pca3$x[, 1:6]
newdata3 = data.frame(newdata3,"Attrition"=train3$Attrition)
myFitLog3 = glm(Attrition ~. ,newdata3,family = "binomial")
test.p3 = predict(pca3,newdata = test.set[,-2])[,1:6]
myFitLogPred3 = predict(myFitLog3,newdata = data.frame(test.p3),type="response")

pca4 <- prcomp(train4[,-2],center=TRUE, scale=TRUE)
plot(pca4$sdev,type = 'o')
newdata4 = pca4$x[, 1:6]
newdata4 = data.frame(newdata4,"Attrition"=train4$Attrition)
myFitLog4 = glm(Attrition ~. ,newdata4,family = "binomial")
test.p4 = predict(pca4,newdata = test.set[,-2])[,1:6]
myFitLogPred4 = predict(myFitLog4,newdata = data.frame(test.p4),type="response")

pca5 <- prcomp(train5[,-2],center=TRUE, scale=TRUE)
plot(pca5$sdev,type = 'o')
newdata5 = pca5$x[, 1:6]
newdata5 = data.frame(newdata5,"Attrition"=train5$Attrition)
myFitLog5 = glm(Attrition ~. ,newdata5,family = "binomial")
test.p5 = predict(pca5,newdata = test.set[,-2])[,1:6]
myFitLogPred5 = predict(myFitLog5,newdata = data.frame(test.p5),type="response")

pred1 = rep(0,270)
pred2 = rep(0,270)
pred3 = rep(0,270)
pred4 = rep(0,270)
pred5 = rep(0,270)
predT = rep(0,270)
pred1[myFitLogPred1 >= 0.50] = 1
pred1[myFitLogPred1 < 0.50] = 0
pred2[myFitLogPred2 >= 0.50] = 1
pred2[myFitLogPred2 < 0.50] = 0
pred3[myFitLogPred3 >= 0.50] = 1
pred3[myFitLogPred3 < 0.50] = 0
pred4[myFitLogPred4 >= 0.50] = 1
pred4[myFitLogPred4 < 0.50] = 0
pred5[myFitLogPred5 >= 0.50] = 1
pred5[myFitLogPred5 < 0.50] = 0
pred = pred1+pred2+pred3+pred4+pred5

predT[pred >= 3] = 1
predT[pred < 3] = 0
table(predT, test.set$Attrition)

Accuracy = (149+40)/270
precision = 40/(40+10)
recall = 40/(40+71)
f1 = 2*precision*recall/(precision+recall)
Accuracy
precision
recall
f1


#lasso + linear regression
library(glmnet)
myAlpha=1
myLambda = 10^(seq(4,-2,length=100))

XTest = model.matrix(Attrition~.,test.set)[,-1]
YTest = test.set$Attrition
XTrain1 = model.matrix(Attrition~.,train1)[,-1]
YTrain1 = train1$Attrition
XTrain2 = model.matrix(Attrition~.,train2)[,-1]
YTrain2 = train2$Attrition
XTrain3 = model.matrix(Attrition~.,train3)[,-1]
YTrain3 = train3$Attrition
XTrain4 = model.matrix(Attrition~.,train4)[,-1]
YTrain4 = train4$Attrition
XTrain5 = model.matrix(Attrition~.,train5)[,-1]
YTrain5 = train5$Attrition




myFitL1 = glmnet(XTrain1,YTrain1,alpha=myAlpha,lambda=myLambda,family = 'binomial')
myFitLcv1 <- cv.glmnet(XTrain1, YTrain1, alpha =myAlpha, lambda = myLambda, family = 'binomial')
myFitRL1 = myFitLcv1$lambda.min
mypred1 = predict(myFitL1,s = myFitRL1, newx=XTest,type='response')

myFitL2 = glmnet(XTrain2,YTrain2,alpha=myAlpha,lambda=myLambda,family = 'binomial')
myFitLcv2 <- cv.glmnet(XTrain2, YTrain2, alpha =myAlpha, lambda = myLambda, family = 'binomial')
myFitRL2 = myFitLcv2$lambda.min
mypred2 = predict(myFitL2,s = myFitRL2, newx=XTest,type='response')

myFitL3 = glmnet(XTrain3,YTrain3,alpha=myAlpha,lambda=myLambda,family = 'binomial')
myFitLcv3 <- cv.glmnet(XTrain3, YTrain3, alpha =myAlpha, lambda = myLambda, family = 'binomial')
myFitRL3 = myFitLcv3$lambda.min
mypred3 = predict(myFitL3,s = myFitRL3, newx=XTest,type='response')

myFitL4 = glmnet(XTrain4,YTrain4,alpha=myAlpha,lambda=myLambda,family = 'binomial')
myFitLcv4 <- cv.glmnet(XTrain4, YTrain4, alpha =myAlpha, lambda = myLambda, family = 'binomial')
myFitRL4 = myFitLcv4$lambda.min
mypred4 = predict(myFitL4,s = myFitRL4, newx=XTest,type='response')

myFitL5 = glmnet(XTrain5,YTrain5,alpha=myAlpha,lambda=myLambda,family = 'binomial')
myFitLcv5 <- cv.glmnet(XTrain5, YTrain5, alpha =myAlpha, lambda = myLambda, family = 'binomial')
myFitRL5 = myFitLcv5$lambda.min
mypred5 = predict(myFitL5,s = myFitRL5, newx=XTest,type='response')

pred1 = rep(0,270)
pred2 = rep(0,270)
pred3 = rep(0,270)
pred4 = rep(0,270)
pred5 = rep(0,270)
predT = rep(0,270)
pred1[mypred1 >= 0.50] = 1
pred1[mypred1 < 0.50] = 0
pred2[mypred2 >= 0.50] = 1
pred2[mypred2 < 0.50] = 0
pred3[mypred3 >= 0.50] = 1
pred3[mypred3 < 0.50] = 0
pred4[mypred4 >= 0.50] = 1
pred4[mypred4 < 0.50] = 0
pred5[mypred5 >= 0.50] = 1
pred5[mypred5 < 0.50] = 0
pred = pred1+pred2+pred3+pred4+pred5

predT[pred >= 3] = 1
predT[pred < 3] = 0
table(predT, test.set$Attrition)

Accuracy = (171+42)/270
precision = 42/(42+8)
recall = 42/(42+49)
f1 = 2*precision*recall/(precision+recall)
Accuracy
precision
recall
f1
predict(myFitL1, s = myFitRL1, type = "coefficients")
predict(myFitL2, s = myFitRL2, type = "coefficients")
predict(myFitL3, s = myFitRL3, type = "coefficients")
predict(myFitL4, s = myFitRL4, type = "coefficients")
predict(myFitL5, s = myFitRL5, type = "coefficients")
