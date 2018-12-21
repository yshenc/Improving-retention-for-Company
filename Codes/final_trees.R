#read dataset
data = read.csv("HR-Employee-Attrition.csv",header=TRUE)
# fix(data)
names(data)[1] <- "Age"
dim(data)
str(data)

tableValue <- function(origin, pred) {
  size = length(origin)
  if(length(pred) != size) {
    return(list(TP=0, TN=0, FP=0, FN=0))
  }
  
  tp = 0
  tn = 0
  fp = 0
  fn = 0
  for(i in 1:size) {
    if(origin[i] == 'Yes') {
      if(pred[i] == 'Yes')
        tp = tp + 1
      else
        fn = fn + 1
    }
    else {
      if(pred[i] == 'Yes')
        fp = fp + 1
      else
        tn = tn + 1
    }
  }
  return(list(TP=tp, TN=tn, FP=fp, FN=fn))
}

# # clean data
# Recoding <- function(data) {
#   ## Recoding, turning all categorical data into numeric
#   data$Attrition = as.character(data$Attrition)
#   data$Attrition = sapply(data$Attrition, switch,
#                           "No"=0,
#                           "Yes"=1)
#   data$BusinessTravel = as.character(data$BusinessTravel)
#   data$BusinessTravel = sapply(data$BusinessTravel, switch,
#                                "Non-Travel"=0,
#                                "Travel_Rarely"=1,
#                                "Travel_Frequently"=2)
#   data$Department = as.character(data$Department)
#   data$Department = sapply(data$Department, switch,
#                            "Human Resources"=0,
#                            "Research & Development"=1,
#                            "Sales"=2)
#   data$EducationField = as.character(data$EducationField)
#   data$EducationField = sapply(data$EducationField, switch,
#                                "Human Resources"=0,
#                                "Life Sciences"=1,
#                                "Marketing"=2,
#                                "Medical"=3,
#                                "Technical Degree"=4,
#                                "Other"=5)
#   data$Gender = as.character(data$Gender)
#   data$Gender = sapply(data$Gender, switch,
#                        "Female"=0,
#                        "Male"=1)
#   unique(data$JobRole)
#   data$JobRole = as.character(data$JobRole)
#   data$JobRole = sapply(data$JobRole, switch,
#                         "Sales Executive"=0,
#                         "Research Scientist"=1,
#                         "Laboratory Technician"=2,
#                         "Manufacturing Director"=3,
#                         "Healthcare Representative"=4,
#                         "Manager"=5,
#                         "Sales Representative"=6,
#                         "Research Director"=7,
#                         "Human Resources"=8)
#   data$MaritalStatus = as.character(data$MaritalStatus)
#   data$MaritalStatus = sapply(data$MaritalStatus, switch,
#                               "Divorced"=0,
#                               "Married"=1,
#                               "Single"=2)
#   data$OverTime = as.character(data$OverTime)
#   data$OverTime = sapply(data$OverTime, switch,
#                          "No"=0,
#                          "Yes"=1)
#   return(data)
# }
# data = Recoding(data)
# summary(data)


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
train.no = train.set[which(train.set$Attrition=='No'),]
train.yes = train.set[which(train.set$Attrition=='Yes'),]
idx1 = train.no[c(1:168),]
idx2 = train.no[c(169:336),]
idx3 = train.no[c(337:504),]
idx4 = train.no[c(505:672),]
idx5 = train.no[c(673:841),]
set.seed(1)
train1 = sample(rbind(idx1,train.yes))
train2 = sample(rbind(idx2,train.yes))
train3 = sample(rbind(idx3,train.yes))
train4 = sample(rbind(idx4,train.yes))
train5 = sample(rbind(idx5,train.yes))
# ========================================================================
library(tree)
training_global <- NA
tuning_global <- NA
generateModel <- function(trainingSet, tuningSet) {

  training_global <<- trainingSet
  tuning_global <<- tuningSet
  set.seed (1)
    
  # Train a tree
  tree.train = tree(Attrition~., training_global)
  
  # Cross Validation to get best parameter
  cv.train = cv.tree(tree.train, FUN = prune.misclass)
  # OR
  # # prune the tree with tuning set to get best parameter
  # model = prune.misclass(tree.train, newdata = tuning_global)
  
  size = cv.train$size
  dev = cv.train$dev
  minIndex_dev = which.min(dev)
  bestTreeNodeNum = size[minIndex_dev]
  if(bestTreeNodeNum==1) {
    size <- size[-minIndex_dev]
    dev <- dev[-minIndex_dev]
    minIndex_dev = which.min(dev)
    bestTreeNodeNum = size[minIndex_dev]
  }
  
  # Train the best tree using best parameter
  best.train = prune.misclass(tree.train, best = bestTreeNodeNum)
  
  return(best.train)
}

model1 = generateModel(train1, tun.set)
plot(model1); text(model1, pretty=1)
model2 = generateModel(train2, tun.set)
plot(model2); text(model2, pretty=1)
model3 = generateModel(train3, tun.set)
plot(model3); text(model3, pretty=1)
model4 = generateModel(train4, tun.set)
plot(model4); text(model4, pretty=1)
model5 = generateModel(train5, tun.set)
plot(model5); text(model5, pretty=1)


pred1 = predict(model1, test.set, type = "class")
table(pred1, test.set$Attrition)
pred2 = predict(model2, test.set, type = "class")
table(pred2, test.set$Attrition)
pred3 = predict(model3, test.set, type = "class")
table(pred3, test.set$Attrition)
pred4 = predict(model4, test.set, type = "class")
table(pred4, test.set$Attrition)
pred5 = predict(model5, test.set, type = "class")
table(pred5, test.set$Attrition)

pred.collection = cbind(pred1, pred2, pred3, pred4, pred5)
pred.final = rowSums(pred.collection)
pred.final <- ifelse(pred.final > 7, 'Yes', 'No')
pred.final = as.factor(pred.final)

# Final test on test set
table(pred.final, test.set$Attrition)
value = tableValue(test.set$Attrition, pred.final)

accuracy = (value$TP+value$TN) / (value$TP+value$TN+value$FP+value$FN)
precision = value$TP / (value$TP+value$FP)
recall = (value$TP) / (value$TP+value$FN)
fMeasure = 2 * (precision * recall) / (precision + recall)
sprintf("Best Test Accuracy: %f", accuracy)
sprintf("Best Test precision: %f", precision)
sprintf("Best Test recall: %f", recall)
sprintf("Best Test fMeasure: %f", fMeasure)

# ============================================================================================

library(randomForest)

training_global <- NA
tuning_global <- NA
generateModel <- function(trainingSet,tuningSet) {
  training_global <<- trainingSet
  tuning_global <<- tuningSet
  set.seed (1)
  
  minUsedVariables = floor(sqrt(ncol(training_global)))-3
  maxUsedVariables = ceiling(sqrt(ncol(training_global)))+3
  if(minUsedVariables < 1) minUsedVariables = 2
  if(maxUsedVariables > (ncol(training_global)-1)) maxUsedVariables = ncol(training_global)-1
  
  bestPrecision = 0
  bestModel <- NA
  for(usedAmount in minUsedVariables:maxUsedVariables) {
    # Train a forest
    rForest.train = randomForest(Attrition~., data = training_global,
                                 mtry = usedAmount, importance = TRUE)
    
    # Test the validation set
    rForest.pred = predict(rForest.train, newdata = tuning_global)
    
    # Get precision
    value = tableValue(tuning_global$Attrition, rForest.pred)
    curPrecision = (value$TP+value$TN) / (value$TP+value$TN+value$FP+value$FN)
    
    if(curPrecision > bestPrecision) {
      bestPrecision = curPrecision
      bestModel = rForest.train
    }
  }
  
  return(bestModel)
}

model1 = generateModel(train1, tun.set)
print(model1)
plot(model1)
model2 = generateModel(train2, tun.set)
print(model2)
plot(model2)
model3 = generateModel(train3, tun.set)
print(model3)
model4 = generateModel(train4, tun.set)
print(model4)
model5 = generateModel(train5, tun.set)
print(model5)

pred1 = predict(model1, test.set, type = "class")
table(pred1, test.set$Attrition)
pred2 = predict(model2, test.set, type = "class")
pred3 = predict(model3, test.set, type = "class")
pred4 = predict(model4, test.set, type = "class")
pred5 = predict(model5, test.set, type = "class")

pred.collection = cbind(pred1, pred2, pred3, pred4, pred5)
pred.final = rowSums(pred.collection)

pred.final <- ifelse(pred.final > 7, 'Yes', 'No')
pred.final = as.factor(pred.final)

# Final test on test set
table(pred.final, test.set$Attrition)
value = tableValue(test.set$Attrition, pred.final)

accuracy = (value$TP+value$TN) / (value$TP+value$TN+value$FP+value$FN)
precision = value$TP / (value$TP+value$FP)
recall = (value$TP) / (value$TP+value$FN)
fMeasure = 2 * (precision * recall) / (precision + recall)
sprintf("Best Test Accuracy: %f", accuracy)
sprintf("Best Test precision: %f", precision)
sprintf("Best Test recall: %f", recall)
sprintf("Best Test fMeasure: %f", fMeasure)


