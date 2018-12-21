# Before loading the data, save data in csv-UTF8
Employee <- read.csv(file="/Users/gavin/Documents/WPI_2018_Spring/502 stat/dataset/HR-Employee-Attrition_delete.csv", header = TRUE)
#fix(Employee)
names(Employee)[1] <- "Age"
dim(Employee)
names(Employee)
summary(Employee)

Recoding <- function(Employee) {
  ## Recoding, turning all categorical data into numeric
  Employee[ ,'Attrition'] <- sapply(Employee[ ,'Attrition'], switch,
                                    "No"=0,
                                    "Yes"=1)
  Employee[ ,'BusinessTravel'] <- sapply(Employee[ ,'BusinessTravel'], switch,
                                         "Non-Travel"=0,
                                         "Travel_Rarely"=1,
                                         "Travel_Frequently"=2)
  Employee[ ,'Department'] <- sapply(Employee[ ,'Department'], switch,
                                     "Human Resources"=0,
                                     "Research & Development"=1,
                                     "Sales"=2)
  Employee[ ,'EducationField'] <- sapply(Employee[ ,'EducationField'], switch,
                                         "Human Resources"=0,
                                         "Life Sciences"=1,
                                         "Marketing"=2,
                                         "Medical"=3,
                                         "Technical Degree"=4,
                                         "Other"=5)
  Employee[ ,'Gender'] <- sapply(Employee[ ,'Gender'], switch,
                                 "Female"=0,
                                 "Male"=1)
  unique(Employee['JobRole'])
  Employee[ ,'JobRole'] <- sapply(Employee[ ,'JobRole'], switch,
                                  "Sales Executive"=0,
                                  "Research Scientist"=1,
                                  "Laboratory Technician"=2,
                                  "Manufacturing Director"=3,
                                  "Healthcare Representative"=4,
                                  "Manager"=5,
                                  "Sales Representative"=6,
                                  "Research Director"=7,
                                  "Human Resources"=8)
  Employee[ ,'MaritalStatus'] <- sapply(Employee[ ,'MaritalStatus'], switch,
                                        "Divorced"=0,
                                        "Married"=1,
                                        "Single"=2)
  #Employee[ ,'Over18'] <- sapply(Employee[ ,'OverTime'], switch,
  #"Yes"=1)
  Employee[ ,'OverTime'] <- sapply(Employee[ ,'OverTime'], switch,
                                   "No"=0,
                                   "Yes"=1)
  return(Employee)
}
Employee <- Recoding(Employee)
summary(Employee)

#pairs(~ Attrition + BusinessTravel + Department, Employee)
library(e1071)

#divide dataset
set.seed(1)
n = dim(Employee)[1]
hand = sample(1:n,1200)
hand.set = Employee[hand,]
hand.set$Attrition = hand.set$Attrition
test.set = Employee[-hand,]

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
#change the attrition into factor
train1$Attrition <- as.factor(train1$Attrition)
train2$Attrition <- as.factor(train2$Attrition)
train3$Attrition <- as.factor(train3$Attrition)
train4$Attrition <- as.factor(train4$Attrition)
train5$Attrition <- as.factor(train5$Attrition)
#select_value=seq(0.05, 0.1, by = 0.0002)  
f1_vector=0
sucessful_rate_vector=0
#select_value=0.09
#select_value=seq(0.001, 1, by = 0.005)  
select_value=0.09
#cost=0.09 optimal suceesfule rate=0.87

for (j in select_value){
  svm.radial_train1 = svm(Attrition~., data=train1, kernel="radial", cost=j)
  svm.radial_train2 = svm(Attrition~., data=train2, kernel="radial", cost=j)
  svm.radial_train3 = svm(Attrition~., data=train3, kernel="radial", cost=j)
  svm.radial_train4 = svm(Attrition~., data=train4, kernel="radial", cost=j)
  svm.radial_train5 = svm(Attrition~., data=train5, kernel="radial", cost=j)
  result_train1=predict(svm.radial_train1, tun.set)
  result_train2=predict(svm.radial_train2, tun.set)
  result_train3=predict(svm.radial_train3, tun.set)
  result_train4=predict(svm.radial_train4, tun.set)
  result_train5=predict(svm.radial_train5, tun.set)
  final_result=0
  
  final_result=as.numeric(result_train1)+as.numeric(result_train2)+as.numeric(result_train3)+as.numeric(result_train4)+as.numeric(result_train5)
  #final_result=as.numeric(result_train1)+result_train2+result_train3+result_train4+result_train5
  print(final_result)
  for ( i in 1:200 ){
    if (final_result[i] > 7){
      final_result[i]=1
    } else{final_result[i]=0}
  }
  print(final_result)
  svm.radial_Confusion_matrix=table(final_result,tun.set$Attrition)
  svm.radial_Confusion_matrix
  sucessful_rate=sum(diag(svm.radial_Confusion_matrix))/200
  sucessful_rate
  sucessful_rate_vector=c(sucessful_rate_vector,sucessful_rate)
  #precision=svm.poly_Confusion_matrix[1,1]/(svm.poly_Confusion_matrix[1,1]+svm.poly_Confusion_matrix[1,2])
  #recall=svm.poly_Confusion_matrix[1,1]/(svm.poly_Confusion_matrix[1,1]+svm.poly_Confusion_matrix[2,1])
  #f1=2*recall*precision/(precision+recall)
  #f1_vector=c(f1_vector,f1)
}
print(sucessful_rate_vector)
#print(f1_vector)
#select_value[200]
plot(select_value,sucessful_rate_vector[-1],type='l',xlab='cost',ylab='accuracy',main='Radial SVM accuracy')
sucessful_rate_vector[20]
select_value[19]
text(select_value[19], sucessful_rate_vector[20],labels='optimal(0.09,0.87)',cex= 0.9,pos=4)
points(select_value[19],sucessful_rate_vector[20], col = "red")
table(final_result,tun.set$Attrition, dnn = c('Prediction','Truth'))


#use testdata set to predict

result_train1=predict(svm.radial_train1, test.set)
result_train2=predict(svm.radial_train2, test.set)
result_train3=predict(svm.radial_train3, test.set)
result_train4=predict(svm.radial_train4, test.set)
result_train5=predict(svm.radial_train5, test.set)
final_result_test=0

final_result_test=as.numeric(result_train1)+as.numeric(result_train2)+as.numeric(result_train3)+as.numeric(result_train4)+as.numeric(result_train5)
#final_result=as.numeric(result_train1)+result_train2+result_train3+result_train4+result_train5
print(final_result_test)
for ( i in 1:length(final_result_test) ){
  if (final_result_test[i] > 7){
    final_result_test[i]=1
  } else{final_result_test[i]=0}
}
print(final_result_test)
svm.radial_Confusion_matrix_test=table(final_result_test,test.set$Attrition, dnn = c('Prediction','Truth'))
svm.radial_Confusion_matrix_test
sucessful_rate=sum(diag(svm.radial_Confusion_matrix_test))/270
sucessful_rate
#0.8296296


precision=svm.radial_Confusion_matrix_test[2,2]/(svm.radial_Confusion_matrix_test[2,2]+svm.radial_Confusion_matrix_test[2,1])
precision
recall=svm.radial_Confusion_matrix_test[2,2]/(svm.radial_Confusion_matrix_test[2,2]+svm.radial_Confusion_matrix_test[1,2])
recall
f1=2*recall*precision/(precision+recall)
f1
