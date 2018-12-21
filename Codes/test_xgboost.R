threshold = 0.6

test.y = test.set$Attrition
test.x = test.set[,-2]
pred.test1 = predict(bst1, as.matrix(test.x))
pred.test2 = predict(bst2, as.matrix(test.x))
pred.test3 = predict(bst3, as.matrix(test.x))
pred.test4 = predict(bst4, as.matrix(test.x))
pred.test5 = predict(bst5, as.matrix(test.x))

#confusion matrix on tuning.set
label1 = ifelse(pred.test1>threshold,1,0)
label2 = ifelse(pred.test2>threshold,1,0)
label3 = ifelse(pred.test3>threshold,1,0)
label4 = ifelse(pred.test4>threshold,1,0)
label5 = ifelse(pred.test5>threshold,1,0)
label = label1+label2+label3+label4+label5
vote = ifelse(label>2,1,0)
(ConfM.BP<-table(test.y,vote))
(Err.BP<-(sum(ConfM.BP)-sum(diag(ConfM.BP)))/sum(ConfM.BP))
recall = ConfM.BP[2,2]/sum(ConfM.BP[2,])
precision = ConfM.BP[2,2]/sum(ConfM.BP[,2])
(F1.score<-2*recall*precision/(recall+precision))