#set parameters
md = 2
e = 0.7
threshold = 0.6

#training model under xgboost
bst1 = xgboost(data = as.matrix(train1[,-2]), label = train1[,2], max.depth = md, eta = e, nthread = 2, nround = 2, objective = "binary:logistic")
bst2 = xgboost(data = as.matrix(train2[,-2]), label = train2[,2], max.depth = md, eta = e, nthread = 2, nround = 2, objective = "binary:logistic")
bst3 = xgboost(data = as.matrix(train3[,-2]), label = train3[,2], max.depth = md, eta = e, nthread = 2, nround = 2, objective = "binary:logistic")
bst4 = xgboost(data = as.matrix(train4[,-2]), label = train4[,2], max.depth = md, eta = e, nthread = 2, nround = 2, objective = "binary:logistic")
bst5 = xgboost(data = as.matrix(train5[,-2]), label = train5[,2], max.depth = md, eta = e, nthread = 2, nround = 2, objective = "binary:logistic")

#testing model on test.set

tun.y = tun.set$Attrition
tun.x = tun.set[,-2]
pred.tun1 = predict(bst1, as.matrix(tun.x))
pred.tun2 = predict(bst2, as.matrix(tun.x))
pred.tun3 = predict(bst3, as.matrix(tun.x))
pred.tun4 = predict(bst4, as.matrix(tun.x))
pred.tun5 = predict(bst5, as.matrix(tun.x))

#confusion matrix on tuning.set
label1 = ifelse(pred.tun1>threshold,1,0)
label2 = ifelse(pred.tun2>threshold,1,0)
label3 = ifelse(pred.tun3>threshold,1,0)
label4 = ifelse(pred.tun4>threshold,1,0)
label5 = ifelse(pred.tun5>threshold,1,0)
label = label1+label2+label3+label4+label5
vote = ifelse(label>2,1,0)
(ConfM.BP<-table(tun.y,vote))
(Err.BP<-(sum(ConfM.BP)-sum(diag(ConfM.BP)))/sum(ConfM.BP))
recall = ConfM.BP[2,2]/sum(ConfM.BP[2,])
precision = ConfM.BP[2,2]/sum(ConfM.BP[,2])
(F1.score<-2*recall*precision/(recall+precision))

