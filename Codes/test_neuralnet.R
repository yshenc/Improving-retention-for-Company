threshold = 0.5

test.y = test.set$Attrition
test.x = test.set[,-2]
output1 = compute(BPnet1,covariate=test.x)
output2 = compute(BPnet2,covariate=test.x)
output3 = compute(BPnet3,covariate=test.x)
output4 = compute(BPnet4,covariate=test.x)
output5 = compute(BPnet5,covariate=test.x)
pred.test1 = output1$net.result
pred.test2 = output2$net.result
pred.test3 = output3$net.result
pred.test4 = output4$net.result
pred.test5 = output5$net.result

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
