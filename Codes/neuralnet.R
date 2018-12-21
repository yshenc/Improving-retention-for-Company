h = 2
threshold = 0.5
library(neuralnet)

BPnet1 = neuralnet(model,data=train1,hidden=h,err.fct="ce",act.fct = "logistic",linear.output = FALSE)
BPnet2 = neuralnet(model,data=train2,hidden=h,err.fct="ce",act.fct = "logistic",linear.output = FALSE)
BPnet3 = neuralnet(model,data=train3,hidden=h,err.fct="ce",act.fct = "logistic",linear.output = FALSE)
BPnet4 = neuralnet(model,data=train4,hidden=h,err.fct="ce",act.fct = "logistic",linear.output = FALSE)
BPnet5 = neuralnet(model,data=train5,hidden=h,err.fct="ce",act.fct = "logistic",linear.output = FALSE)
#testing model on test.set

tun.y = tun.set$Attrition
tun.x = tun.set[,-2]
output1 = compute(BPnet1,covariate=tun.x)
output2 = compute(BPnet2,covariate=tun.x)
output3 = compute(BPnet3,covariate=tun.x)
output4 = compute(BPnet4,covariate=tun.x)
output5 = compute(BPnet5,covariate=tun.x)
pred.tun1 = output1$net.result
pred.tun2 = output2$net.result
pred.tun3 = output3$net.result
pred.tun4 = output4$net.result
pred.tun5 = output5$net.result

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

summary(pred.tun1)
