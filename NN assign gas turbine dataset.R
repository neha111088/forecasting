library(neuralnet)
library(nnet)
library(NeuralNetTools)
library(plyr) 

View(gas_turbines)
class(gas_turbines)      
str(gas_turbines)

normalize <- function(x){
  return((x-min(x))/(max(x)-min(x)))
}


gasturbines_norm<-as.data.frame(lapply(gas_turbines[,1:11],FUN=normalize))
View(gasturbines_norm)
summary(gasturbines_norm)


train <- gasturbines_norm[1:10000,]
test <-  gasturbines_norm[10001:15039,]


gasturbines_model <- neuralnet(TEY~AT+AP+AH+AFDP+GTEP+TIT+TAT+CDP+CO+NOX,data = train)
plot(gasturbines_model)

model_result <- compute(gasturbines_model,test)
str(model_result)


predicted_TEY <- model_result$net.result
cor(predicted_TEY,test$TEY)

plot(predicted_TEY,test$NOX)


f_model<-neuralnet(TEY~AT+AP+AH+AFDP+GTEP+TIT+TAT+CDP+CO+NOX,data= gasturbines_norm,hidden = 5)
plot(f_model)
f_model_res<-compute(f_model,test)

pred_str<-f_model_res$net.result
cor(pred_str,test$TEY)
plot(pred_str,test$TEY,col="blue")
