install.packages("neuralnet")
install.packages("nnet")
install.packages("NeuralNetTools")

library(neuralnet)
library(nnet)
library(NeuralNetTools)
library(plyr) 

class(forestfires)
 str(forestfires)

 normalize <- function(x){
   return((x-min(x))/(max(x)-min(x)))
 }  

 
 forestfires_norm<-as.data.frame(lapply(forestfires[,3:30],FUN=normalize))
 View(forestfires_norm)
 summary(forestfires_norm)
 
 train <- forestfires_norm[1:400,]
 test <- forestfires_norm[401:517,] 

 forestfires_model <- neuralnet(area~FFMC+DMC+DC+ISI+temp+RH+wind+rain+area+dayfri+daymon+daysat+daysun+daythu+daytue+daywed+monthapr+monthaug+monthdec+monthfeb+monthjan+monthjul+monthjun+monthmar+monthmay+monthnov+monthoct+monthsep,data = train)
 plot(forestfires_model)
 model_result <- compute(forestfires_model,test) 
 str(model_result)

 
 predicted_area <- model_result$net.result
 cor(predicted_area,test$area)
 plot(predicted_area,test$Strength)

 
 f_model<-neuralnet(area~FFMC+DMC+DC+ISI+temp+RH+wind+rain+area+dayfri+daymon+daysat+daysun+daythu+daytue+daywed+monthapr+monthaug+monthdec+monthfeb+monthjan+monthjul+monthjun+monthmar+monthmay+monthnov+monthoct+monthsep,data= forestfires_norm,hidden = 5)
 plot(f_model)

  f_model_res<-compute(f_model,test)
 
 pred_str<-f_model_res$net.result
 cor(pred_str,test$area)

  plot(pred_str,test$area,col="blue") 
 