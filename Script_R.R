#Proyecto de aula, Sistemas Inteligentes; 
library(readr)
library(caTools)
library(caret)
x <- file.choose()
conjunto_datosX <- read.table(x,sep=";", header = TRUE)
conjunto_datosX
conjunto_datosX$Resultado 
conjunto_datos <- conjunto_datosX[,c(1:19)]  
conjunto_datos$Resultado <- factor(ifelse(conjunto_datos$Resultado =="NO", "1", conjunto_datos$Resultado))
conjunto_datos$Resultado <- factor(ifelse(conjunto_datos$Resultado =="SI","2", conjunto_datos$Resultado))
conjunto_datos
kfold.crossval.reg=function(df,nfolds){
  fold=sample(1:nfolds, nrow(df),replace = TRUE)
  cat("------------------------")
  cat(fold)
  mean.sqr.errs=sapply(1:nfolds,   
                       kfold.cval.reg.iter,
                       df, fold) 
  list("MSE"=mean.sqr.errs, 
       "Overall_Mean_Sqr_Error"=mean(mean.sqr.errs),
       "Std_Mean_Sqr_Error"=sd(mean.sqr.errs))
}
kfold.cval.reg.iter=function(k,df,fold){
  tr.ids=!fold %in% c(k) 
  cat("------------------------") 
  cat(tr.ids) 
  test.ids=fold %in% c(k) 
  mod=lm(Resultado ~., data = df[tr.ids,])
  pred=predict(mod, df[test.ids,]) 
  df[,'Resultado'] <- as.numeric(as.character(df[,'Resultado']))
  sqr.errs=(pred - df[test.ids,"Resultado"])^2
  mean(sqr.errs)
}
y <- file.choose()
encuestas <- read.table(y,sep=";", header = TRUE)
library(e1071)
Probabilidades <- naiveBayes(Resultado ~., data=conjunto_datos[-1])
set.seed(120)
Prediccion <- predict (Probabilidades , encuestas[,-20]); 
encuestas$Prediccion_resultado <- Prediccion
encuestas
write.csv(encuestas,"C:/Users/wilo-/Desktop/encuestas_predecidas.csv")
K <- file.choose()
Datos_encuestas <- read.table(K,sep=",", header = TRUE)
Datos_encuestas
Datos_encuestas$Prediccion_resultado <- factor(ifelse(Datos_encuestas$Prediccion_resultado =="1", "NO", Datos_encuestas$Prediccion_resultado))
Datos_encuestas
write.csv(Datos_encuestas,"C:/Users/wilo-/Desktop/encuestas_predecidas.csv")
Datos_encuestas
tablamatrix <- confusionMatrix(Prediccion,conjunto_datos[["Resultado"]])
tablamatrix
plot(tablamatrix[["table"]])
