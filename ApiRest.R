#* @apiTitle Prueba de Apis usando Plumber
#* @apiDescription test de varias apis con Plumber en R.
#* @apiHost /servicios

#* @filter cors
cors <- function(res) {
  
  res$setHeader("Access-Control-Allow-Origin", "*") # Or whatever
  res$setHeader("Access-Control-Allow-Methods", "*")
  res$setHeader("Access-Control-Allow-Headers", "*")
  plumber::forward()
}

#* funcion para obtener un csv
#* @serializer csv
#* @get /data_csv
function(){
  data.frame(x=seq(10), letters=letters[1:10])
}


#* Retornar el mensaje
#* @param text El mensaje que va a recibir el endPoint
#* @get /msg
function(text = ""){
  list("h"=text)
}


#* FunciÃ³n para obtener un plot se serializa en png para indicar que devolvera un ong
#* @serializer png
#* @get /plot
function(){
  r <- rnorm(10)
  hist(r)
}

#* Suma de dos nÃºmeros
#* @param a primer nÃºmero
#* @param b segundo nÃºmero
#* @serializer unboxedJSON
#* @post /suma
function(a, b){
  a<-as.numeric(a)
  b<-as.numeric(b)
  list(a=a,b=b,suma=a+b)
}


#* Return the sum of two numbers
#* @param a The first number to add
#* @param b The second number to add
#* @post /sum
function(a, b) {
  print(paste(a, b, sep = " - "))
}




#* VisualizaciÃ³n de datos faltantes
#* @serializer png
#* @param data valores para la visualizaciÃ³n
#* @param headr encabezados
#* @param name titulo para la imagen a retornar
#* @get /imagmiss
imagmiss <-function(data, headr, name = "Table")
{
  
  headr <- jsonlite::fromJSON(headr);
  
  data <- jsonlite::fromJSON(data);
  data.table::as.data.table(data);
  
  data<-data[headr];
  
  for(fil in 1:length(data[0,])){
    mintable<-data[fil,]
    for(revix in 1:length(mintable)){
      if(!is.null(mintable[revix]) && is.na(mintable[revix]))
      {
        data[fil, revix]<-as.numeric(mintable[revix]);
      }
    }
  }
  
  #Function to create a graph of the observations of the dataset
  #leaving white lines where data is missing.
  #The main idea is to use the original dataset to create 
  #a temporary dataset containing 1 if a value is found or
  #0 is the value is missing. The temporary data set is graphed by column
  #changing color for each feature and leaving a blank horizontal line if
  #value is missing.
  
  #Uses the R function image from the base library.
  
  #data:  the dataset
  #name:the name of the dataset as desired in the graph title
  
  ncol=dim(data)[2]
  nrow=dim(data)[1]
  
  xaxis=colnames(data)
  #identificar las columnas categoricas
  exept<-c();
  for(index in 1:dim(medataset)[2])
  {
    if(is.character(medataset[1,index]) || is.factor(medataset[1,index]))
    {
      exept[length(exept)+1]<-index;
    }
  }
  cat("Columnas eliminadas: ",exept,"\n");
  
  data = as.matrix(data)
  #no incluir las columnas categoricas.
  if(length(exept)>0)
  {
    xaxis = xaxis[-exept]
    ticks = 1:(dim(data)[2] - length(exept))
    data = data[, -exept]
    ncol1=ncol-1;
  }else{
    ticks = 1:(dim(data)[2])
    ncol1=ncol;
  }
  #seleccion de colores segun si existen datos ausentes.
  if(sum(is.na(medataset))>0)
  {
    mis_colores <- c(0,topo.colors(100));
  }else{
    mis_colores <- c(topo.colors(101));
  }
  cat(length(exept),":taka<-",length(exept)>0,"\n")
  
  
  cat("Report on missing values for ",name,":\n")
  cat("\nNumber of missing values overall: ")
  cat(sum(is.na(data)))
  cat("\nPercent of missing values overall: ")
  cat((sum(is.na(data))/(dim(data)[1]*dim(data)[2]))*100)
  cat("\nFeatures with missing values (percent): \n")
  print(colSums(is.na(data))[colSums(is.na(data))!=0]/dim(data)[1]*100)
  #cat("\n",length(which(colSums(is.na(data))!=0)))
  cat("\nPercent of features with missing values: ")
  cat(length(which(colSums(is.na(data))!=0))/dim(data)[2]*100)
  cat("\nNumber of instances with missing values: ")
  cat(length(which(rowSums(is.na(data))!=0, arr.ind=TRUE)))
  cat("\nPercent of instances with missing values: ")
  cat(length(which(rowSums(is.na(data))!=0))/dim(data)[1]*100)
  cat("\n")
  data[which(data!="NA")]=1
  data[-which(data!="NA")]=0
  
  
  #ncol1=ncol
  
  for(i in 1:ncol1)
  {
    data[data[,i]!=0,i]=i
  }
  
  x=1:ncol1
  y=1:nrow
  data=apply(data,1,as.numeric)
  cat("columnas",x);
  
  graph.title=paste("Distribution of missing values by variable for - ",name)
  
  #whitex<-gray.colors(1, start =1, end = 0)
  image(x,y,data,col=mis_colores,xlab="features",ylab="instances",axes=FALSE,main=(graph.title),cex.main=.7)
  axis(1,labels=xaxis,at=ticks, las=2)
}




#* VisualizaciÃ³n de datos faltantes
#* @param data valores para la visualizaciÃ³n
#* @param headr encabezados
#* @param name titulo para la imagen a retornar
#* @serializer unboxedJSON
#* @post /imagmisspost
imagmiss2 <-function(data, headr, name = "Table")
{
  
  print(data)
  print(class(data))
  print(headr)
  print(length(headr))
  print(name)
  print(class(name))
  #headr <- jsonlite::fromJSON(headr);
  print(headr)
  print(":3")
  #data <- jsonlite::fromJSON(data);
  #data.table::as.data.table(data);
  
  data<-data[,headr];
  
  tmp<-tempfile(fileext = ".png")
  ggsave(tmp, ggplot(data.frame(x = c(data[,headr[1]]), y = c(data[,headr[2]])), aes(x, y)) + geom_point());
  base64<-dataURI(file = tmp, mime = "image/png")
  file.remove(tmp);
  
  list(base64=base64)
