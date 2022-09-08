#* @apiTitle Envío de datos desde R
#* @apiDescription Aquí estaran los endpoint
#* de ejemplo para el envío.

#* Eco del input
#* @param msg El mensaje de respuesta
#* @get /echo

function(msg=""){
  list(msg = paste0("El mensaje es: ",msg)) 
}

#* Histograma distribucion normal
#* @serializer png
#* @get /plot 

function(){
  rand <- rnorm(100)
  hist(rand)
}

#* Lectura de dos parametros
#* @param a el priemr parámetro
#* @param b el segundo parámetro
#* post /sum

function(a,b){
  a <- as.numeric(a)
  b <- as.numeric(b)
  list(a=a,
       b=b,
       output=a+b)
}

#* ejemplo serializacion csv
#* @serializer csv
#* @param n numero de filas
#* @get /data

function(n=10){
  head(mtcars,as.numeric(n))
}
