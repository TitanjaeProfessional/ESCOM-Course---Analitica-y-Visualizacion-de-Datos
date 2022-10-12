"Instituto Politecnico Nacional
Escuela Superior de Computo
Garcia de Arcos Jose Angel Eduardo 
Analitica y visualizacion de datos
Set IRIS Question solution"

#------------------------------------------------------------------------------

data("iris") #cargamos el dataset
names(iris) #informacion de las columnas 
table(iris$Species) #informacion con respecto a las especies existentes
summary(iris) # imprime el minimo , medio y maximo del sepal y el petal (length and width)

#--------------------------------------------------------------------------------------------------------

#Pregunta numero 1 (Programa de error)

setosainfo <- iris[iris$Species == "setosa",]  #Mandamos a llamar los valores unicamente correspondientes a setosa
print(setosainfo)
versicolorinfo <- iris[iris$Species == "versicolor",]
print(versicolorinfo)
virginicainfo<- iris[iris$Species == "virginica",]
print(virginicainfo)
summary(setosainfo) # Usamos esta funcion para conocer el min y maximo de cada uno de los valores correspondientes a setosa
summary(versicolorinfo)# Usamos esta funcion para conocer el min y maximo de cada uno de los valores correspondientes a versicolor
summary(virginicainfo)# Usamos esta funcion para conocer el min y maximo de cada uno de los valores correspondientes a versicolor

#solicitamos los datos para hacer la evaluacion



SL <- readline (prompt = "Ingrese el valor de Sepal lenght : ")
SW <- readline (prompt = "Ingrese el valor de Sepal Width :")
PL <- readline (prompt = "Ingrese el valor de Petal length : ")
PW <- readline (prompt = "Ingrese el valor de Petal Width :")

print(paste("Los valores introducido al respecto son  SL :",SL,"SW :",SW,"PL :",PL,"PW :",PW))

if ( ( (SL>= 4.3) && (SL<= 5.8)) && ((SW>= 2.3) && (SW<= 4.4)) && ((PL>= 1.0) && (PL<= 1.9)) && ((PW>= 0.1) && (PW<= 0.6)) ){
  print("Es una setosa")
} else if ( ( (SL>= 4.9) && (SL<= 7.0)) && ((SW>= 2.0) && (SW<= 3.4)) && ((PL>= 3.0) && (PL<= 5.1)) && ((PW>= 1.0) && (PW<= 1.8)) ){
  print("Es un versicolor")
}else if ( ( (SL>= 4.9) && (SL<= 7.9)) && ((SW>= 2.2) && (SW<= 3.8)) && ((PL>= 4.5) && (PL<= 6.9)) && ((PW>= 1.4) && (PW<= 2.5)) ){
  print("Es una virginica")
} else {
  print("Error")
}
  
 #-------------------------------------------------------------------------------------

#Pregunta numero 2

RedondeoIRISinfoSL <- round(iris$Sepal.Length)#Redonde de la columna Sepal lenght
RedondeoIRISinfoSW <- round(iris$Sepal.Width)#Redondeo de la columna Sepal width
RedondeoIRISinfoPL <- round(iris$Petal.Length)#Redondeo de la columna Petal Length
RedondeoIRISinfoPW <- round(iris$Petal.Width)#Redondeo de la columna Petal Width

MatrizIRIS <- cbind(RedondeoIRISinfoSL,RedondeoIRISinfoSW,RedondeoIRISinfoPL,RedondeoIRISinfoPW)
MatrizIRIS # creacion e impresion de una matriz que une los redondeo por columna
PLPlot <- iris$Petal.Length #Guardamos el valor de la columna Petal Length
PWPlot <- iris$Petal.Width  #Guardamos el valor de la columna Petal Width
SLPlot<- iris$Sepal.Length #Guardamos el valor de la columna Sepal lenght
SWPlot<- iris$Sepal.Width  #Guardamos el valor de la columna Sepal width

plot(PLPlot , PWPlot) #Ploteo de la columna Petal lenght y Petal Width sin redondeo
plot(RedondeoIRISinfoPL, RedondeoIRISinfoPW)#Ploteo de la columna Petal lenght y Petal Width con redondeo
plot(SLPlot,SWPlot) # Ploteo de la columna Sepal lenght y sepal width sin redondeo
plot(RedondeoIRISinfoSL, RedondeoIRISinfoSW)  # Ploteo de la columna Sepal lenght y sepal width con redondeo

#--------------------------------------------------------------------------------
#Pregunta Numero 3 y 4

PetalCor <-  cor(PLPlot,PWPlot,method ="pearson") #Calculamos la correlacion entre el largo y el ancho del petalo
PetalCor
SetalCor <-  cor(SLPlot,SWPlot,method ="pearson") #Calculamos la correlacion entre el largo y el ancho del Setalo
SetalCor

#-------------------------------------------------------------------------------------
