# -*- coding: utf-8 -*-
#Program : Proyecto 1
#Autor: Garcia De Arcos Jose Angel Eduardo ESCOM Student 
#course : Analitica y visualizacion de datos
#Created on Wen Oct 23  2022

#Instalacion de paqueterias necesarias

install.packages("readr")
library(readr)

#Ruta del archivo + Generacion de tablas
#IrisData <- read.csv(file.choose()) #Apagado
irisData <- read.csv("C:\\Users\\Angel\\Downloads\\Iris.csv")
table(irisData$variety)
SetosaData <- irisData[irisData$variety == "Setosa",]
VersicolorData <- irisData[irisData$variety == "Versicolor",]
VirginicaData <- irisData[irisData$variety == "Virginica",]}

#Calculos
#petalo
#setosa
"Supongamos el Area deriva de la siguiente 
    Formula para el sepal (2*(2(pi) * longitud / 4)))
    Basado en el video del siguiente url para el calculo del area de una flor
    https://www.youtube.com/watch?v=piHcat_oZCk"
#Declaramos variables iniciales en caso de necesitarlas
#Calculo del area de cada uno de los petalos basado en la longitud

for(PetaloSetosaPosition1 in 1:50){
  AreaPetaloSetosa = 2*((2*pi*(SetosaData[PetaloSetosaPosition1,"petal.length"]))/4)
  print(paste("El Area del  sepalo para la setosa en la posicion :",PetaloSetosaPosition1 , " es de :" ,AreaPetaloSetosa))
}
#Calculo de la media aritmetica de las areas
PetaloSetosaMACAL<-0
for(PetaloSetosaPosition2 in 1:50 ){
  AreaPetaloSetosa2 = 2*((2*pi*(SetosaData[PetaloSetosaPosition2,"petal.length"]))/4)
  PetaloSetosaMACAL = PetaloSetosaMACAL + AreaPetaloSetosa2
}

PetaloSetosaMAResult <- PetaloSetosaMACAL /50

#Calculo de la mediana
PetaloSetosaMedianaCal = ( 50 +1 /2)

#Calculo de la moda

FqPetaloSetosa <- data.frame(table(SetosaData$petal.length))
PetaloSetosaModa<-FqPetaloSetosa[which.max(FqPetaloSetosa$Freq),1]


#Calculo de la Varianza

petaloSetosaVarCAL2 <- 0

for (PetaloSetosaPosition3 in 1:50){
  AreaPetaloSetosa3 = 2*((2*pi*(SetosaData[PetaloSetosaPosition3,"petal.length"]))/4)
  petaloSetosaVarCAL <-  (AreaPetaloSetosa3 - PetaloSetosaMAResult)**2
  petaloSetosaVarCAL2 <- petaloSetosaVarCAL2 + petaloSetosaVarCAL
}
PetaloSetosaVarResult <- petaloSetosaVarCAL2/50


#Calculo de la Desviacion estandar

PetaloSetosaDEResult <- sqrt(PetaloSetosaVarResult)

#Calculo de los maximos y minimo
DatosMaxMinPetaloSetosa <- data.frame()

for (PetaloSetosaPosition4 in 1: 50){
  AreapetaloSetosa4 = 2*((2*pi*(SetosaData[PetaloSetosaPosition4,"petal.length"]))/4)
  DatosMaxMinPetaloSetosa <- rbind(DatosMaxMinPetaloSetosa,AreapetaloSetosa4)
}
PetaloSetosaMax <- max(DatosMaxMinPetaloSetosa)
PetaloSetosaMin <- min(DatosMaxMinPetaloSetosa)

print("Resultados de operaciones petalo - Setosa ")
print(paste("\n La Media aritmetica del petalo para la setosa es de :" ,PetaloSetosaMAResult))
print(paste("\n La mediana del petalo para la setosa es de: ", PetaloSetosaMedianaCal))
print(paste("\n La moda del petalo para la setosa es de : ",PetaloSetosaModa))
print(paste("\n La Varianza del petalo para la setosa es de : ",PetaloSetosaVarResult))
print(paste("\n La desviacion estandar del petalo para la setosa es de :",PetaloSetosaDEResult))
print(paste("\n Los Max y min del petalo para la setosa es de  max :", PetaloSetosaMax ))
print(paste("\n Los Max y min del petalo para la setosa es de min  : ",PetaloSetosaMin))


#------------------------------------------------------------------------------------------------------------------------------------------------
#versicolor
#Calculo del area de cada uno de los petalos basado en la longitud

for(PetaloVersicolorPosition1 in 1:50){
  AreaPetaloversicolor = 2*((2*pi*(VersicolorData[PetaloVersicolorPosition1,"petal.length"]))/4)
  print(paste("El Area del  sepalo para la Versicolor en la posicion :",PetaloVersicolorPosition1 , " es de :" ,AreaPetaloversicolor))
}

#Calculo de la media aritmetica de las areas
PetaloVersicoloraMACAL<-0
for(PetaloVersicolorPosition2 in 1:50 ){
  AreaPetaloVersicolor2 = 2*((2*pi*(VersicolorData[PetaloVersicolorPosition2,"petal.length"]))/4)
  PetaloVersicoloraMACAL = PetaloVersicoloraMACAL + AreaPetaloVersicolor2
}

PetaloVersicolorMAResult <- PetaloVersicoloraMACAL /50

#Calculo de la mediana
PetaloVersicolorMedianaCal = ( 50 +1 /2)

#Calculo de la moda

FqPetaloversicolor <- data.frame(table(VersicolorData$petal.length))
PetaloversicolorModa<-FqPetaloversicolor[which.max(FqPetaloversicolor$Freq),1]

#Calculo de la Varianza

petaloVersicolorVarCAL2 <- 0

for (PetaloVersicolorPosition3 in 1:50){
  AreaPetaloVersicolor3 = 2*((2*pi*(VersicolorData[PetaloVersicolorPosition3,"petal.length"]))/4)
  petaloVersicolorVarCAL <-  (AreaPetaloVersicolor3 - PetaloVersicolorMAResult)**2
  petaloVersicolorVarCAL2 <- petaloVersicolorVarCAL2 + petaloVersicolorVarCAL
}
PetaloVersicolorVarResult <- petaloVersicolorVarCAL2/50

#Calculo de la Desviacion estandar

PetaloversicolorDEResult <- sqrt(PetaloVersicolorVarResult)

#Calculo de los maximos y minimo
DatosMaxMinPetaloversicolor <- data.frame()

for (PetaloVersicolorPosition4 in 1: 50){
  Areapetaloversicolor4 = 2*((2*pi*(VersicolorData[PetaloVersicolorPosition4,"petal.length"]))/4)
  DatosMaxMinPetaloversicolor <- rbind(DatosMaxMinPetaloversicolor,Areapetaloversicolor4)
}
PetaloversicolorMax <- max(DatosMaxMinPetaloversicolor)
PetaloversicolorMin <- min(DatosMaxMinPetaloversicolor)

print("Resultados de operaciones petalo - Versicolor ")
print(paste("\n La Media aritmetica del petalo para la Versicolor es de :" ,PetaloVersicolorMAResult))
print(paste("\n La mediana del petalo para la Versicolor es de: ", PetaloVersicolorMedianaCal))
print(paste("\n La moda del petalo para la Versicolor es de : ",PetaloversicolorModa))
print(paste("\n La Varianza del petalo para la Versicolor es de : ",PetaloVersicolorVarResult))
print(paste("\n La desviacion estandar del petalo para la Versicolor es de :",PetaloversicolorDEResult))
print(paste("\n Los Max y min del petalo para la Versicolor es de  max :", PetaloversicolorMax ))
print(paste("\n Los Max y min del petalo para la Versicolor es de min  : ",PetaloversicolorMin))

#--------------------------------------------------------------------------------------------------------------------------------------------------
#Virgini
#Calculo del area de cada uno de los petalos basado en la longitud

for(PetaloVirginiPosition1 in 1:50){
  AreaPetalovirgini = 2*((2*pi*(VirginicaData[PetaloVirginiPosition1,"petal.length"]))/4)
  print(paste("El Area del  sepalo para la Virginica en la posicion :",PetaloVirginiPosition1 , " es de :" ,AreaPetalovirgini))
}

#Calculo de la media aritmetica de las areas
PetaloVirginiaMACAL<-0
for(PetalovirginiPosition2 in 1:50 ){
  AreaPetaloVirgini2 = 2*((2*pi*(VirginicaData[PetalovirginiPosition2,"petal.length"]))/4)
  PetaloVirginiaMACAL = PetaloVirginiaMACAL + AreaPetaloVirgini2
}

PetaloVirginiMAResult <- PetaloVirginiaMACAL /50

#Calculo de la mediana
PetaloVirginiMedianaCal = ( 50 +1 /2)

#Calculo de la moda

FqPetalovirgini <- data.frame(table(VirginicaData$petal.length))
PetaloVirginiModa<-FqPetalovirgini[which.max(FqPetalovirgini$Freq),1]

#Calculo de la Varianza

petalovirginiVarCAL2 <- 0

for (PetalovirginiPosition3 in 1:50){
  AreaPetaloVirgini3 = 2*((2*pi*(VirginicaData[PetalovirginiPosition3,"petal.length"]))/4)
  petaloVersicolorVarCAL <-  (AreaPetaloVirgini3 - PetaloVirginiMAResult)**2
  petalovirginiVarCAL2 <- petalovirginiVarCAL2 + petaloVersicolorVarCAL
}
PetaloVirginiVarResult <- petalovirginiVarCAL2/50

#Calculo de la Desviacion estandar

PetalovirginiDEResult <- sqrt(PetaloVirginiVarResult)


#Calculo de los maximos y minimo
DatosMaxMinPetalovirgini <- data.frame()

for (PetaloVirginiPosition4 in 1: 50){
  Areapetalovirgini4 = 2*((2*pi*(VirginicaData[PetaloVirginiPosition4,"petal.length"]))/4)
  DatosMaxMinPetalovirgini <- rbind(DatosMaxMinPetalovirgini,Areapetalovirgini4)
}
PetalVirginiMax <- max(DatosMaxMinPetalovirgini)
PetalovirginiMin <- min(DatosMaxMinPetalovirgini)

print("Resultados de operaciones petalo - Virginica ")
print(paste("\n La Media aritmetica del petalo para la Virginica es de :" ,PetaloVirginiMAResult))
print(paste("\n La mediana del petalo para la Virginica es de: ", PetaloVirginiMedianaCal))
print(paste("\n La moda del petalo para la Virginica es de : ",PetaloVirginiModa))
print(paste("\n La Varianza del petalo para la Virginica es de : ",PetaloVirginiVarResult))
print(paste("\n La desviacion estandar del petalo para la Virginica es de :",PetalovirginiDEResult))
print(paste("\n Los Max y min del petalo para la Virginica es de  max :", PetalVirginiMax ))
print(paste("\n Los Max y min del petalo para la Virginica es de min  : ",PetalovirginiMin))





