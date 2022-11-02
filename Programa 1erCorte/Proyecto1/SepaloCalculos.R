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
  VirginicaData <- irisData[irisData$variety == "Virginica",]

#Calculos
#Sepalo
  #Setosa
    "Supongamos el Area deriva de la siguiente 
    Formula para el sepal (2*(2(pi) * longitud / 3)))
    Basado en el video del siguiente url para el calculo del area de una flor
    https://www.youtube.com/watch?v=piHcat_oZCk"
    #Declaramos variables iniciales en caso de necesitarlas
    
    #Calculo del area de cada uno de los sepalos basado en la longitud
    for (SepaloSetosaposition1 in 1:50){
      AreaSepaloSetosa = 2*((2*pi*(SetosaData[SepaloSetosaposition1,"sepal.length"]))/3)
      print(paste("El Area del  sepalo para la setosa en la posicion :",SepaloSetosaposition1 , " es de :" ,AreaSepaloSetosa))
    }
    
    #Calculo de la Media aritmetica de las areas
    SepaloSetosaMACAL<- 0
    for (SepaloSetosaposicion2 in 1:50){
      AreaSepaloSetosa2 = 2*((2*pi*(SetosaData[SepaloSetosaposicion2,"sepal.length"]))/3)
      SepaloSetosaMACAL <- (AreaSepaloSetosa2+SepaloSetosaMACAL)
    }
    SepaloSetosaMAResult <- SepaloSetosaMACAL/50
    
    #Calculo de la mediana
    SepaloSetosaMedianaCal = ( 50 +1 /2)
    
    #Calculo de la moda
    
    FqSepaloSetosa <- data.frame(table(SetosaData$sepal.length))
    SepaloSetosaModa<-FqSepaloSetosa[which.max(FqSepaloSetosa$Freq),1]
    
    #Calculo de la Varianza
    
    SepaloSetosaVarCAL2 <- 0
    
    for (SepaloSetosaposicion3 in 1:50){
      AreaSepaloSetosa3 = 2*((2*pi*(SetosaData[SepaloSetosaposicion3,"sepal.length"]))/3)
      SepaloSetosaVarCAL <-  (AreaSepaloSetosa3 - SepaloSetosaMAResult)**2
      SepaloSetosaVarCAL2 <- SepaloSetosaVarCAL2 + SepaloSetosaVarCAL
    }
    SepaloSetosaVarResult <- SepaloSetosaVarCAL2/50
    
    #Calculo de la Desviacion estandar
    
    SepaloSetosaDEResult <- sqrt(SepaloSetosaVarResult)
    
    #Calculo de los maximos y minimo
    DatosMaxMinSepaloSetosa <- data.frame()
    
    for (SepaloSetosaposicion4 in 1: 50){
      AreaSepaloSetosa4 = 2*((2*pi*(SetosaData[SepaloSetosaposicion4,"sepal.length"]))/3)
      DatosMaxMinSepaloSetosa <- rbind(DatosMaxMinSepaloSetosa,AreaSepaloSetosa4)
    }
    SepaloSetosaMax <- max(DatosMaxMinSepaloSetosa)
    SepaloSetosaMin <- min(DatosMaxMinSepaloSetosa)
    
  
  
    #Resultados Impresion
    print("Resultados de operaciones Sepalo - Setosa ")
    print(paste("\n La Media aritmetica del sepalo para la setosa es de :" ,SepaloSetosaMAResult))
    print(paste("\n La mediana del sepalo para la setosa es de: ", SepaloSetosaMedianaCal))
    print(paste("\n La moda del sepalo para la setosa es de : ",SepaloSetosaModa))
    print(paste("\n La Varianza del sepalo para la setosa es de : ",SepaloSetosaVarResult))
    print(paste("\n La desviacion estandar del sepalo para la setosa es de :",SepaloSetosaDEResult))
    print(paste("\n Los Max y min del Sepalo para la setosa es de  max :", SepaloSetosaMax ))
    print(paste("\n Los Max y min del sepalo para la setosa es de min  : ",SepaloSetosaMin))
    
#Calculos
  #Sepalo
    #Versicolor
    "Supongamos el Area deriva de la siguiente 
    Formula para el sepal (2*(2(pi) * longitud / 3)))
    Basado en el video del siguiente url para el calculo del area de una flor
    https://www.youtube.com/watch?v=piHcat_oZCk"
    #Declaramos variables iniciales en caso de necesitarlas
    
    #Calculo del area de cada uno de los sepalos basado en la longitud
    
    for(SepaloVersicolorposition  in 1:50){
      AreaSepaloVersicolor =  2*((2*pi*(VersicolorData[SepaloVersicolorposition,"sepal.length"]))/3)
      print(paste("El Area del  sepalo para la setosa en la posicion :",SepaloVersicolorposition , " es de :" ,AreaSepaloVersicolor))
    }
    
    #Calculo de la media aritmetica de las areas
    SepaloVersicolorMACAL <- 0
    for (SepaloVersicolorposition2 in 1:50) {
      AreaSepaloVersicolor2 =  2*((2*pi*(VersicolorData[SepaloVersicolorposition2,"sepal.length"]))/3)
      SepaloVersicolorMACAL<- SepaloVersicolorMACAL+AreaSepaloVersicolor2
    }
    SepaloVersicolorMAResult <- SepaloVersicolorMACAL /50
    
    
    
    
    #Calculo de la mediana
    SepaloSetosaMedianaCal = ( 50 +1 /2)
    
    #Calculo de la moda
    
    FqSepaloVersicolor <- data.frame(table(VersicolorData$sepal.length))
    SepaloVersicolorModa <- FqSepaloVersicolor [which.max(FqSepaloVersicolor$Freq),1]
    
    #Calculo de la varianza
    
    SepaloVersicolorVarCAL2 <- 0
    
    for (SepaloVersicolorposition3 in 1:50) {
      AreaSepaloVersicolor3 =  2*((2*pi*(VersicolorData[SepaloVersicolorposition3,"sepal.length"]))/3)
      SepaloVersicolorVarCAL <- (AreaSepaloVersicolor3 -  SepaloVersicolorMAResult )**2
      SepaloVersicolorVarCAL2 <-  SepaloVersicolorVarCAL2 + SepaloVersicolorVarCAL
    }
    SepaloVersicolorVarResult <-  SepaloVersicolorVarCAL2 /50
    
    #Calculo de la desviacion estandar
    
    SepaloVersicolorDEResult<- sqrt(SepaloVersicolorVarResult)
    
    #Calculo de maximos y minimos 
    
    DatosMaxMinSepaloVersicolor <- data.frame()
    for (SepaloVersicolorposition4 in 1:50){
      AreaSepaloVersicolor4 = 2*((2*pi*(VersicolorData[SepaloVersicolorposition4,"sepal.length"]))/3)
      DatosMaxMinSepaloVersicolor <- rbind(DatosMaxMinSepaloVersicolor, AreaSepaloVersicolor4)
    }
    SepaloVersicolorMax<- max(DatosMaxMinSepaloVersicolor)
    SepaloVersicolorMin<- min(DatosMaxMinSepaloVersicolor)
    
    #Resultados Impresion
    print("Resultados de operaciones Sepalo - Versicolor")
    print(paste("\n la media aritmetica del sepalo para la versicolor es de :",SepaloVersicolorMAResult))
    print(paste("\n la mediana del sepalo para la versicolor es de : ",SepaloSetosaMedianaCal ))
    print(paste("\n la moda del sepalo para la versicolor es de : ", SepaloVersicolorModa))
    print(paste("\n la varianza del sepalo para la versicolor es de :",SepaloVersicolorVarResult))
    print(paste("\n La Desviacion estadar del sepalo para la versicolor es de :", SepaloVersicolorDEResult))
    print(paste("\n Los Max y min del Sepalo para la Versicolor es de  max :", SepaloVersicolorMax ))
    print(paste("\n Los Max y min del sepalo para la Versicolor es de min  : ",SepaloVersicolorMin))
#Sepalo
  #Vergini
    "Supongamos el Area deriva de la siguiente 
    Formula para el sepal (2*(2(pi) * longitud / 3)))
    Basado en el video del siguiente url para el calculo del area de una flor
    https://www.youtube.com/watch?v=piHcat_oZCk"
    #Declaramos variables iniciales en caso de necesitarlas
    
    #Calculo del area de cada uno de los sepalos basado en la longitud
    for (SepaloVerginiposition1 in 1:50){
      AreaSepaloVirgini = 2*((2*pi*(VirginicaData[SepaloVerginiposition1,"sepal.length"]))/3)
      print(paste("El Area del  sepalo para la setosa en la posicion :",SepaloVerginiposition1 , " es de :" ,AreaSepaloVirgini))
    }
    
    #Calculo de la Media aritmetica de las areas
    SepaloVirginiMACAL<- 0
    for (SepaloVerginiposition2 in 1:50){
      AreaSepaloVirgini2 = 2*((2*pi*(VirginicaData[SepaloVerginiposition2,"sepal.length"]))/3)
      SepaloVirginiMACAL <- (AreaSepaloVirgini2+SepaloVirginiMACAL)
    }
    SepaloVirginiMAResult <- SepaloVirginiMACAL/50
    
    #Calculo de la mediana
    SepaloVirginiMedianaCal = ( 50 +1 /2)
    
    #Calculo de la moda
    
    FqSepalovirgini <- data.frame(table(VirginicaData$sepal.length))
    SepalovirginiModa<-FqSepalovirgini[which.max(FqSepalovirgini$Freq),1]
    
    #Calculo de la Varianza
    
    SepaloVirginiVarCAL2 <- 0
    
    for (SepaloVerginiposition3 in 1:50){
      AreaSepalovirgini3 = 2*((2*pi*(VirginicaData[SepaloVerginiposition3,"sepal.length"]))/3)
      SepaloVirginiVarCAL <-  (AreaSepalovirgini3 - SepaloVirginiMAResult)**2
      SepaloVirginiVarCAL2 <- SepaloVirginiVarCAL2 + SepaloVirginiVarCAL
    }
    SepalovirginiVarResult <- SepaloVirginiVarCAL2/50
    
    #Calculo de la Desviacion estandar
    
    SepaloVirginiDEResult <- sqrt(SepalovirginiVarResult)
    
    #Calculo de los maximos y minimo
    DatosMaxMinSepaloVirgni <- data.frame()
    
    for (SepaloVerginiposition4 in 1: 50){
      AreaSepalovirgini4 = 2*((2*pi*(VirginicaData[SepaloVerginiposition4,"sepal.length"]))/3)
      DatosMaxMinSepaloVirgni <- rbind(DatosMaxMinSepaloVirgni,AreaSepalovirgini4)
    }
    SepaloVirginiMax <- max(DatosMaxMinSepaloVirgni)
    SepalovirginiMin <- min(DatosMaxMinSepaloVirgni)
    
    
    
    #Resultados Impresion
    print("Resultados de operaciones Sepalo - Virgini ")
    print(paste("\n La Media aritmetica del sepalo para la setosa es de :" ,SepaloVirginiMAResult))
    print(paste("\n La mediana del sepalo para la setosa es de: ", SepaloVirginiMedianaCal))
    print(paste("\n La moda del sepalo para la setosa es de : ",SepalovirginiModa))
    print(paste("\n La Varianza del sepalo para la setosa es de : ",SepalovirginiVarResult))
    print(paste("\n La desviacion estandar del sepalo para la setosa es de :",SepaloVirginiDEResult))
    print(paste("\n Los Max y min del Sepalo para la setosa es de  max :", SepaloVirginiMax ))
    print(paste("\n Los Max y min del sepalo para la setosa es de min  : ",SepalovirginiMin))
  