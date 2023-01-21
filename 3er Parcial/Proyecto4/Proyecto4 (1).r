# -*- coding: utf-8 -*-
#Program : Proyecto 4
#Autor: Garcia De Arcos Jose Angel Eduardo ESCOM Student 
#course : Analitica y visualizacion de datos
#Created on Nov 06  2022

#importacion de Paqueterias y librerias

install.packages("readr")
library(readr)

#Ruta de archivo mas generacion de la matriz
SelectArchive <- file.choose()
print(SelectArchive)
lecturaDatos <- read.csv("C:\\Users\\Angel\\OneDrive\\Documents\\ProyectosEnR\\datos.csv")



#Calculo de media aritmetica
  VentasMedia <- mean(lecturaDatos[,2]) 
  BeneficiosMedia <- mean(lecturaDatos[,3])
  BienesMedia <- mean(lecturaDatos[,4])
  
#Diferencia de Medias
  VentasDM <- c()
  BeneficiosDM<- c()
  BienesDM <- c()
  
  for(ValuesDM in 1:10){
    ProcessDMVentas <- lecturaDatos[ValuesDM,2] - VentasMedia
    ProcessDMBeneficios <- lecturaDatos[ValuesDM,3] - BeneficiosMedia
    ProcessDMBienes <- lecturaDatos[ValuesDM,4] - BienesMedia
    VentasDM <- c(VentasDM , ProcessDMVentas)
    BeneficiosDM<- c(BeneficiosDM,ProcessDMBeneficios)
    BienesDM <- c(BienesDM,ProcessDMBienes)
  }
  MatrizDiferenciaMedias <- cbind(VentasDM,BeneficiosDM,BienesDM)
  
#Matriz de Covarianza
  #Ventas * Todos los demas
  MCVentas_Ventas <- cov(MatrizDiferenciaMedias[,1],MatrizDiferenciaMedias[,1])
  MCVentas_Beneficios <- cov(MatrizDiferenciaMedias[,1],MatrizDiferenciaMedias[,2])
  MCVentas_Bienes <- cov(MatrizDiferenciaMedias[,1],MatrizDiferenciaMedias[,3])
  
  ConcatenVentas <- c(MCVentas_Ventas,MCVentas_Beneficios,MCVentas_Bienes)
  #Beneficios * Todos los demas
  
  MCBeneficios_Ventas <-cov(MatrizDiferenciaMedias[,2],MatrizDiferenciaMedias[,1])
  MCBeneficios_Beneficios <- cov(MatrizDiferenciaMedias[,2],MatrizDiferenciaMedias[,2])
  MCBeneficios_Bienes <- cov(MatrizDiferenciaMedias[,2],MatrizDiferenciaMedias[,3])
  ConcatenBeneficios <- c(MCBeneficios_Ventas,MCBeneficios_Beneficios,MCBeneficios_Bienes)
  
  #Bienes * todos los demas
  
  MCBienes_ventas <- cov(MatrizDiferenciaMedias[,3],MatrizDiferenciaMedias[,1])
  MCBienes_Beneficios <- cov(MatrizDiferenciaMedias[,3],MatrizDiferenciaMedias[,2])
  MCBienes_Bienes <- cov(MatrizDiferenciaMedias[,3],MatrizDiferenciaMedias[,3])
  ConcatenBienes <- c(MCBienes_ventas,MCBienes_Beneficios,MCBienes_Bienes)
  #Matriz de covarianza
  MC <- cbind("Ventas" = ConcatenVentas,
              "Beneficios " = ConcatenBeneficios,
              "Bienes" = ConcatenBienes)
  #Inversa de la matriz de Covarianza
  McInversa <- solve(MC)
  #transpuesta de la matriz de diferencia de medias
  TDiferenciaMedias <- t(MatrizDiferenciaMedias)
  #Multiplicacion de La matriz de diferencias medias * la inversa de la matriz de covarianza
  MatrizMultiDM_MCI <- MatrizDiferenciaMedias%*%McInversa
  #Proceso final de obtencion de la distancai de mahalanobis
  
  MatrizProcessFinal1 <- MatrizMultiDM_MCI %*% TDiferenciaMedias
  ValoresDiagonal <- diag(MatrizProcessFinal1)
  MatrizProcessFinal2<- c()
  for(diagValues in 1:10){
    RaizDiagValues <- sqrt(ValoresDiagonal[diagValues])
    MatrizProcessFinal2 <- append(MatrizProcessFinal2,RaizDiagValues)
  }
    MatrizFinal<- cbind(MatrizProcessFinal2)
    colnames(MatrizFinal)<- c("Distancia Mahalanobis")
    row.names(MatrizFinal)<-c("General Motors","Ford","Exxon","IBM","GeneralElectric"
                              ,"Mobil","Philip Morris","Chrysler","DuPont","Texaco")
  
  #Calculo de distancias
     DistanciaFordExxon = MatrizFinal[2,1] - MatrizFinal[3,1]
     DistanciaGeneralIBM = MatrizFinal[1,1] - MatrizFinal[4,1]
     DistanciaPhilipTexaco = MatrizFinal[7,1] - MatrizFinal[10,1]
  #Matriz de distancias Calculadas
     DistanciasDiferenciasCalculadas <- rbind(DistanciaFordExxon,DistanciaGeneralIBM,DistanciaPhilipTexaco)
    colnames(DistanciasDiferenciasCalculadas)<- c("DiferenciaDistancias")