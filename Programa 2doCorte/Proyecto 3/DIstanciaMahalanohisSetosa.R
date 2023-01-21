# -*- coding: utf-8 -*-
#Program : Proyecto 3
#Autor: Garcia De Arcos Jose Angel Eduardo ESCOM Student 
#course : Analitica y visualizacion de datos
#Created on Nov 06  2022

#importacion de Paqueterias y librerias


#Lectura de data
data("iris")
summary(iris)
DataFrameIris<- data.frame(iris)
DataSetosa <- iris[1:50,]  
DataVersicolor <- iris[51:100,]
DataVirginica <-  iris[101:150,]

#DataframeSetosa 
  #Calculo de Media Artimetica
  SetosaSepalLenghtMedia <- mean(DataSetosa[,1])
  SetosaSepalWidthMedia <- mean(DataSetosa[,2])
  SetosaPetalLenghtMedia<- mean(DataSetosa [,3])
  SetosaPetalWidthMedia <- mean(DataSetosa[,4])
  
  #Diferencia de Medias
    #Setosa Diferencia Medias
  
    SetosaSepalLenghtDM <- c()
    SetosaSepalWidthDM <- c()
    SetosaPetalLenghtDM <- c()
    SetosaPetalwidthDM <- c()
    for (i in 1:50) {
      ProcessDMSSL <- DataSetosa[i,1] - SetosaSepalLenghtMedia
      ProcessDMSSW <- DataSetosa[i,2] - SetosaSepalWidthMedia
      ProcessDMSPL <- DataSetosa[i,3] - SetosaPetalLenghtMedia
      ProcessDMSPW <- DataSetosa[i,4] - SetosaPetalWidthMedia
      SetosaSepalWidthDM <- c( SetosaSepalWidthDM , ProcessDMSSW)
      SetosaSepalLenghtDM<-c(SetosaSepalLenghtDM , ProcessDMSSL)
      SetosaPetalLenghtDM <- c (SetosaPetalLenghtDM , ProcessDMSPL)
      SetosaPetalwidthDM <- c(SetosaPetalwidthDM , ProcessDMSPW)
    }
    
    dfDiferenciaMedias <- cbind(SetosaSepalLenghtDM,SetosaSepalWidthDM,SetosaPetalLenghtDM , SetosaPetalwidthDM)
    
  #Matriz de covarianza
    #Sepal Lenght * Todos los demas
    MC_SL_SL <- cov(dfDiferenciaMedias[,1], dfDiferenciaMedias[,1])
    MC_SL_SW <- cov(dfDiferenciaMedias[,1] , dfDiferenciaMedias [,2])
    MC_SL_PL <- cov(dfDiferenciaMedias[,1] , dfDiferenciaMedias [,3])
    MC_SL_PW <- cov(dfDiferenciaMedias[,1] , dfDiferenciaMedias [,4])
    ConcatenMCSLPW <- c(MC_SL_SL ,MC_SL_SW,MC_SL_PL,MC_SL_PW )
    #Sepal width * Todos los demas
    MC_SW_SL <- cov(dfDiferenciaMedias[,2] , dfDiferenciaMedias [,1])
    MC_SW_SW <- cov(dfDiferenciaMedias[,2] , dfDiferenciaMedias [,2])
    MC_SW_PL <- cov(dfDiferenciaMedias[,2] , dfDiferenciaMedias [,3])
    MC_SW_PW <- cov(dfDiferenciaMedias[,2] , dfDiferenciaMedias [,4])
    ConcatenMCSWPW <- c(MC_SW_SL,MC_SW_SW,MC_SW_PL,MC_SW_PW)
    #Petalo lenght * Todos los demas
    MC_PL_SL <- cov(dfDiferenciaMedias[,3] , dfDiferenciaMedias [,1])
    MC_PL_SW <- cov(dfDiferenciaMedias[,3] , dfDiferenciaMedias [,2])
    MC_PL_PL <- cov(dfDiferenciaMedias[,3] , dfDiferenciaMedias [,3])
    MC_PL_PW <- cov(dfDiferenciaMedias[,3] , dfDiferenciaMedias [,4])
    ConcatenMCSPLPW <- c(MC_PL_SL,MC_PL_SW,MC_PL_PL,MC_PL_PW)
    #Petalo width * Todos los demas
    MC_PW_SL <- cov(dfDiferenciaMedias[,4] , dfDiferenciaMedias [,1])
    MC_PW_SW <- cov(dfDiferenciaMedias[,4] , dfDiferenciaMedias [,2])
    MC_PW_PL <- cov(dfDiferenciaMedias[,4] , dfDiferenciaMedias [,3])
    MC_PW_PW <- cov(dfDiferenciaMedias[,4] , dfDiferenciaMedias [,4])
    COncatenMCSPWPW <- c(MC_PW_SL,MC_PW_SW,MC_PW_PL,MC_PW_PW)
    #Dataframe de covarianza
    MC <- cbind("SLMC" = ConcatenMCSLPW ,
                     "SWMC" = ConcatenMCSWPW ,
                     "PLMC" = ConcatenMCSPLPW ,
                     "PWMC" = COncatenMCSPWPW)
    #Inversa de la matriz de covarianza
    MCInversa <- solve(MC)
    #Transpues de la matriz de la diferencia de medias
    TranspuestaMatrixDM <- t(dfDiferenciaMedias)
    #Multiplicacion de La matriz de diferencias medias * la inversa de la matriz de covarianza
    MatrizMultiplicacionDM_MCInversa <- dfDiferenciaMedias%*%MCInversa
    
    #MatrizDistanciaMahalanobis  Setosa
    MDistanciaMahalanobisSetosa <- MatrizMultiplicacionDM_MCInversa %*% TranspuestaMatrixDM
    #Matriz final de la distancia de mahalanobis calculos e implementacion
    
    valoresdiagonalesMDM <- diag(MDistanciaMahalanobisSetosa)
    MDistanciaMahalanobisSetosaFInal <- c()
    for (a in 1:50){
      RaizValoresdiagonal <- sqrt(valoresdiagonalesMDM[a])
      MDistanciaMahalanobisSetosaFInal <- append(MDistanciaMahalanobisSetosaFInal , RaizValoresdiagonal)
    }
    MatrizFinalDistancias <-  cbind(MDistanciaMahalanobisSetosaFInal)
  #Plot values Matriz Final Distancia Setosa
    plot(MatrizFinalDistancias)
    
    
#------------------------------------------------------------------------------------------------    
    #Dataframe Versicolor
    #Calculo de Media aritmetica
    VersicolorSepalLenghtMedia <- mean(DataVersicolor[,1])
    VersicolorSepalWidthMedia <- mean(DataVersicolor[,2])
    VersicolorPetalLenghtMedia <- mean(DataVersicolor[,3])
    VersicolorPetalWidthMedia <- mean(DataVersicolor[,4])
    
    #Diferencia de Medias Versicolor
    #Versicolor 
    VersicolorSepalLenghtDM <-c()
    VersicolorSepalWidthDM <-c()
    VersicolorPetalLenghtDM <-c()
    VersicolorPetalWidthDM <-c()
    h = 1
    for(h in 1:50){
      ProcessDMVersicolorSepalLenght <- DataVersicolor[(h),1] - VersicolorSepalLenghtMedia
      ProcessDMVersicolorSepalWidth <- DataVersicolor[(h),2] - VersicolorSepalWidthMedia
      ProcessDMVersicolorPetalLenght <- DataVersicolor[(h),3] - VersicolorPetalLenghtMedia
      ProcessDMVersicolorPetalWidth <- DataVersicolor[(h),4] -  VersicolorPetalWidthMedia
      VersicolorSepalLenghtDM <- c( VersicolorSepalLenghtDM , ProcessDMVersicolorSepalLenght )
      VersicolorSepalWidthDM <- c(VersicolorSepalWidthDM , ProcessDMVersicolorSepalWidth)
      VersicolorPetalLenghtDM <- c(VersicolorPetalLenghtDM , ProcessDMVersicolorPetalLenght )
      VersicolorPetalWidthDM <- c(VersicolorPetalWidthDM , ProcessDMVersicolorPetalWidth)
    }
    
    MatrizDiferenciaMediasVersicolor <- cbind(VersicolorSepalLenghtDM,VersicolorSepalWidthDM,VersicolorPetalLenghtDM,VersicolorPetalWidthDM )
    
    #Matriz de Covarianza
    #Versicolor Sepal lenght * Todos los demas
    MCVersicolor_SL_SL <- cov(MatrizDiferenciaMediasVersicolor[,1],MatrizDiferenciaMediasVersicolor[,1])
    MCVersicolor_SL_SW <- cov(MatrizDiferenciaMediasVersicolor[,1],MatrizDiferenciaMediasVersicolor[,2])
    MCVersicolor_SL_PL <- cov(MatrizDiferenciaMediasVersicolor[,1],MatrizDiferenciaMediasVersicolor[,3])
    MCVersicolor_SL_PW <- cov(MatrizDiferenciaMediasVersicolor[,1],MatrizDiferenciaMediasVersicolor[,4])
    
    ConcatenMCVersicolorSL <- c(MCVersicolor_SL_SL,MCVersicolor_SL_SW,MCVersicolor_SL_PL ,MCVersicolor_SL_PW)
    
    #Versicolor Sepal width * Todos los demas
    
    MCVersicolor_SW_SL <- cov(MatrizDiferenciaMediasVersicolor[,2],MatrizDiferenciaMediasVersicolor[,1])
    MCVersicolor_SW_SW <- cov(MatrizDiferenciaMediasVersicolor[,2],MatrizDiferenciaMediasVersicolor[,2])
    MCVersicolor_SW_PL <- cov(MatrizDiferenciaMediasVersicolor[,2],MatrizDiferenciaMediasVersicolor[,3])
    MCVersicolor_SW_PW <- cov(MatrizDiferenciaMediasVersicolor[,2],MatrizDiferenciaMediasVersicolor[,4])
    
    ConcatenMCVersicolorSW <- c ( MCVersicolor_SW_SL ,MCVersicolor_SW_SW , MCVersicolor_SW_PL ,MCVersicolor_SW_PW)
    
    #Versicolor Petal Length * Todos los demas
    
    MCVersicolor_PL_SL <- cov(MatrizDiferenciaMediasVersicolor[,3],MatrizDiferenciaMediasVersicolor[,1])
    MCVersicolor_PL_SW <- cov(MatrizDiferenciaMediasVersicolor[,3],MatrizDiferenciaMediasVersicolor[,2])
    MCVersicolor_PL_PL <- cov(MatrizDiferenciaMediasVersicolor[,3],MatrizDiferenciaMediasVersicolor[,3])
    MCVersicolor_PL_PW <- cov(MatrizDiferenciaMediasVersicolor[,3],MatrizDiferenciaMediasVersicolor[,4])
    
    ConcatenMCVersicolorPL <- c(MCVersicolor_PL_SL , MCVersicolor_PL_SW, MCVersicolor_PL_PL , MCVersicolor_PL_PW)
    
    #Versicolor Petal width * Todos los demas
    
    MCVersicolor_PW_Sl <- cov(MatrizDiferenciaMediasVersicolor[,4],MatrizDiferenciaMediasVersicolor[,1])
    MCVersicolor_PW_SW <- cov(MatrizDiferenciaMediasVersicolor[,4],MatrizDiferenciaMediasVersicolor[,2])
    MCVersicolor_PW_PL <- cov(MatrizDiferenciaMediasVersicolor[,4],MatrizDiferenciaMediasVersicolor[,3])
    MCVersicolor_PW_PW <- cov(MatrizDiferenciaMediasVersicolor[,4],MatrizDiferenciaMediasVersicolor[,4])
    
    ConcatenMCVersicolorPW <- c(MCVersicolor_PW_Sl,MCVersicolor_PW_SW,MCVersicolor_PW_PL,MCVersicolor_PW_PW)
    
    #Dataframe de Covarianza
    
    MatrizCovarianzaVersicolor <- cbind("SLMC" = ConcatenMCVersicolorSL ,
                                        "SWMC" = ConcatenMCVersicolorSW ,
                                        "PLMC" = ConcatenMCVersicolorPL ,
                                        "PWMC" = ConcatenMCVersicolorPW)
    #Inversa de la matriz de Covarianza
    InversaMatrizCovarianzaVersicolor <- solve(MatrizCovarianzaVersicolor)
    #Transpuesta de la Matriz de la diferencia de Medias Versicolor
    TranspuestaMatrizDiferenciaMediasVersicolor <- t(MatrizDiferenciaMediasVersicolor)
    #Multiplicacion de la Matriz de diferencia de medias * la inversa de la matriz de covarianza
    MatrizMultiplicacionVersicolorDM_IMCV <- MatrizDiferenciaMediasVersicolor %*% InversaMatrizCovarianzaVersicolor
    #Proceso de obtencion de la matriz de Ditancia Mahalanobis Versicolor
    
    MatrizProcessFinal1Versicolor <-  MatrizMultiplicacionVersicolorDM_IMCV %*% TranspuestaMatrizDiferenciaMediasVersicolor
    
    ValoresDiagonalesVersicolorMPF1V <- diag(MatrizProcessFinal1Versicolor)
    
    MatrizProcessFinal2Versicolor <- c()
    for(diagvalues in 1:50){
      RaizValoresDiagonalVersicolor <- sqrt(ValoresDiagonalesVersicolorMPF1V[diagvalues])
      MatrizProcessFinal2Versicolor <- append(MatrizProcessFinal2Versicolor , RaizValoresDiagonalVersicolor )
    }
    
    MatrizFinalDistanciaMahalanobisVersicolor <- cbind(MatrizProcessFinal2Versicolor)
    #Plot values Matriz Final Distancia Versicolor   
    plot(MatrizFinalDistanciaMahalanobisVersicolor)
#------------------------------------------------------------
    #DataFrame Virginica
    #Calculo de Media Aritmetica
    VirginicaSepalLenghtMedia <- mean(DataVirginica[,1])
    VirginicaSepalWidthMedia <- mean(DataVirginica[,2])
    VirginicaPetalLenghtMedia <- mean(DataVirginica[,3])
    VirginicaPetalWidthMedia <- mean(DataVirginica[,4])
    
    #Diferencia de Medias Virginica
    
    VirginicaSepalLenghtDM <- c()
    VirginicaSepalWidthDM <- c()
    VirginicaPetalLenghtDM <- c()
    VirginicaPetalWidthDM <- c()
    
    for(PositionDM in 1:50){
      ProcessVirginicaDMSepalLenght <- DataVirginica[PositionDM,1] - VirginicaSepalLenghtMedia
      ProcessVirginicaDMSepalWidth <- DataVirginica[PositionDM,2] - VirginicaSepalWidthMedia
      ProcessVirginicaDMPetalLenght <- DataVirginica[PositionDM,3] - VirginicaPetalLenghtMedia
      ProcessVirginicaDMPetalWidth <- DataVirginica[PositionDM,4] - VirginicaPetalWidthMedia
      VirginicaSepalLenghtDM <- c(VirginicaSepalLenghtDM,ProcessVirginicaDMSepalLenght)
      VirginicaSepalWidthDM <- c(VirginicaSepalWidthDM,ProcessVirginicaDMSepalWidth)
      VirginicaPetalLenghtDM <- c(VirginicaPetalLenghtDM,ProcessVirginicaDMPetalLenght)
      VirginicaPetalWidthDM <- c(VirginicaPetalWidthDM , ProcessVirginicaDMPetalWidth )
    }
    
    MatrizDiferenciaMediasVirginica <- cbind(VirginicaSepalLenghtDM,VirginicaSepalWidthDM,VirginicaPetalLenghtDM,VirginicaPetalWidthDM)
    
    #Matriz de Covarianza
    #Virginica Sepal Lenght * Todos los demas
    MatrizCovarianzaVirginica_SL_SL <- cov(MatrizDiferenciaMediasVirginica[,1],MatrizDiferenciaMediasVirginica[,1])
    MatrizCovarianzaVirginica_SL_SW <- cov(MatrizDiferenciaMediasVirginica[,1],MatrizDiferenciaMediasVirginica[,2])
    MatrizCovarianzaVirginica_SL_PL <- cov(MatrizDiferenciaMediasVirginica[,1],MatrizDiferenciaMediasVirginica[,3])
    MatrizCovarianzaVirginica_SL_PW <- cov(MatrizDiferenciaMediasVirginica[,1],MatrizDiferenciaMediasVirginica[,4])
    
    ConcatenVirginicaSL <- c(MatrizCovarianzaVirginica_SL_SL,MatrizCovarianzaVirginica_SL_SW,MatrizCovarianzaVirginica_SL_PL,MatrizCovarianzaVirginica_SL_PW)
    
    #Virginica Sepal width * Todos los demas
    
    MatrizCovarianzaVirginica_SW_SL <- cov(MatrizDiferenciaMediasVirginica[,2],MatrizDiferenciaMediasVirginica[,1])
    MatrizCovarianzaVirginica_SW_SW <- cov(MatrizDiferenciaMediasVirginica[,2],MatrizDiferenciaMediasVirginica[,2])
    MatrizCovarianzaVirginica_SW_PL <- cov(MatrizDiferenciaMediasVirginica[,2],MatrizDiferenciaMediasVirginica[,3])
    MatrizCovarianzaVirginica_SW_PW <- cov(MatrizDiferenciaMediasVirginica[,2],MatrizDiferenciaMediasVirginica[,4])
    
    ConcatenVirginicaSW <- c(MatrizCovarianzaVirginica_SW_SL,MatrizCovarianzaVirginica_SW_SW,MatrizCovarianzaVirginica_SW_PL, MatrizCovarianzaVirginica_SW_PW)
    
    #Virginica Petal lenght * Todos los demas
    
    MatrizCovarianzaVirginica_PL_SL <-cov(MatrizDiferenciaMediasVirginica[,3],MatrizDiferenciaMediasVirginica[,1])
    MatrizCovarianzaVirginica_PL_SW <-cov(MatrizDiferenciaMediasVirginica[,3],MatrizDiferenciaMediasVirginica[,2])
    MatrizCovarianzaVirginica_PL_PL <- cov(MatrizDiferenciaMediasVirginica[,3],MatrizDiferenciaMediasVirginica[,3])
    MatrizCovarianzaVirginica_PL_Pw <- cov(MatrizDiferenciaMediasVirginica[,3],MatrizDiferenciaMediasVirginica[,4])
    
    ConcatenVirginicaPL <- c(MatrizCovarianzaVirginica_PL_SL,MatrizCovarianzaVirginica_PL_SW,MatrizCovarianzaVirginica_PL_PL,MatrizCovarianzaVirginica_PL_Pw)
    
    #Virginica Petal width * todos los demas
    
    MatrizCovarianzaVirginica_PW_SL <- cov(MatrizDiferenciaMediasVirginica[,4],MatrizDiferenciaMediasVirginica[,1])
    MatrizCovarianzaVirginica_PW_SW <- cov(MatrizDiferenciaMediasVirginica[,4],MatrizDiferenciaMediasVirginica[,2])
    MatrizCovarianzaVirginica_PW_PL <- cov(MatrizDiferenciaMediasVirginica[,4],MatrizDiferenciaMediasVirginica[,3])
    MatrizCovarianzaVirginica_PW_PW <- cov(MatrizDiferenciaMediasVirginica[,4],MatrizDiferenciaMediasVirginica[,4])
    
    ConcatenVirginicaPW <- c(MatrizCovarianzaVirginica_PW_SL,MatrizCovarianzaVirginica_PW_SW,MatrizCovarianzaVirginica_PW_PL,MatrizCovarianzaVirginica_PW_PW)
    
    #Matriz de la covarianza
    
    MatrizCovarinzaVirginica <- cbind("SLMC" =  ConcatenVirginicaSL,
                                      "SWMC" =  ConcatenVirginicaSW,
                                      "PLMC" = ConcatenVirginicaPL ,
                                      "PWMC" = ConcatenVirginicaPW)
    
    #Inversa de la matriz de covarianza
    
    InversaMatrizCovarianzaVirginica <- solve(MatrizCovarinzaVirginica)
    #Transpuesta de la matriz de la diferencia de medias
    TranspuestaMatrizDMVirginica <- t(MatrizDiferenciaMediasVirginica)
    #Multiplicacion de La matriz de diferencias medias * la inversa de la matriz de covarianza
    MatrizMultiplicacionVersicolor_DM_IMCV <- MatrizDiferenciaMediasVirginica %*% InversaMatrizCovarianzaVirginica
    
    #Proceso de obtencion de la matriz de Ditancia Mahalanobis Virginica
    
    MatrizProcessFinal1Virginica <- MatrizMultiplicacionVersicolor_DM_IMCV %*% TranspuestaMatrizDMVirginica
    
    ValoresDiagonalesVirginicaMPF1V <- diag(MatrizProcessFinal1Virginica)
    
    MatrizProcessFinal2Virginica <- c()
    for(diagvalues2 in 1:50){
      RaizValoresDiagonalVirginica <- sqrt(ValoresDiagonalesVirginicaMPF1V[diagvalues2])
      MatrizProcessFinal2Virginica <- append(MatrizProcessFinal2Virginica ,RaizValoresDiagonalVirginica )
      
    }
    
    MatrizFinalDistanciaMahalanobisVirginica <- cbind(MatrizProcessFinal2Virginica)
    
    #Plot Values Matriz Final Distancia Virginica
    plot(MatrizFinalDistanciaMahalanobisVirginica)
    
    
    
    
    
    MatrizALLValuesDistance <- cbind ( "Distancia Mahalanobis setosa" = MatrizFinalDistancias ,
                                       "Distancia Mahalanobis Versicolor" = MatrizFinalDistanciaMahalanobisVersicolor ,
                                       "Distancia Mahalanobis Virginica " = MatrizFinalDistanciaMahalanobisVirginica)
    