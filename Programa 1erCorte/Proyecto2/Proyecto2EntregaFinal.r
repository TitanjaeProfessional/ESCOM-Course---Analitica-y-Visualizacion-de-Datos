# -*- coding: utf-8 -*-
#Program : Proyecto 2
#Autor: Garcia De Arcos Jose Angel Eduardo ESCOM Student 
#course : Analitica y visualizacion de datos
#Created on Wen Oct 23  2022

#Instalacion de paqueteria necesaria
install.packages("pracma")
library(datasets)
library("dplyr")

#Desarrollo de practica
data(iris)
summary(iris)
#Problema 1
DataFrameIris<- data.frame(iris)
DataSetosa <- iris[1:50,]  
DataVersicolor <- iris[51:100,]
DataVirginica <-  iris[101:150,]   
#Desarrollo de la norma euclidiana
EuclidianaSepaloSetosa <- sqrt(sum(DataSetosa["Sepal.Length"]*DataSetosa["Sepal.Width"]^2))
EuclidianaSepaloVersicolor <- sqrt(sum(DataVersicolor["Sepal.Length"]*DataVersicolor["Sepal.Width"]^2))
EuclidanaSepaloVirginica <- sqrt(sum(DataVirginica["Sepal.Length"]*DataVirginica["Sepal.Width"]^2))

print(paste("El Area del Sepalo para la setosa :",EuclidianaSepaloSetosa))
print(paste("El Area del Sepala para la Versicolor :", EuclidianaSepaloVersicolor))
print(paste("El Area del Sepalo para la virgnica :", EuclidanaSepaloVirginica))

EuclidianaPetaloSetosa <- sqrt(sum(DataSetosa["Petal.Length"]*DataSetosa["Petal.Width"]^2))
EuclidianaPetaloVersicolor <- sqrt(sum(DataVersicolor["Petal.Length"]*DataVersicolor["Petal.Width"]^2))
EuclidianaPetaloVirginica <- sqrt(sum(DataVirginica["Petal.Length"]*DataVirginica["Petal.Width"]^2))

print(paste("El Area del petalo para la setosa :",EuclidianaPetaloSetosa))
print(paste("El Area del petalo para la Versicolor :", EuclidianaPetaloVersicolor))
print(paste("El Area del petalo para la virgnica :", EuclidianaPetaloVirginica))

#Problema 5

MatrizSetosa <- matrix(unlist(DataSetosa), ncol=5, byrow=TRUE)
MatrizVersicolor <- matrix(unlist(DataVersicolor), ncol=5, byrow=TRUE)
MatrizVirginica <- matrix(unlist(DataVirginica), ncol=5, byrow=TRUE)

SetosaVersicolor <- dotchart(MatrizSetosa ,  MatrizVersicolor )
SetosaVirginica <- dotchart(MatrizSetosa ,  MatrizVirginica)
VersicolorVirgiica <- dotchart ( MatrizVersicolor , MatrizVirginica)
VirginicaSetosa <- dotchart( MatrizVirginica , MatrizSetosa )
VirginicaVersicolor <-  dotchart(MatrizVirginica , MatrizVersicolor)
dot
