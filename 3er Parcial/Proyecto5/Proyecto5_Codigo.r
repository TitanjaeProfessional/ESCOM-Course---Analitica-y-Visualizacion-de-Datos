
# -*- coding: utf-8 -*-
#Program : Proyecto 5
#Autor: Garcia De Arcos Jose Angel Eduardo ESCOM Student 
#course : Analitica y visualizacion de datos
#Created on Enero 019  2023

covid <- read.csv("C:/Users/Angel/OneDrive/Documents/covid_abstracts.csv", header = TRUE, sep = ",")


install.packages(lsa)
install.packages(tm)
install.packages(pheatmap)

library(lsa)
library(proxy)
library(tm)
library(pheatmap)
covid <- covid[501:600,]

nrow(covid)
text = covid[,2]
corpus <- VCorpus(VectorSource(text))
tdm <- TermDocumentMatrix(corpus, 
                          control = list(wordLengths = c(1, Inf)))
occurrence <- apply(X = tdm, 
                    MARGIN = 1, 
                    FUN = function(x) sum(x > 0) / ncol(tdm))
tdm_mat <- as.matrix(tdm[names(occurrence)[occurrence >= 0.5], ])
lsaSpace <- lsa(tdm_mat)

lsaMatrix <- diag(lsaSpace$sk) %*% t(lsaSpace$dk)

distMatrix <- cosine(lsaMatrix)
resultado_final <- round(distMatrix, 3)



pheatmap(resultado_final, cluster_rows = FALSE, cluster_cols = FALSE)



for (i in 1:15) {
  print(covid[order(distMatrix[i, ], decreasing = TRUE)[2], 1])
}