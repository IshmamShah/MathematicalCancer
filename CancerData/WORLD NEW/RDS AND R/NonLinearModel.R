#--------------loading required libraries--------------
library(ggplot2)
library(plotly)
library(tidyr)
library(caret)

#--------------loading necessary data frames and variables--------------
BothSexesCancerGenerated <- readRDS("C:/Users/HP/Desktop/Epidemiology & Carcinogenesis/Cancer Data/WORLD NEW/RDS AND R/BothSexesCancerGenerated.rds")
#Note: BothSexesCancerGenerated is sorted based on the incidence rate of age group 00-14
FemaleCancerFormatted <- readRDS("C:/Users/HP/Desktop/Epidemiology & Carcinogenesis/Cancer Data/WORLD NEW/RDS AND R/FemaleCancerFormatted.rds")
MaleCancerFormatted <- readRDS("C:/Users/HP/Desktop/Epidemiology & Carcinogenesis/Cancer Data/WORLD NEW/RDS AND R/MaleCancerFormatted.rds")

