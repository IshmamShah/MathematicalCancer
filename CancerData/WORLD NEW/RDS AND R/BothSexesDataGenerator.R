
#--------------loading necessary data frames and variables--------------
BothSexesCancerFormatted <- readRDS("C:/Users/HP/Desktop/Epidemiology & Carcinogenesis/Cancer Data/WORLD NEW/RDS AND R/BothSexesCancerFormatted.rds")
#Note: BothSexesCancerFormatted is sorted based on the incidence rate of age group 00-14
FemaleCancerFormatted <- readRDS("C:/Users/HP/Desktop/Epidemiology & Carcinogenesis/Cancer Data/WORLD NEW/RDS AND R/FemaleCancerFormatted.rds")
MaleCancerFormatted <- readRDS("C:/Users/HP/Desktop/Epidemiology & Carcinogenesis/Cancer Data/WORLD NEW/RDS AND R/MaleCancerFormatted.rds")

#--------------generating BS data--------------
cancerNames <- colnames(BothSexesCancerFormatted)
BothSexesCancerGenerated <- 0.5 * (MaleCancerFormatted[,cancerNames] + FemaleCancerFormatted[,cancerNames])

saveRDS(BothSexesCancerGenerated,"BothSexesCancerGenerated.rds")