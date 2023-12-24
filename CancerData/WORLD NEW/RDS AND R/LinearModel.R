#--------------loading required libraries--------------
library(ggplot2)
library(plotly)
library(tidyr)

#--------------generating necessary variables--------------

my.formula <- y ~ x

AgeDesc <- 'AgeGroup : AgeClass
       00-14 : 1
       15-39 : 2
       40-44 : 3
       45-49 : 4
       50-54 : 5
       55-59 : 6
       60-64 : 7
       65-69 : 8
       70-74 : 9'

#--------------loading necessary data frames and variables--------------
BothSexesCancerGenerated <- readRDS("~/Documents/Epidemiology & Carcinogenesis/Cancer Data/WORLD NEW/RDS AND R/BothSexesCancerGenerated.rds")
#Note: BothSexesCancerGenerated is sorted based on the incidence rate of age group 00-14
FemaleCancerFormatted <- readRDS("~/Documents/Epidemiology & Carcinogenesis/Cancer Data/WORLD NEW/RDS AND R/FemaleCancerFormatted.rds")
MaleCancerFormatted <- readRDS("~/Documents/Epidemiology & Carcinogenesis/Cancer Data/WORLD NEW/RDS AND R/MaleCancerFormatted.rds")

#--------------generating linear model and plotting graphs--------------
cancerNames <- colnames(BothSexesCancerGenerated)
#Note : 01 to 11 cancers in cancerNames with incedence @ (00-14) : 0
#Note : 12 to 21 cancers in cancerNames with incedence @ (00-14) > 0

#creating helping data frame
logAC <- c(log(seq(1,8,1)))

cancerDF <- data.frame(logAC)

#plotting graphs of cancers with incedence @ (00-14) : 0

#Note : The coefficients are being determined using the lm() function and
#       r-squared value and p-value of the coefficients are being obtained
#       from the summary("lm object"). It is a uni-variate regression model
#       and the predictor of log(I) is log(AC). That's why the p-value of
#       the slope is overall p-value of the regression model.

for(idx in seq(1,11,1)) {
  
  cancerDF <- data.frame(logAC)
  
  cancer <- cancerNames[idx]
  
  #regression of both sexes data and collecting important stat values
  cancerDF$logIBS <- log(BothSexesCancerGenerated[,idx][2:9])
  linearModelBS <- lm(data = cancerDF, logIBS ~ logAC)
  rSqrBS <- round(summary(linearModelBS)$r.squared, digits = 4)
  pValsBS <- round(summary(linearModelBS)$coefficients[,4], digits = 4)
  coeffsBS <- round(summary(linearModelBS)$coefficients[,1], digits = 4)
  graphDescp <- paste(" BothSexes  => ","formula : log(Incidence) = (",coeffsBS[1],") + (",coeffsBS[2],") * log(AgeClass)"," r-squared :", rSqrBS, "p-values : (Intercept)", pValsBS[1], ", (Slope)", pValsBS[2],"\n")
  
  
  #regression of female data and collecting important stat values
  cancerDF$logIFML <- log(FemaleCancerFormatted[,cancer][2:9])
  linearModelFML <- lm(data = cancerDF, logIFML ~ logAC)
  rSqrFML <- round(summary(linearModelFML)$r.squared, digits = 4)
  pValsFML <- round(summary(linearModelFML)$coefficients[,4], digits = 4)
  coeffsFML <- round(summary(linearModelFML)$coefficients[,1], digits = 4)
  graphDescp <- paste(graphDescp, "Female       => ","formula : log(Incidence) = (",coeffsFML[1],") + (",coeffsFML[2],") * log(AgeClass)"," r-squared :", rSqrFML, "p-values : (Intercept)", pValsFML[1], ", (Slope)", pValsFML[2],"\n")
  
  
  #regression of male data and collecting important stat values
  cancerDF$logIMAL <- log(MaleCancerFormatted[,cancer][2:9])
  linearModelMAL <- lm(data = cancerDF, logIMAL ~ logAC)
  rSqrMAL <- round(summary(linearModelMAL)$r.squared, digits = 4)
  pValsMAL <- round(summary(linearModelMAL)$coefficients[,4], digits = 4)
  coeffsMAL <- round(summary(linearModelMAL)$coefficients[,1], digits = 4)
  graphDescp <- paste(graphDescp, "Male          => ","formula : log(Incidence) = (",coeffsMAL[1],") + (",coeffsMAL[2],") * log(AgeClass)"," r-squared :", rSqrMAL, "p-values : (Intercept)", pValsMAL[1], ", (Slope)", pValsMAL[2],"\n")
  
  #plotting all the graphs
  tempVals <- cancerDF
  cancerPlot <- tempVals %>% gather(key, value, logIBS, logIFML, logIMAL) %>% ggplot(aes(x=logAC,y=value, colour=key)) + 
    geom_point() + 
    geom_smooth(method = "lm", formula = my.formula, se=FALSE) + 
    labs(title = paste(cancer,"Cancer"), x = "log(Age Class)", y = "log(Incidence)", subtitle = graphDescp) + 
    scale_color_manual(labels = c("BothSexes", "Female", "Male\n"), values = c("red", "blue", "green"))
  
  print(cancerPlot)
  
  ggsave(filename = paste(paste(cancer,"Cancer"),".pdf",sep = ""), height = 9, width = 12)
}

#creating helping data frame
logAC <- c(log(seq(1,9,1)))

cancerDF <- data.frame(logAC)

#inf to NA util function
infToNA <- function(df) {
  n <- nrow(df)
  m <- ncol(df)
  for(i in seq(1,n,1)) {
    for(j in seq(1,m,1)) {
      if(is.infinite(df[i,j])) {
        df[i,j] = NA;
      }
    }
  }
  return(df)
}

#plotting graphs of cancers with incedence @ (00-14) > 0
for(idx in seq(12,21,1)) {
  
  cancerDF <- data.frame(logAC)
  
  cancer <- cancerNames[idx]
  
  cancerDF$logIBS <- log(BothSexesCancerGenerated[,cancer])
  cancerDF$logIFML <- log(FemaleCancerFormatted[,cancer])
  cancerDF$logIMAL <- log(MaleCancerFormatted[,cancer])
  
  cancerDF <- infToNA(cancerDF)
  
  #regression of both sexes data and collecting important stat values
  linearModelBS <- lm(data = cancerDF, logIBS ~ logAC)
  rSqrBS <- round(summary(linearModelBS)$r.squared, digits = 4)
  pValsBS <- round(summary(linearModelBS)$coefficients[,4], digits = 4)
  coeffsBS <- round(summary(linearModelBS)$coefficients[,1], digits = 4)
  graphDescp <- paste(" BothSexes  => ","formula : log(Incidence) = (",coeffsBS[1],") + (",coeffsBS[2],") * log(AgeClass)"," r-squared :", rSqrBS, "p-values : (Intercept)", pValsBS[1], ", (Slope)", pValsBS[2],"\n")
  
  
  #regression of female data and collecting important stat values
  linearModelFML <- lm(data = cancerDF, logIFML ~ logAC)
  rSqrFML <- round(summary(linearModelFML)$r.squared, digits = 4)
  pValsFML <- round(summary(linearModelFML)$coefficients[,4], digits = 4)
  coeffsFML <- round(summary(linearModelFML)$coefficients[,1], digits = 4)
  graphDescp <- paste(graphDescp, "Female       => ","formula : log(Incidence) = (",coeffsFML[1],") + (",coeffsFML[2],") * log(AgeClass)"," r-squared :", rSqrFML, "p-values : (Intercept)", pValsFML[1], ", (Slope)", pValsFML[2],"\n")
  
  
  #regression of male data and collecting important stat values
  linearModelMAL <- lm(data = cancerDF, logIMAL ~ logAC)
  rSqrMAL <- round(summary(linearModelMAL)$r.squared, digits = 4)
  pValsMAL <- round(summary(linearModelMAL)$coefficients[,4], digits = 4)
  coeffsMAL <- round(summary(linearModelMAL)$coefficients[,1], digits = 4)
  graphDescp <- paste(graphDescp, "Male          => ","formula : log(Incidence) = (",coeffsMAL[1],") + (",coeffsMAL[2],") * log(AgeClass)"," r-squared :", rSqrMAL, "p-values : (Intercept)", pValsMAL[1], ", (Slope)", pValsMAL[2],"\n")
  
  #plotting the graphs of both sexes, male and female cancer in a single plot
  
  tempVals <- infToNA(cancerDF)
  cancerPlot <- tempVals %>% gather(key, value, logIBS, logIFML, logIMAL) %>% ggplot(aes(x=logAC,y=value, colour=key)) + 
    geom_point() + 
    geom_smooth(method = "lm", formula = my.formula, se=FALSE, na.rm = TRUE) + 
    labs(title = paste(cancer,"Cancer"), x = "log(Age Class)", y = "log(Incidence)", subtitle = graphDescp) + 
    scale_color_manual(labels = c("BothSexes", "Female", paste("Male")), values = c("red", "blue", "green"))
  
  print(cancerPlot)
  
  #saving the plot
  ggsave(filename = paste(paste(cancer,"Cancer"),".pdf",sep = ""), height = 9, width = 12)
}

