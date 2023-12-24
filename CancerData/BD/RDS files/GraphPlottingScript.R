#loading Data
MaleCancerBD <- readRDS("C:/Users/HP/Desktop/Epidemiology & Carcinogenesis/Cancer Data/RDS files/MaleCancerBD.rds")
FemaleCancerBD <- readRDS("C:/Users/HP/Desktop/Epidemiology & Carcinogenesis/Cancer Data/RDS files/FemaleCancerBD.rds")
BothGendersCancerBD <- readRDS("C:/Users/HP/Desktop/Epidemiology & Carcinogenesis/Cancer Data/RDS files/BothGendersCancerBD.rds")

#formatting Male Cancer Data

cancerNames <- MaleCancerBD$Cancer
tempDF <- t(MaleCancerBD[ , -c(1,2)])
colnames(tempDF) <- cancerNames
MaleCancerFormatted <- as.data.frame(tempDF)

#formatting Female Cancer Data

cancerNames <- FemaleCancerBD$Cancer
tempDF <- t(FemaleCancerBD[ , -c(1,2)])
colnames(tempDF) <- cancerNames
FemaleCancerFormatted <- as.data.frame(tempDF)

#formatting Both Gender Cancer Data

cancerNames <- BothGendersCancerBD$Cancer
tempDF <- t(BothGendersCancerBD[ , -c(1,2)])
colnames(tempDF) <- cancerNames
BothGendersCancerFormatted <- as.data.frame(tempDF)

library(ggplot2)
library(plotly)
library(ggpmisc)


logAC <- c(log(1:8))
my.formula <- y ~ x


#plotting Male Cancer Graphs
for(cancer in colnames(MaleCancerFormatted)) {
  LabelPlot <- ggplot(data = MaleCancerFormatted, 
                    aes( x = logAC, y = log(MaleCancerFormatted[,cancer]))) + 
                    geom_smooth(method = "lm", formula = my.formula) + 
                    stat_poly_eq(formula = my.formula, 
                    aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), parse = TRUE) + 
                    geom_point() + 
                    labs(title = paste("Male Cancer -", cancer), x = "log(Age Class)", y = "log(Incidence)")
  
  print(LabelPlot)
  
  readline("Press 'enter' to continue....")
}

#plotting Female Cancer Graphs
for(cancer in colnames(FemaleCancerFormatted)) {
  LabelPlot <- ggplot(data = FemaleCancerFormatted, 
                      aes( x = logAC, y = log(FemaleCancerFormatted[,cancer]))) + 
    geom_smooth(method = "lm", formula = my.formula) + 
    stat_poly_eq(formula = my.formula, 
                 aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), parse = TRUE) + 
    geom_point() + 
    labs(title = paste("Female Cancer -", cancer), x = "log(Age Class)", y = "log(Incidence)")
  
  print(LabelPlot)
  
  readline("Press 'enter' to continue....")
}

#plotting Both Gender Cancer Graphs
for(cancer in colnames(BothGendersCancerFormatted)) {
  LabelPlot <- ggplot(data = BothGendersCancerFormatted, 
                      aes( x = logAC, y = log(BothGendersCancerFormatted[,cancer]))) + 
    geom_smooth(method = "lm", formula = my.formula) + 
    stat_poly_eq(formula = my.formula, 
                 aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), parse = TRUE) + 
    geom_point() + 
    labs(title = paste("Both Genders Cancer -", cancer), x = "log(Age Class)", y = "log(Incidence)")
  
  print(LabelPlot)
  
  readline("Press 'enter' to continue....")
}

#plotting Interactive Male Cancer Graphs
for(cancer in colnames(MaleCancerFormatted)) {
  LabelPlot <- ggplot(data = MaleCancerFormatted, 
                      aes( x = logAC, y = log(MaleCancerFormatted[,cancer]))) + 
    geom_smooth(method = "lm", formula = my.formula) + 
    geom_point() + 
    labs(title = paste("Male Cancer -", cancer), x = "log(Age Class)", y = "log(Incidence)")
  
  LabelPlot <- ggplotly(LabelPlot)
  ggplotly(LabelPlot)
  print(LabelPlot)
  
  readline("Press 'enter' to continue....")
}

#plotting Interactive Female Cancer Graphs
for(cancer in colnames(FemaleCancerFormatted)) {
  LabelPlot <- ggplot(data = FemaleCancerFormatted, 
                      aes( x = logAC, y = log(FemaleCancerFormatted[,cancer]))) + 
    geom_smooth(method = "lm", formula = my.formula) + 
    geom_point() + 
    labs(title = paste("Female Cancer -", cancer), x = "log(Age Class)", y = "log(Incidence)")
  
  LabelPlot <- ggplotly(LabelPlot)
  ggplotly(LabelPlot)
  print(LabelPlot)
  
  readline("Press 'enter' to continue....")
}

#plotting Interactive Both Gender Cancer Graphs
for(cancer in colnames(BothGendersCancerFormatted)) {
  LabelPlot <- ggplot(data = BothGendersCancerFormatted, 
                      aes( x = logAC, y = log(BothGendersCancerFormatted[,cancer]))) + 
    geom_smooth(method = "lm", formula = my.formula) + 
    geom_point() + 
    labs(title = paste("Both Genders Cancer -", cancer), x = "log(Age Class)", y = "log(Incidence)")
  
  LabelPlot <- ggplotly(LabelPlot)
  ggplotly(LabelPlot)
  print(LabelPlot)
  
  readline("Press 'enter' to continue....")
}
