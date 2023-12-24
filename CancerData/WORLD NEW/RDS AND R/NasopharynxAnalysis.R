library(ggplot2)
library(plotly)
library(caret)

#Initialize DataFrame
T <- seq(1,9)
CancerDF <- data.frame(T)
CancerDF$ln_T <- log(CancerDF$T)

cancerName <- "Nasopharynx"
CancerDF$ln_I_Cancer <- log(BothSexesCancerGenerated[,cancerName])

#Iterate through all the possible values to find the best fit
for (deg in seq(.01,4,.01)) {
  r2Vals <- c()
  for (k in seq(0.01,0.99,0.01)) {
    
    if(k * 9^deg < 1) {
      CancerDF$ln_I_Cancer_Mod <- CancerDF$ln_I_Cancer - log(1 - k * CancerDF$T^deg)
      predModel <- lm(data = CancerDF, ln_I_Cancer_Mod ~ ln_T)
      
      c_incpt <- summary(predModel)$coefficient[1]
      c_x <- k
      c_lnx <- summary(predModel)$coefficient[2]
      
      PredCancerIT = function(x) {
        (c_incpt) + log(1-((x^deg) * (c_x)))  +   (c_lnx) * log(x)
      }
      
      CancerDF$CancerIPred <- PredCancerIT(1:9)
      
      R_2 <- postResample(obs = CancerDF$ln_I_Cancer, pred = CancerDF$CancerIPred)[2]
      
      r2Vals <- append(r2Vals, R_2)
    }
    
    
  }
  
  if(length(r2Vals)>0) {
    print(paste("deg = ", deg, "max R_2 = ", round(max(r2Vals),digits = 4), "k = ", which.max(r2Vals)))
  }
  
}

#choose the best coefficients by analyzing the printed data
deg <- .14
k<- 0.73

CancerDF$ln_I_Cancer_Mod <- CancerDF$ln_I_Cancer - log(1 - k * CancerDF$T^deg)
predModel <- lm(data = CancerDF, ln_I_Cancer_Mod ~ ln_T)

c_incpt <- summary(predModel)$coefficient[1]
c_x <- k
c_lnx <- summary(predModel)$coefficient[2]

PredCancerIT = function(x) {
  (c_incpt) + log(1-((x^deg) * (c_x)))  +   (c_lnx) * log(x)
}
CancerDF$CancerIPred <- PredCancerIT(1:9)

R_2 <- postResample(obs = CancerDF$ln_I_Cancer, pred = CancerDF$CancerIPred)[2]

funct <- paste('ln(I) = (',round(c_incpt,digits = 4),') + (',round(c_lnx,digits = 4),') * ln(AgeClass) +  ln ( 1 - (' , round(c_x,digits = 4), ') * (AgeClass)^', deg, ' )    R^2 = ', round(R_2,digits = 4), sep = "")

#Plot the graph
plot2 <- ggplot(data = CancerDF) + geom_point(aes(x = ln_T, y = ln_I_Cancer))
plot2 <- plot2 + geom_line(aes(x = ln_T, y = CancerIPred))
plot2 <- plot2 + labs(title = paste("Both Sexes Cancer -", cancerName), x = "log(Age Class)", y = "log(Incidence)", subtitle = funct)
print(plot2)

plotName <- paste("Both Sexes Cancer", cancerName)

ggsave(filename = paste(plotName,".pdf",sep = ""), height = 9, width = 12)
ggsave(filename = paste(plotName,".eps",sep = ""), height = 9, width = 12)


deg <- .14

#Male

r2Vals <- c()
for (k in seq(.01,.99,.01)) {
  if(k * 8^deg <= 1) {
    CancerDF$ln_I_Cancer_M <- log(MaleCancerFormatted$CancerI)
    CancerDF$ln_I_Cancer_M_Mod <- CancerDF$ln_I_Cancer - log(1 - k * CancerDF$T^deg) - (c_lnx) * log(T)
    
    predModel <- lm(data = CancerDF, ln_I_Cancer_M_Mod ~ 1)
    c_incpt <- summary(predModel)$coefficient[1]
    c_x <- k
    PredCancerIT = function(x) {
      (c_incpt) + log(1-((x^deg) * (c_x)))  +   (c_lnx) * log(x)
    }
    
    CancerDF$CancerIPredM <- PredCancerIT(1:9)
    
    R_2 <- postResample(obs = CancerDF$ln_I_Cancer_M, pred = CancerDF$CancerIPredM)[2]
    
    r2Vals <- append(r2Vals, R_2)
  }
}

print(paste("deg = ", deg, "max R_2 = ", round(max(r2Vals),digits = 4), "k = ", which.max(r2Vals)))

deg <- .14
k <- .73

CancerDF$ln_I_Cancer_M <- log(MaleCancerFormatted$CancerI)
CancerDF$ln_I_Cancer_M_Mod <- CancerDF$ln_I_Cancer - log(1 - k * CancerDF$T^deg) - (c_lnx) * log(T)

predModel <- lm(data = CancerDF, ln_I_Cancer_M_Mod ~ 1)
c_incpt <- summary(predModel)$coefficient[1]
c_x <- k

#adjust c_inpt from predicted values and the graph
c_incpt <- -0.54

PredCancerIT = function(x) {
  (c_incpt) + log(1-((x^deg) * (c_x)))  +   (c_lnx) * log(x)
}

CancerDF$CancerIPredM <- PredCancerIT(1:9)

R_2 <- postResample(obs = CancerDF$ln_I_Cancer_M, pred = CancerDF$CancerIPredM)[2]

funct <- paste('ln(I) = (',round(c_incpt,digits = 4),') + (',round(c_lnx,digits = 4),') * ln(AgeClass) +  ln ( 1 - (' , round(c_x,digits = 4), ') * (AgeClass)^', deg, ' )    R^2 = ', round(R_2,digits = 4), sep = "")

#Plot the graph
plot2 <- ggplot(data = CancerDF) + geom_point(aes(x = ln_T, y = ln_I_Cancer_M))
plot2 <- plot2 + geom_line(aes(x = ln_T, y = CancerIPredM))
plot2 <- plot2 + labs(title = paste("Male Cancer -", cancerName), x = "log(Age Class)", y = "log(Incidence)", subtitle = funct)
print(plot2)

plotName <- paste("Male Cancer", cancerName)

ggsave(filename = paste(plotName,".pdf",sep = ""), height = 9, width = 12)
ggsave(filename = paste(plotName,".eps",sep = ""), height = 9, width = 12)



#Female
r2Vals <- c()
for (k in seq(.01,.99,.01)) {
  if(k * 8^deg <= 1) {
    CancerDF$ln_I_Cancer_F <- log(FemaleCancerFormatted$CancerI)
    CancerDF$ln_I_Cancer_F_Mod <- CancerDF$ln_I_Cancer - log(1 - k * CancerDF$T^deg) - (c_lnx) * log(T)
    
    predModel <- lm(data = CancerDF, ln_I_Cancer_F_Mod ~ 1)
    c_incpt <- summary(predModel)$coefficient[1]
    c_x <- k
    PredCancerIT = function(x) {
      (c_incpt) + log(1-((x^deg) * (c_x)))  +   (c_lnx) * log(x)
    }
    
    CancerDF$CancerIPredF <- PredCancerIT(1:9)
    
    R_2 <- postResample(obs = CancerDF$ln_I_Cancer_F, pred = CancerDF$CancerIPredF)[2]
    
    r2Vals <- append(r2Vals, R_2)
  }
}

print(paste("deg = ", deg, "max R_2 = ", round(max(r2Vals),digits = 4), "k = ", which.max(r2Vals)))

deg <- 0.14
k <- .73

CancerDF$ln_I_Cancer_F <- log(FemaleCancerFormatted$CancerI)
CancerDF$ln_I_Cancer_F_Mod <- CancerDF$ln_I_Cancer - log(1 - k * CancerDF$T^deg) - (c_lnx) * log(T)

predModel <- lm(data = CancerDF, ln_I_Cancer_F_Mod ~ 1)
c_incpt <- summary(predModel)$coefficient[1]
c_x <- k

#adjust c_inpt from predicted values and the graph
c_incpt <- -1.36

PredCancerIT = function(x) {
  (c_incpt) + log(1-((x^deg) * (c_x)))  +   (c_lnx) * log(x)
}

CancerDF$CancerIPredF <- PredCancerIT(1:9)

R_2 <- postResample(obs = CancerDF$ln_I_Cancer_F, pred = CancerDF$CancerIPredF)[2]

funct <- paste('ln(I) = (',round(c_incpt,digits = 4),') + (',round(c_lnx,digits = 4),') * ln(AgeClass) +  ln ( 1 - (' , round(c_x,digits = 4), ') * (AgeClass)^', deg, ' )    R^2 = ', round(R_2,digits = 4), sep = "")

#Plot the graph
plot2 <- ggplot(data = CancerDF) + geom_point(aes(x = ln_T, y = ln_I_Cancer_F))
plot2 <- plot2 + geom_line(aes(x = ln_T, y = CancerIPredF))
plot2 <- plot2 + labs(title = paste("Female Cancer -", cancerName), x = "log(Age Class)", y = "log(Incidence)", subtitle = funct)
print(plot2)

plotName <- paste("Female Cancer", cancerName)

ggsave(filename = paste(plotName,".pdf",sep = ""), height = 9, width = 12)
ggsave(filename = paste(plotName,".eps",sep = ""), height = 9, width = 12)

