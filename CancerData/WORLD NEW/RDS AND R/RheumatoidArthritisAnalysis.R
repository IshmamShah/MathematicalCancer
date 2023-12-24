#--------------loading required libraries--------------
library(ggplot2)
library(tidyr)

#--------------generating necessary variables--------------

my.formula <- y ~ x

AgeDesc <- 'AgeGroup : AgeClass
       21-30 : 1
       31-40 : 2
       41-50 : 3
       51-60 : 4
       61-70 : 5
       >70   : 6'

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

#--------------creating rheumatoid arthiritis data frame--------------

#   ***Source: doi:10.1093/rheumatology/ken205; Table 1***

logAC <- c(log(seq(1,6,1)))
rmtdArthDF <- data.frame(logAC)
name.of.rows <- c("21-30","31-40","41-50","51-60","61-70","> 70")
row.names(rmtdArthDF) <- name.of.rows
rmtdArthDF$Female <- c(4.2, 8.1, 13.8, 18.3, 14.5, 15.3)
rmtdArthDF$Male <- c(1.2, 3.2, 3.2, 6.9, 9.1, 15.8)
rmtdArthDF$BothSex <- (rmtdArthDF$Female + rmtdArthDF$Male) / 2
rmtdArthDF$logIFML <- log(rmtdArthDF$Female)
rmtdArthDF$logIMAL <- log(rmtdArthDF$Male)
rmtdArthDF$logIBS <- log(rmtdArthDF$BothSex)
saveRDS(rmtdArthDF,"rmtdArthDF.rds")

#--------------plotting graphs--------------

tempDF <- infToNA(rmtdArthDF)

#regression of both sexes data and collecting important stat values
linearModelBS <- lm(data = tempDF, logIBS ~ logAC)
rSqrBS <- round(summary(linearModelBS)$r.squared, digits = 4)
pValsBS <- round(summary(linearModelBS)$coefficients[,4], digits = 4)
coeffsBS <- round(summary(linearModelBS)$coefficients[,1], digits = 4)
graphDescp <- paste(" BothSexes  => ","formula : log(Incidence) = (",coeffsBS[1],") + (",coeffsBS[2],") * log(AgeClass)"," r-squared :", rSqrBS, "p-values : (Intercept)", pValsBS[1], ", (Slope)", pValsBS[2],"\n")


#regression of female data and collecting important stat values
linearModelFML <- lm(data = tempDF, logIFML ~ logAC)
rSqrFML <- round(summary(linearModelFML)$r.squared, digits = 4)
pValsFML <- round(summary(linearModelFML)$coefficients[,4], digits = 4)
coeffsFML <- round(summary(linearModelFML)$coefficients[,1], digits = 4)
graphDescp <- paste(graphDescp, "Female       => ","formula : log(Incidence) = (",coeffsFML[1],") + (",coeffsFML[2],") * log(AgeClass)"," r-squared :", rSqrFML, "p-values : (Intercept)", pValsFML[1], ", (Slope)", pValsFML[2],"\n")


#regression of male data and collecting important stat values
linearModelMAL <- lm(data = tempDF, logIMAL ~ logAC)
rSqrMAL <- round(summary(linearModelMAL)$r.squared, digits = 4)
pValsMAL <- round(summary(linearModelMAL)$coefficients[,4], digits = 4)
coeffsMAL <- round(summary(linearModelMAL)$coefficients[,1], digits = 4)
graphDescp <- paste(graphDescp, "Male          => ","formula : log(Incidence) = (",coeffsMAL[1],") + (",coeffsMAL[2],") * log(AgeClass)"," r-squared :", rSqrMAL, "p-values : (Intercept)", pValsMAL[1], ", (Slope)", pValsMAL[2],"\n")

#plotting the graphs of both sexes, male and female cancer in a single plot

tempVals <- infToNA(tempDF)
cancerPlot <- tempVals %>% gather(key, value, logIBS, logIFML, logIMAL) %>% ggplot(aes(x=logAC,y=value, colour=key)) + 
  geom_point() + 
  geom_smooth(method = "lm", formula = my.formula, se=FALSE, na.rm = TRUE) + 
  scale_color_manual(labels = c("BothSexes", "Female", paste("Male")), values = c("red", "blue", "green"))

print(cancerPlot)

#saving the plot
ggsave(filename = paste(paste("Rheumatoid Arthritis"),".pdf",sep = ""), height = 9, width = 12)
