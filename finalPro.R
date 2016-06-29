load("C:/Users/Sriram Raju/My Own/UVa/Assignments/LSM/Final Project/regression/Twitter/twitterData.RData")

# Sources

source("C:/Users/Sriram Raju/My Own/UVa/Assignments/LSM/R scripts/SPM_Panel.R")
source("C:/Users/Sriram Raju/My Own/UVa/Assignments/LSM/R scripts/PCAplots.R")

# Choose 38393
set.seed(12345)
data <- twitterData[order(runif(38393)),]

# Result
res = data[,78]
hist(res, breaks = "scott", main = "Histogram of NAD", xlab = "Mean Number of active discussion", col = "steelblue")

NCD <- data[,1:7]
AI <- data[,8:14]
AL <- data[,15:21]
BL <- data[,22:28]
NAC <- data[,29:35]
AtL <- data[,36:42]
CS <- data[,43:49]
Aint <- data[,50:56]
NAu <- data[,57:63]
ADL <- data[,64:70]
NAD <- data[,71:77]

getMean <- function(val){
  for (i in 1:38393 ) {
    mean.out[i] <- mean(as.numeric(val[i,1:7]))  
  }
  return(mean.out)
}


data$Mean.NCD <- getMean(NCD)
data$Mean.AI <- getMean(AI)
data$Mean.AL <- getMean(AL)
data$Mean.BL <- getMean(BL)
data$Mean.NAC <- getMean(NAC)
data$Mean.AtL <- getMean(AtL)
data$Mean.CS <- getMean(CS)
data$Mean.Aint <- getMean(Aint)
data$Mean.NAu <- getMean(NAu)
data$Mean.ADL <- getMean(ADL)
data$Mean.NAD <- getMean(NAD)



#################### Graphics

uva.pairs(data[,c("Mean.NAD","Mean.ADL", "Mean.CS", "Mean.Aint", "Mean.BL", "Mean.AtL")]) 

xyplot(Mean.NAD~Mean.NAu, main = "XY plot between NAD and NAu", xlab = "NAu", ylab = "NAD", data = data)

mean(NAD[,1])
mean(NAD[,2])
mean(NAD[,3])
mean(NAD[,4])
mean(NAD[,5])
mean(NAD[,6])
mean(NAD[,7])

###### Extract Potential buzz using Mean Gradient
MeanGrad <- function(vals)
{
  vals_1 <- vals[,2]-vals[,1]
  vals_2 <- vals[,3]-vals[,2]
  vals_3 <- vals[,4]-vals[,3]
  vals_4 <- vals[,5]-vals[,4]
  vals_5 <- vals[,6]-vals[,5]
  vals_6 <- vals[,7]-vals[,6]
  out <- (vals_1 + vals_2 +vals_3 +vals_4 +vals_5 +vals_6)/7
  return(out)
}

data$Meangrad.NCD <- MeanGrad(NCD)
data$Meangrad.AI <- MeanGrad(AI)
data$Meangrad.AL <- MeanGrad(AL)
data$Meangrad.BL <- MeanGrad(BL)
data$Meangrad.NAC <- MeanGrad(NAC)
data$Meangrad.AtL <- MeanGrad(AtL)
data$Meangrad.CS <- MeanGrad(CS)
data$Meangrad.Aint <- MeanGrad(Aint)
data$Meangrad.NAu <- MeanGrad(NAu)
data$Meangrad.ADL <- MeanGrad(ADL)
data$Meangrad.NAD <- MeanGrad(NAD)


# Threshold 
hist(data$Meangrad.NAD,breaks='scott')

length(which(data$Meangrad.NAD>= 20))

data.MGrad <- data[data$Meangrad.NAD >= 20,]

# Remove outliers
boxplot(data.MGrad$Meangrad.NAD, main = "title", ylab = "y", col = "steelblue", pch = "*")
which(data.MGrad$Meangrad.NAD > 6000) #3077,3801

data.MGrad <- data.MGrad[data.MGrad$Meangrad.NAD <= 6000,]
boxplot(data.MGrad$Meangrad.NAD, main = "Mean of the Gradient of NAD", ylab = "Mean of the gradient", col = "steelblue", pch = "*")


hist(data.MGrad$Meangrad.NAD,breaks='scott')

##### Graphics


qqnorm(data.MGrad$Meangrad.NAD, main = "NAD")
qqline(data.MGrad$Meangrad.NAD)

uva.pairs(data.MGrad[,c("Meangrad.NAD","Meangrad.CS", "Meangrad.NAu", "Meangrad.Aint", "Meangrad.BL", "Meangrad.AtL")]) 

boxplot(data.MGrad[,71:77],main = 'Boxplot of NAD reative to time',ylab='NAD values',xlab='Relative time')

mean(data.MGrad[,71])
mean(data.MGrad[,73])
mean(data.MGrad[,75])
mean(data.MGrad[,77])


library('lattice')
xyplot(Meangrad.NAD~Meangrad.NAu, main = "title", xlab = "NAu", ylab = "NAD", data = data.MGrad)
xyplot(Meangrad.NAD~Meangrad.ADL, main = "title", xlab = "ADL", ylab = "NAD", data = data.MGrad)

twit.pca <- princomp(data.MGrad[,c("Meangrad.NAD","Meangrad.ADL", "Meangrad.NAu","Meangrad.Aint", "Meangrad.BL", "Meangrad.NCD")], cor = T)
biplot(twit.pca)
screeplot(twit.pca, main = "Variance for PC of Metrics")
barplot(twit.pca$loadings[,1])
barplot(twit.pca$loadings[,2])

cumplot(twit.pca, col = "blue")

plot(data.MGrad$Meangrad.NAD,main = "Plot of the Mean of the gradient of NAD", xlab = "Instance Number", ylab = "Mean of Gradient")
