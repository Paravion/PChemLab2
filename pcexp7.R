#P_Chem Exp7

library(ggplot2)

setwd("PChemLab2/data/exp7")

start <- function(a=1){
  filename <- paste("a",a,"-1",".txt",sep="") 
  data1 <- read.table(filename)
  filename <- paste("a",a,"-2",".txt",sep="")
  data2 <- read.table(filename)
  aa <- data1[,1] > 0
  bb <- data2[,1] > 0
  time <- (data1[aa,1]+data2[bb,1])/2
  intensity <- (data1[aa,2]+data2[bb,2])/2
  data <- data.frame(time,intensity)
  t <- length(data$time) -1
  it <- data$intensity[1] #it = intensity_t
  A <- log10(data$intensity/it) #A = intensity / intensity_t
  full <- data.frame(time,intensity,A) #full = full data
}

model <- lm (full$intensity~full$time)

#plot intensity to time
ggplot(data=full, aes(x=time,y=intensity))

#plot A to time
ggplot(data=full, aes(x=time,y=intensity))

#calculate A_eq






