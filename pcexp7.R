#P_Chem Exp7

#read data
#convert data
#find yt

start <- function(a=1){
  filename <- paste("a",a,"-1",".txt",sep="") 
  data1 <- read.table(filename)
  filename <- paste("a",a,"-2",".txt",sep="")
  data2 <- read.table(filename)
  time <- (data1[,1]+data2[,1])/2
  intensity <- (data1[,2]+data2[,2])/2
  data <- data.frame(time,intensity)
  }





