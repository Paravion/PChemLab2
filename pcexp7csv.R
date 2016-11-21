#PChem_Exp7 alternative

setwd("PChemLab2/data/exp7")

for (a in 1:5){
  filename <- paste("a",a,"-1",".txt",sep="") 
  data1 <- read.table(filename)
  filename <- paste("a",a,"-2",".txt",sep="")
  data2 <- read.table(filename)
  aa <- data1[,1] > 0
  bb <- data2[,1] > 0
  time <- (data1[aa,1]+data2[bb,1])/2
  intensity <- (data1[aa,2]+data2[bb,2])/2
  data <- data.frame(time,intensity)
  t <- length(data$time)
  it <- data$intensity[t] #it = intensity_t
  A <- log10(data$intensity/it) #A = intensity / intensity_t
  full <- data.frame(time,intensity,A) #full = full data
  newfile <- paste("A",a,".csv",sep="")
  write.csv(full,file=newfile,row.names=FALSE)
  a <- a+1
 }
 
for (a in 1:5){
  filename <- paste("a",a,"-1",".txt",sep="") 
  data1 <- read.table(filename)
  filename <- paste("a",a,"-2",".txt",sep="")
  data2 <- read.table(filename)
  aa <- data1[,1] > 0
  bb <- data2[,1] > 0
  time <- (data1[aa,1]+data2[bb,1])/2
  intensity <- (data1[aa,2]+data2[bb,2])/2
  data <- data.frame(time,intensity)
  t <- length(data$time)
  it <- data$intensity[t] #it = intensity_t
  A <- log10(data$intensity/it) #A = intensity / intensity_t
  aa <- A != 0
  full <- data.frame(time[aa],intensity[aa],A[aa]) #full = full data
  newfile <- paste("A",a,"aa",".csv",sep="")
  write.csv(full,file=newfile,row.names=FALSE)
  a <- a+1
}
