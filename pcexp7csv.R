#PChem_Exp7 alternative

setwd("PChemLab2/data/exp7")

a <-1

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
  it <- data$intensity[1] #it = intensity_t
  A <- log10(it/data$intensity) #A = intensity_t / intensity
  full <- data.frame(time,intensity,A) #full = full data
  newfile <- paste("A",a,".csv",sep="")
  write.csv(full,file=newfile,row.names=FALSE)
  a <- a+1
  if (a >5){break}
 }

#calulate ln(Aeq-At)-t

index <- 1:5
aeq <- c(0.64,0.80,0.81,0.82,0.84) #from observations
aeq <- data.frame(index,aeq)

a <- 1
for (a in 1:5){
  filename <- paste("a",a,".csv",sep="") 
  data <- read.csv(filename)
  time <- data$time
  lnaeq <- log(aeq$aeq[a] - full$A) #lnaeq = ln(Aeq-At)
  data <- data.frame(time,lnaeq)
  newfile <- paste("A",a,"ln",".csv",sep="")
  write.csv(data,file=newfile,row.names=FALSE)
  a <- a+1
  if (a >5){break}
}


