#P_Chem Lab2 Exp2: Hydrolysis of Methyl Acetate
#Coded by Henry Lin

#T1 = 25 degree C, T2 = 35 degree

#read data
data0 <- read.csv("data/pchem2_exp2.csv")
head(data0)
data25 <- data0[data0$temp == 25 & data0$time != 58,] #drop the point at time=58
data35 <- data0[data0$temp == 35,]

#k = ln[(v_inf - v_0)/(v_inf - v_t)] / t-t_0
#t_0 = 0 , v_0 = 0
#k = - ln(v_inf - v_r) / t
#ln(v_inf - v_t) = - kt
#k

#make a new data frame for T1
v_inf <- data25[9,"naohV"]
log.v <- log(v_inf - data25$naohV)
data25 <- data.frame(data25, log.v)
plot(data25$time,data25$log.v)
data25 <- data25[1:7,]
#calulate k1 using linear modeling
model <- lm(formula = log.v ~ time, data = data25)

#ploting
plot(x=data25$time ,
     y=data25$log.v ,
     main = "ln(v_inf - v_t) vs t" ,
     xlab = "t (sec)",
     ylab = "ln(v_inf - v_t)")
abline(model, lwd = 2)
k1 <- coefficients(model)["time"]


