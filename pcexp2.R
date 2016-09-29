#P_Chem Lab2 Exp2: Hydrolysis of Methyl Acetate
#Coded by Henry Lin

#T1 = 25 degree C, T2 = 35 degree

require(lattice)

#read data
data0 <- read.csv("pchem2_exp2.csv")
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
head(data25)
#calulate k1 using linear modeling
model <- lm(formula = log.v ~ time, data = data25)
model