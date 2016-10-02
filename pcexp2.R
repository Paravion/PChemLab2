#P_Chem Lab2 Exp2: Hydrolysis of Methyl Acetate
#Coded by Henry Lin

#T1 = 25 degree C, T2 = 35 degree

require(ggplot2)

setwd("PChemLab2")

#read data
data0 <- read.csv("data/pchem2_exp2.csv")
head(data0)
data25 <- data0[data0$temp == 25 & data0$time != 58,] #drop the point at time=58
data35 <- data0[data0$temp == 35,]

#calculate v0
v0 <- (10.6+9.8+10.4)/3

#k = ln[(v_inf - v_0)/(v_inf - v_t)] / t-t_0
#t_0 = 0 , v_0 = 10.26
#k = [ln(v_inf - v_0) - ln(v_inf - v_0)] / t
#[ln(v_inf - v_0) - ln(v_inf - v_0)] = kt
#k = slope

#make a new data frame for T1
v_inf <- data25[9,"naohV"]
log.v <- log (v_inf-v0) - log(v_inf - data25$naohV) 
data25 <- data.frame(data25, log.v)
plot(data25$time,data25$log.v) #just for checking if the trend is right
data25 <- data25[1:7,]

#calulate k1 using linear modeling
model <- lm(formula = log.v ~ time, data = data25)
k <- coefficients(model)["time"]
intc <- coefficients(model)["(Intercept)"]
k1 <- k

summary(model)

#plotting
png("exp2/t25.png", width = 1000, height = 1000)

ggplot(data = data25, aes(x=time, y=log.v))+
  geom_point()+
  geom_abline(slope = k, intercept = intc)+
  labs(title= "ln vs t(sec) [T=25]",
       x="ln",
       y="t(sec)")+
  theme_bw()

dev.off()

#make a new data frame for T2
v_inf <- data35[12,"naohV"]
data35 <- data35[1:10,]
log.v <- log (v_inf-v0) - log(v_inf - data35$naohV) 
data35 <- data.frame(data35, log.v)
plot(data35$time,data35$log.v)

#calulate k2 using linear modeling
model <- lm(formula = log.v ~ time, data = data35)
k <- coefficients(model)["time"]
intc <- coefficients(model)["(Intercept)"]
k2 <- k

summary(model)

#plotting
png("exp2/t35.png", width = 1000, height = 1000)

ggplot(data = data25, aes(x=time, y=log.v))+
  geom_point()+
  geom_abline(slope = k, intercept = intc)+
  labs(title= "ln vs t(sec) [T=35]",
       x="ln",
       y="t(sec)")+
  theme_bw()

dev.off()

#compute half-life
#t_1/2 = 0.693/k
hl1 <- 0.693/k1
hl2 <- 0.693/k2

#calculate Ea
#Ea = R*T1*T2/(T2-T1)*ln(k2/k1)
Ea <- 8.314*25*35/(35-25)*log(k2/k1)

#show the answers
print(paste0("k1 =",k1,"k2 = ",k2))
print(paste0("hl1 =",hl1,"hl2 = ",hl2))
print(paste0("Ea =",Ea))
