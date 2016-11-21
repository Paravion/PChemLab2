#P_Chem Lab2 Exp3: Reaction of Ethyl Acetate with Hydroxyl Ion 
#Followed by Electrical Conudutace
#Coded by Henry Lin

require(ggplot2)

setwd("PChemLab2")

t25 <- read.csv("data/1001-25-1.csv")
t35 <- read.csv("data/1001-35.csv")

#T=25
#dealing with data
t25 <- data.frame(t=t25$SAMPLE+1,gt=t25$Uncomp.Cond)
g0 <- 2.98
t25 <- t25[5001:20500,]
t25 <- data.frame(t=t25$t - 5000,gt=t25$gt,g0gtt=(g0 - t25$gt)/t25$t)
plot(t25$g0gtt,t25$gt)

#linear modeling
model <- lm(formula = g0gtt ~ gt, data = t25)
k1a <- coefficients(model)["gt"]
gc <- coefficients(model)["(Intercept)"]
k1a25 <- k1a
gc25 <- gc

summary(model)

#plotting
png("plot/exp3_t25.png", width = 500, height = 500)

plot25 <- ggplot(data=t25,(aes(x=gt,y=g0gtt)))
plot25 + geom_point()+
  geom_abline(slope = k1a25, intercept = gc)+
  labs(title= "Gt vs (G0-Gt)/t [T=25]",
       x="(G0-Gt)/t",
       y="Gt")+
  theme_gray()

dev.off()

#T=35
#dealing with data
t35 <- data.frame(t=t35$SAMPLE+1,gt=t35$Uncomp.Cond)
g0 <- 2.63
t35 <- t35[4001:13974,]
t35 <- data.frame(t=t35$t - 4000,gt=t35$gt,g0gtt=(g0 - t35$gt)/t35$t)
plot(t35$g0gtt,t35$gt)

#linear modeling
model <- lm(formula = g0gtt ~ gt, data = t35)
k1a <- coefficients(model)["gt"]
gc <- coefficients(model)["(Intercept)"]
k1a35 <- k1a
gc35 <- gc

summary(model)

#plotting
png("plot/exp3_t35.png", width = 500, height = 500)

plot25 <- ggplot(data=t35,(aes(x=gt,y=g0gtt)))
plot25 + geom_point()+
  geom_abline(slope = k1a35, intercept = gc)+
  labs(title= "Gt vs (G0-Gt)/t [T=35]",
       x="(G0-Gt)/t",
       y="Gt")+
  theme_gray()

dev.off()

k1a25 <- k1a25^(-1)
k1a35 <- k1a35^(-1)

k1a25
gc25

k1a35
gc35
