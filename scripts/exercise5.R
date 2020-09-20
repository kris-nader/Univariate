fev.dat=read.table(file=file.choose(),header=TRUE)
#descriptive stat
summary(fev.dat)
sapply(fev.dat,class)
names(fev.dat)

sapply(fev.dat[,1:3],mean)
sapply(fev.dat[,1:3],median)
sapply(fev.dat[,1:3],mode)

boxplot(fev.dat[,1:3])
hist(fev.dat[,1:3])
table(fev.dat$Smoke)
table(fev.dat$Sex)
cor(fev.dat[,1:3])

par(mfrow=c(1,3))
plot(density(fev.dat[,1]))
plot(density(fev.dat[,2]))
plot(density(fev.dat[,3])) # we see right skewed

#dummy variables
x1=as.numeric(fev.dat$Sex=="Female")
x2=as.numeric(fev.dat$Smoke=="Yes")

#Exercise 3
boxplot(fev.dat$Fev~x2) #nt really what we expected what we can say is that the other variable like age and height are actually an influence --> confounding 
# we can also see this in the cor.test because we saw that thy ar ev hghly correclated
lm.reg=lm(fev.dat$Fev~x2)
summary(lm.reg) # we can see this from the veryyy low R2 value only 6% is explained by the model
fit<-fitted(lm.reg)
rs<-rstandard(lm.reg)
par(mfrow=c(1,2))
plot(rs~x2)
plot(rs~fit)
#Exercise 4

#Check normally distributed
sapply(fev.dat[,1:3],shapiro.test)
pairs(fev.dat[,1:3])
cor(fev.dat[,1:3])
#propose a linear regression
temp_fev=cbind(fev.dat$Fev,fev.dat$Age,fev.dat$Height,x1,x2)
colnames(temp_fev)=c("Fev","Age","Height","x1","x2")

temp_fev_subset=temp_fev[1:dim(fev.dat)[1]/2,]
mreg=lm(fev.dat$Fev~fev.dat$Age+fev.dat$Height+x1+x2+I(fev.dat$Height*fev.dat$Age)+I(fev.dat$Height*x1)+I(fev.dat$Height*x2)+I(fev.dat$Age*x1)+I(fev.dat$Age*x2))
summary(mreg)
fit<-fitted(mreg)
rs<-rstandard(mreg)
plot(rs~fit)

par(mfrow=c(1,2))
scatter.smooth(fev.dat$Height,fev.dat$Fev)
scatter.smooth(fev.dat$Age,fev.dat$Fev)

#adjusted r2 takes into account the number of variables
mreg=lm(fev.dat$Fev~fev.dat$Age+fev.dat$Height+x1+x2+I(fev.dat$Height*fev.dat$Age)+I(fev.dat$Height*x1)+I(fev.dat$Height*x2)+I(fev.dat$Age*x1)+I(fev.dat$Age*x2)+I(fev.dat$Height^2))
summary(mreg)
fit<-fitted(mreg)
rs<-rstandard(mreg)
plot(rs~fit)

par(mfrow=c(1,2))

#deviation from straight line in a QQ test means not normal
#polynomial terms make the model unfit for predictions --> problem w extrapolation
#poly is only good to explain what is happening in the situation but not for prediction