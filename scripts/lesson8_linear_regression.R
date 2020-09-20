#Chapter 8: Linear regression

# simple regression-EXAMPLE1
res.lm1<-lm(annual~lat)
summary(res.lm1)
# Steps to follow
# 1. Is there regression? H0: B=0( no linear) H1: B!=0 
#    we look agt p-value= 1.226e-13 <0.05 so we have small confidence in our H0-- rejected
#    then we have regression
#                            Annual=34.96769-0.50368*lat
pairs(combine)
cor(combine)
plot(annual~lat)
abline(res.lm1,col="pink")
# 2. How good is the regression? 
#    We look at the R2 value and we see that 81.5% of variability in the data can be explained using this model
anova(res.lm1)
# 3. Parameter estimate
#    H0: a=0 H1: a!=0 ( not very interested but we see that the pvalue is sig)
#    H0: b=0 H1: b!=0 ( very interested 1.23e-13 < 0.05 then H1 is accepted)
# 4. Check assumptions
#    A. Linearity --> we hope for a random pattern in the residual plots
#		- check the residuals against fitted values
#		- check the residuals against each regressor(independent variable)
fit<-fitted(res.lm1) # gives fitted values
rs<-rstandard(res.lm1) # gives standard residuals
par(mfrow=c(1,2))
plot(rs~fit)
plot(rs~lat)
#    B. Normality of the RESIDUALS
shapiro.test(rs)
hist(rs,prob=TRUE,col="pink")
#    C. Influential point
par(mfrow=c(1,1))
plot(cooks.distance(res.lm1)) #no influential points so we dont have to remove anything
# 5. Predict--Extrapolating
#	A. Predict average annual temp for a city with given lat
res.pred1<-predict(res.lm1, list(lat=c(50,37,39,13)), interval="confidence")
city1<-c("Belgium","Athens","Ankara","Bangkok")
pred1<-data.frame(city1,res.pred1)
pred1
#	B. Predict the annual temp for a city with a given lat
res.pred2<-predict(res.lm1,list(lat=c(50,37,39,13)),interval="prediction")
pred2<-data.frame(city1,res.pred2)
pred2

# EXAMPLE 2
time<-1:15
bact <- c(355,211,197, 166,142,106,104,60,56,38,36,32,21,19,15)
# 1. Is there regression? H0: B=0( no linear) H1: B!=0 
shapiro.test(bact)
shapiro.test(time)
bacteria_time<-data.frame(time,bact)
pairs(bacteria_time)
cor.test(bact,time,method="spearman")
res.lm2<-lm(bact~time)
summary(res.lm2)
plot(bact~time)
abline(res.lm2,col="pink")
# 2. How good is regression? 
anova(res.lm2)# R2 is 82% then 82% of variablity can be explained with this model
# 3. Parameter estimates
# h0: a=0 h1: a!=0 pvalue <0.05 then h0 rejected
# h0: b=0 h1: b!=0 pvalie <0.05 then h0 rejected
# 4. Check assumptions based on residuals
# 	A. Linearity--> of residuals
#		-check the residuals vs fitted
#		-check the residuals vs regressors
rs<-rstandard(res.lm2)
fit<-fitted(res.lm2)
par(mfrow=c(1,2))
plot(rs~fit)
plot(rs~time)
# CONCLUSION: not random-- quadratic
#	B. Normality of residuals
#		-check shapiro or residuals
#		-check histogram of residuals
shapiro.test(rs)
hist(rs,prob=TRUE,col="pink")
# CONCLUSION: not normal distribution-- skewed
#	C. Influential points
par(mfrow=c(1,1))
plot(cooks.distance(res.lm2))
# CONCLUSION: we see an outlier-- needs more analysis to see if it is influential and should be removed


# because it did not pass out assumption test we go back to step 1 to transform
ln_bact<-log(bact)
reslog.lm<-lm(ln_bact~time)
summary(reslog.lm)
# 4. check assumptions
# normality
rs<-rstandard(reslog.lm)
shapiro.test(rs)

# EXERCISE 3-- for cooks distance
amount <- c(0.05,0.1,0.15,0.2,0.25,0.5)
surface <-c(70,60,49,41,30,46)
ex2 <- data.frame(amount, surface)
plot(surface~amount)
res.lm3<-lm(surface~amount)
summary(res.lm3)
plot(surface~amount)
abline(res.lm3,col="pink")

plot(cooks.distance(res.lm3))
ex2[cooks.distance(res.lm3)>8,]
# Now we make the 2 vectors B and B1 with and without the influential point
amount_new<-amount[-6]
surface_new<-surface[-6]
res.lm4<-lm(surface_new~amount_new)
plot(surface_new~amount_new)
abline(res.lm4, col="pink")
plot(cooks.distance(res.lm4))
summary(res.lm4)$r.squared
summary(res.lm3)$r.quared
str(summary(res.lm3))

#EXERCISE 5--LINEARITY
temp <- c(10,20,30,40,50,60)
vapor <- c(9.2,17.5,31.8,55.3,92.5,149.4)
plot(vapor~temp)
res.lm5<-lm(vapor~temp)
abline(res.lm5,col="hot pink")
summary(res.lm5)

fit<-fitted(res.lm5)
rs<-rstandard(res.lm5)
par(mfrow=c(1,2))
plot(rs~fit)
plot(rs~temp)

res.lm6<-lm(vapor~temp+I(temp^2))
summary(res.lm6)
fit6<-fitted(res.lm6)
rs6<-rstandard(res.lm6)
par(mfrow=c(1,2))
plot(rs6~fit6)
plot(rs6~temp)


## MULTIPLE REGRESSION

# 1. Is there regression? H0: B1=B2=0 H1: B1!=0 B2!=0 or both
#    pvalue <0.05 then reject null hypothesis
res.lm7<-lm(annual~lat+long)
summary(res.lm7)
# 2. How good is the regression?
anova(res.lm7)
summary(res.lm7)$r.squared # then 86 % can be explained byt the model
# 3. parameter estimates
# a= 35 b1=-0.4 b2=-0.08
# 	annual=34-0.47*lat -0.08*long
# 4. checl residual assumptions
# 	A. Normality
fit<-fitted(res.lm7)
rs<-rstandard(res.lm7)
par(mfrow=c(2,2))
plot(rs~long)
plot(rs~lat)
plot(rs~fit)
#	B. Linearity
shapiro.test(rs) # not normal
hist(rs, prob=TRUE,col="pink") # normality is assumed
#	C. cooks distance
plot(cooks.distance(res.lm7))
warm_temp[cooks.distance(res.lm7)>0.3,]

annual_new<-annual[-2]
lat_new<-lat[-2]
long_new<-long[-2]
res.lm8<-lm(annual_new~lat_new+long_new)
summary(res.lm8) 
# conclusion: observation is not influential so we keep the point

# 5. prediction








