#Chapter 9 --variable selection

install.packages("leaps")
insur=read.table(file=file.choose(),header=TRUE)

attach(insur)
library(leaps)
leap<-leaps(x=cbind(AGE,EXPER,MILES,TOWORK),y=LOSS,method="r2")
leap2<-leaps(x=cbind(AGE,EXPER,MILES,TOWORK),y=LOSS,method="Cp")
combine<-cbind(leap$which,leap$size,leap$r2,leap2$Cp)
dimnames(combine)<-list(1:15,c("age","exper","miles","towork","size","r2","Cp"))
round(combine,digits=3)

plot(leap2$size,leap2$Cp,ylim=c(1,5))
abline(a=0,b=1,col="hot pink")

# Partial F stat
reg.lm1<-lm(LOSS~AGE+TOWORK)
reg.lm2<-lm(LOSS~AGE+TOWORK+MILES+EXPER)
anova(reg.lm1,reg.lm2)

#Akaike's Information Criteria

AIC(reg.lm1)
AIC(reg.lm2)
# in this case the AIC is smaller for reduced model of reg.lm1 so this model is prefered


#Selection process
#Forward selection
slm.forward<-step(lm(LOSS~1,data=insur),scope=~AGE+EXPER+MILES+TOWORK,direction="forward")

#Backward selection
reg.lm3<-lm(LOSS~AGE+EXPER+MILES+TOWORK)
slm.backward<-step(reg.lm3,direction="backward")

#Stepwise selection
reg.lm4<-lm(LOSS~AGE+EXPER+MILES+TOWORK)
reg.stepwise<-step(reg.lm4,direction="both")

detach(insur)
#EXERCISES


#using all possible subsets selection

cars<-read.table(file=file.choose(),header=TRUE)
attach(cars)
#Using the all possible subsets selection
leap<-leaps(x=cbind(Horsepower,Length,Luggage,Uturn,Wheelbase,Width),y=MidrangePrice,method="r2",nbest=3)
leap2<-leaps(x=cbind(Horsepower,Length,Luggage,Uturn,Wheelbase,Width),y=MidrangePrice,method="Cp",nbest=3)

combine<-cbind(leap$which,leap$size,leap$r2,leap2$Cp)
dimnames(combine)<-list(1:dim(combine)[1],c("Horsepower","length","Lugagge","Uturn","Wheelbase","Width","size","r2","Cp"))
round(combine,digits=3)
plot(leap2$size,leap2$Cp,ylim=c(1,7))
abline(a=0,b=1,col="hot pink")

#using one of the automatic selection techniwues

#forward
slm.forward<-step(lm(MidrangePrice~1),scope=~Horsepower+Length+Luggage+Uturn+Wheelbase+Width,direction="forward")
#backward
reg.lm6<-lm(MidrangePrice~Horsepower+Length+Luggage+Uturn+Wheelbase+Width)
slm.backward<-step(reg.lm6,direction="backward")
#Stepwise
slm.stepwise<-step(reg.lm6,direction="both")








