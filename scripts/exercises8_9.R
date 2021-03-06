
#Exercise 1
running=read.table(file=file.choose(),header=TRUE)
fix(running)
attach(running)
reg1.f=leaps(lm(oxygen~1,data=running),scope=~age+weight+runtime+rstpulse+maxpulse,direction="forward")
library(leaps)
reg1.f=leaps(lm(oxygen~1,data=running),scope=~age+weight+runtime+rstpulse+maxpulse,direction="forward")
summary(reg.f)
summary(reg1.f)
reg1.f=step(lm(oxygen~1,data=running),scope=~age+weight+runtime+rstpulse+maxpulse,direction="forward")
reg.b=step(lm(oxygen~age+weight+rstpulse+maxpulse+runtime),direction="backward")
reg.both=step(lm(oxygen~age+weight+rstpulse+maxpulse+runtime),direction="both")
leap=leaps(x=cbind(age,weight,rstpulse,maxpulse,runtime),y=oxygen,method="r2",nbest3)
leap=leaps(x=cbind(age,weight,rstpulse,maxpulse,runtime),y=oxygen,method="r2",nbest=3)
leap2=leaps(x=cbind(age,weight,rstpulse,maxpulse,runtime),y=oxygen,method="Cp",nbest=3)
combine=cbind(leap$which,leap$size,lead$r2,leap2$Cp)
combine=cbind(leap$which,leap$size,leap$r2,leap2$Cp)
dimnames(combine)=list(1:dim(combine)[1],c("age","weight","rstpulse","maxpulse
","runtime","size","r2","Cp"))
round(combine,digits=3)
all<-lm(oxygen~age+runtime)
AIC(reg.both)
AIC(all)
history()
chol=read.table(file=file.choose(),header=TRUE)
detach(running)
ls()
fix(chol)
attach(chol)
leap=leaps(x=cbind(age,height,weight),y=cholesterol,method="r2")
names(chol)
leap=leaps(x=cbind(AGE,HEIGHT,WEIGHT),y=CHOL,method="r2")
leap2=leaps(x=cbind(AGE,HEIGHT,WEIGHT),y=CHOL,method="Cp")
combine=cbinf(leap$which,leap$size,leap$r2,leap2$Cp)
combine=cbind(leap$which,leap$size,leap$r2,leap2$Cp)
dimnames(combine)=list(1:dim(combine)[1],c("AGE","HEIGHT","WEIGHT","SIZE","R2","CP"))
leap=leaps(x=cbind(AGE,HEIGHT,WEIGHT),y=CHOL,method="r2",nbest=3)
leap2=leaps(x=cbind(AGE,HEIGHT,WEIGHT),y=CHOL,method="Cp",nbest=3)
combine=cbind(leap$which,leap$size,leap$r2,leap2$Cp)
dimnames(combine)=list(1:dim(combine)[1],c("AGE","HEIGHT","WEIGHT","SIZE","R2","CP"))
round(combine,digits=3)
all<-lm(CHOL~AGE+HEIGHT+WEIGHT)
reg1.f=step(lm(CHOL~1),scope=~AGE+HEIGHT+WEIGHT,method="forward")
reg1.bk=step(lm(CHOL~AGE+HEIGHT+WEIGHT),method="backward")
reg1.b=step(lm(CHOL~AGE+HEIGHT+WEIGHT),method="both")
round(combine,digits=3)
AIC(all)
AIC(reg1.b)

plot(leap2$Cp~leap2$size)
abline(0,1)
