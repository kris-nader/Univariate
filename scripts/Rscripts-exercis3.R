#all inferences are done about the ppopulation and not the sample
Belgium=c(160,128,169,105,151,162,177,185,150,182,158,123,141,176,162,172)
Iran=c(128,125,133,104,146,132,125,118,129,124)
xmeanB=mean(Belgium)
xmeanI=mean(Iran)
#1
shapiro.test(Belgium)
shapiro.test(Iran)
#h0: iran=belgium h1: iran<belgium
#check variances equal or not
#b
var.test(Iran,Belgium)
#c d
t.test(Iran, Belgium, var.equal=TRUE)
#e
t.test(Iran, Belgium ,var.equal=FALSE,alternative="less",conf.level=0.90)
# does not include 0 so we can reject the null hypothesis

#exercise 2
blood.df=read.table(file=file.choose(),header=TRUE, sep=(","))
blood.Y<-subset(blood.df,age<50)
blood.O<-subset(blood.df,age>68)

#check normality
shapiro.test(blood.O$testost)
shapiro.test(blood.Y$testost)
#assume normal

#then use t test
# cannot reject that there is =
t.test(blood.Y$testost, blood.O$testost)


#Exercise 3
install.packages("faraway")
library("faraway")
tvdoctor.df=tvdoctor
shapiro.test(tvdoctor$life)
shapiro.test(tvdoctor$tv)
shapiro.test(tvdoctor$doctor)
cor(tvdoctor.df, method="spearman")# more tv less life
pairs(tvdoctor)
#correlation does not imply causation

#Exercise 4
senic.df=read.table(file=file.choose(),header=TRUE, sep=("\t"))
summary(senic.df)
shapiro.test(senic.df$length)
shapiro.test(senic.df$risk)
shapiro.test(senic.df$fac)
shapiro.test(senic.xray)
k=data.frame(senic.df$length, senic.df$risk, senic.df$fac, senic.df$xray)
summary(k)
pairs(k)
plot(k)
var(senic.df$length)

cor(k, method="spearman")


#look for outliers
boxplot(
# and look at density curves to see skew
plot(density(senic.df$length))

senic.lml=lm(senic.df$risk~senic.df$length, data=senic.df)

summary(senic.lml)
# the intercept is when x=0 and the infection risk is the alpha then people that dont go to the hospital will already have a risk of 0.77

the f value is the t value ^2

