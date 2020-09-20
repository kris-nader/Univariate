#one sided and 2 sided tests
library(BSDA)
#non parametric test is a test of the medians
# when sigma is known
west<-subset(warm_temp,warm_temp$Area=='West', select=October)
z.test(west$October, alternative="less",mu=12,sigma.x=1.5,conf.level=0.95)
z.test(west$October, alternative="greater",mu=12,sigma.x=1.5,conf.level=0.95)
z.test(west$October, alternative="two.sided",mu=12,sigma.x=1.5,conf.level=0.95)
#pnorm()
# when sigma is unknown
# need to check for normality--Y: 1 sample t test N: transform or nonparametric
shapiro.test(west$October) # ho: normal h1: not normal with a=0.05
# case where shapiro is normal
t.test(west$October,mu=12,conf.level=0.95, alternative="less")
# left sided pt(-2.1608,8)right sided 1-pt(-2.1608,8)
# 2 sided 2*(1-pt(0.0011,50))

# when shapiro test proves the alternative hypothesis we use binom.test or SIGN.test
# remember that the non parametric tests are not that powerful
#p value pbinom(x,n,p) and the p here is between 2 options: so temp above 12 and equal to 12
sum<-sum(west$October-12>0)
binom.test(sum,9, alternative="less")
pbinom(sum,9,0.5)
#OR we can use BSDA
libray(BSDA)
SIGN.test(west$October, md=12, alternative="less")


# 2 independent samples: 2 sample t test
# first we check normality
west=subset(warm_temp,warm_temp$Area==West,select="annual")
east=subset(warm_temp,warm_temp$Area==East,select="annual")
# both must be normal so that we proceed with a 2 sample independent t test
shapiro.test(west$Annual)
shapiro.test(east$Annual)
# then we need to check variances h0: equal variance
var.test(west$annual,east$annual)
# then we use the t test for equal variances 
t.test(west$annual, east$annual, var.equal=TRUE)
# if variances do not equal
t.test(west$annual, east$annual, var.equal=FALSE)
# IN CASE NORMALILTY FAILS FOR ONE OR BOTH THEN WE USE NON PARAMETRIC WILCOXON SIGN SUM TEST
# wilcoxon is a test for similar distributions h0: same distribution h1 : shifted 
westeastT<-subset(warm_temp, warm_temp$Area=='West'|warm_temp$Area=='East', select=c(annual,area))
install.packages("doBy")
library("doBy")
sortannual<-orderBy(~annual, data=westeast)
wilcoxon.test(west$annual,east$annual)
# visualize with boxplots



# PAIRED t-test --> relies on assumption that they are normally distributed
shapiro.test(data1)
shapiro.test(data2)
differnece=data1-data2
shapiro.test(difference)
t.test(data1, data2, paired=TRUE, alternative="greater")
#if normality is rejected for the difference we use a non parametric distribution free test

sum=sum(difference>0)
binom.test(sum,n,alternative="greater")
# or we can use sign test from BSDA
library("BSDA")
SIGN.test(data1,data2,alternative="greater"


# test for proportions
n<-length(warm_temp$annual)
number<-sum(temperature$annual>10)
number
prop.test(number,n,p=0.5)
#or we can use binom.test
binom.test(number,n,p=0.5,alternative="two.sided")

# hypotheis for 2 proportions



#Chiq test ho= equal distributions
birthday<-c(10,12,10,16)
predprob<-seq(0.25,0.25,0.25,0.25)

chisq.test(birthday,p=predprob)