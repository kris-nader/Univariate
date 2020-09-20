#Hypothesis testing for the mean when the standard deviation is known
#Exercise 1
temperature=read.table(file=file.choose(), header=TRUE)
west<-subset(temperature,temperature$Area=="West",select=October)
xmean<-mean(west$October)
xmean=10.94
mu0=12
alpha=0.05
sigma=1.5
n=length(west$October)
n=9
zobs=(xmean-mu0)/(sigma/sqrt(n))
zobs
pvalue=pnorm(zobs)
pvalue
make_decision<- function(pvalue,alpha){
if(pvalue>alpha){
	print("Accept the null hypothesis")
} else {
print("Reject the null hypothesis")
}
}
xmean=10.94444
lower_bound=xmean-qnorm(1-alpha/2)*sigma/sqrt(n)
upper_bound=xmean+qnorm(1-alpha/2)*sigma/sqrt(n)
lower_bound
upper_bound
mu0=10
set_bound<-function(lower_bound, upper_bound, mu0){
if( mu0>lower_bound && mu0<upper_bound){
	print("Accept Null Hypothesis")
} else {
	print("Reject Null Hypothesis")
}
}
set_bound(lower_bound,upper_bound,mu0)
#Exercise 2
mu0=10
alpha=0.05
sigma=1.5
xmean=10.94
n=9
zobs=(xmean-mu0)/(sigma/sqrt(n))
zobs
t_sided_p=2*(1-pnorm(zobs))
t_sided_p
make_decision(t_sided_p,alpha)


#Exercise 3
mu0=0
alpha=0.05
sigma=0.5
n=27
xmean=1.893883
zobs<-(xmean-mu0)/(sigma/sqrt(n))
zobs
o_sided_p=1-pnorm(zobs)
o_sided_p
make_decision(o_sided_p,alpha)

#Exercise 4 
mu0=0
alpha=0.05
sigma=0.25
xmean=5.37037e-05
n=27
zobs=(xmean-mu0)/(sigma/sqrt(n))
zobs
t_sided_p=2*(1-pnorm(abs(zobs)))
t_sided_p
make_decision(t_sided_p,alpha)

#compute the confidence interval
lower_bound=xmean-qnorm(1-alpha/2)*(sigma/sqrt(n))
upper_bound=xmean+qnorm(1-alpha/2)*(sigma/sqrt(n))
set_bound<-function(lower_bound, upper_bound, mu0){
if( mu0>lower_bound && mu0<upper_bound){
	print("Accept Null Hypothesis")
} else {
	print("Reject Null Hypothesis")
}
}
set_bound(lower_bound,upper_bound,mu0)




##USING THE BSDA
library(BSDA)
# one sided
z.test(west$October, alternative="less", mu=12, sigma.x=1.5, conf.level=0.95)
# two sided
z.test(west$October, alternative="two.sided", mu=10, sigma.x=1.5, conf.level=0.95)



##WHEN sigma is not known
t.test(west$October, mu=12, conf.level=0.95, alternative="less")
# default t.test is two.sided

