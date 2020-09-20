#compute the confidence interval knowing the sigma(SD)
west=subset(temperature, temperature$Area=='West', select=October)
n<-9
sigma_sd<-1.5
conf<-0.95
xmean<-mean(west$October)
alpha=1-conf
lower_bound=xmean-(qnorm(1-alpha/2)*sigma_sd)/sqrt(n)
upper_bound=xmean+(qnorm(1-alpha/2)*sigma_sd) / sqrt(n)
lower_bound
upper_bound
result=list(mean=xmean,lower_bound=lower_bound, upper_bound=upper_bound)
result
print(paste("The confidence interval is: [" ,result$lower_bound ,",", result$upper_bound ,"]"))

# the width of the CI will decrease as sample size increase
# the width will incrrease as SD increases
# the width increases as confidence level increases(0.5-0.9999 stronger)
# the width increases as significance levels decrease( 0.5 to 0.00001 stronger)
library(BSDA)
z.test(west$October, sigma.x=1.5)



# compute the confidence interval when sigma is not known --> we use t test with n-1 degree of freedom
t.test(west$October)

#test for normality 
# we look at the p value and alpha(0.05) pvalue > alpha then that implies
# that the distribution is not significatly different than the normal distrubution 
# we assume normality for pvalue>alpha(0.05)
shapiro.test(west$October)

# calculate sample size when sigma is known
# we never have both sample size and sigma unknown cause then we have 2 unknown
# here we need a B value to estimate the mean to within a certain amount of units
b<-0.5
n<-ceiling(((qnorm(1-alpha/2)*sigma_sd)/b)^2)
print(paste("Sample size needed to estimate the mean within ", b, "units is : " ,n))

# confidence interval for a proportion
# using the CLT the binomial distribution can be approximated with N distribution(np,npq)
# approximation works when np>=5 AND nq>=5 then instead of B(n,p)--> N(np,npq)
proportion=240/400
n=400
conf=0.95
alpha=1-conf
za=qnorm(1-alpha/2)
lower_bound=proportion-za*sqrt((proportion*(1-proportion))/n)
upper_bound=proportion+za*sqrt((proportion*(1-proportion))/n)
print(paste("The confidence interval is [",lower_bound ,",",upper_bound,"]"))

prop.test(240,400)

# Exercises
sigma=8
n=4
xmean=101.4
conf=0.99
alpha=1-conf

lower_bound=xmean-qnorm(1-alpha/2)*(sigma/sqrt(n))
upper_bound=xmean+qnorm(1-alpha/2)*(sigma/sqrt(n))

b=2
n_estimate<-ceiling((qnorm(1-alpha/2)*sigma/b)^2)
print(paste("Sample size needed to estimate the mean within ", b, "units is : " ,n_estimate))

library(BSDA)
zsum.test(mean.x=101.4, sigma.x=8,n.x=4, conf.level=conf)
