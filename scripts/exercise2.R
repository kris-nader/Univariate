#LAB 2
# Exercise 1
a)random variable: 1-6
b)Discrete distribution
c) 1: 1/6 2: 1/6 3: 1/6 4: 1/6 5: 1/6 6: 1/6
d) 


# Exercise 2
y=c(1:10)
y
densbin<-dbinom(y,10,0.5)
cumdens<-pbinom(y,10,0.5)
cumdens[6]]
1-cumdens[6]
qbinom(0.75,10,0.5)




# Exercise 4

z.test(41.924,sigma.x=0.3)
zsum.test(41.924,sigma.x=0.3)
zsum.test(41.924,sigma.x=0.3,n.x=10,conf.level=0.95)
zsum.test(750,sigma.x=30,n.x=20,conf.level=0.95)
pwd
pwd()
getwd()
getwd()
blood.df<-read.table(file=file.choose(),header=TRUE)
head(blood.df)
blood<-read.table(file=file.choose(), header=TRUE)
blood<-read.table(file=file.choose(), header=TRUE,delim=",")
blood<-read.table(file=file.choose(), header=TRUE,sep=",")
head(blood)
z.test(blood$age,sigma.x=5)
z.test(blood$age,sigma.x=5,conf.level=0.90)
t.tesst(blood$prolactn)
t.test(blood$prolactn)
subset.df=subset(blood,age<50)
view(subset.df)
head(subset.df)
t.test(subset.df$testost,conf.level=0.99)
history
history()