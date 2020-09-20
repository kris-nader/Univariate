BETACAR2<-read.table(file=file.choose(), header=TRUE)
BETACAR2_update<-read.csv(file=file.choose(),header=TRUE,sep=";")
fix(BETACAR2_update)
colnames(BETACAR2_update)=c("ID","Log.Change","Week","Prepar")
colnames(BETACAR2_update)
chick_weight=read.table(file=file.choose(),header=TRUE)
chick_weight
colnames(chick_weight)=c("Chicken_ID","Weight","Feed")
#For Weight variable
mean(chick_weight$Weight)
var(chick_weight$Weight)
sd(chick_weight$Weight)
median(chick_weight$Weight)

quantile(chick_weight$Weight)
IQR=IQR(chick_weight$Weight)
IQR
quantile(chick_weight$Weight,0.75)-quantile(chick_weight$Weight,0.25)

#For Chicken variable
mean(chick_weight$Chicken_ID)
var(chick_weight$Chicken_ID)
sd(chick_weight$Chicken_ID)
quantile(chick_weight$Chicken_ID)

summary(chick_weight)
#Does not make sense bc its the chicken ID, we can change them to ordinal
colnames(chick_weight)
chick_weight[1]="No"
#wha he did names(chick_weight)[1]="NO"


by(chick_weight$weight,chick_weight$feed,summary)
by(chick_weight$weight,chick_weight$feed,var)
by(chick_weight$weight,chick_weight$feed,sd)
by(chick_weight$weight,chick_weight$feed,quantile)
summary(chick_weight$feed)
#or table(chick_weight$feed)

##Exercise 4
monica.df<-read.table(file=file.choose(),header=TRUE,sep=";")

by(monica.df$age,monica.df$sex,summary)
by(monica.df$age,monica.df$sex,quartiles) #cannot do quartiles
by(monica.df$age,monica.df$sex,sd)
by(monica.df$age,monica.df$sex,var)

boxplot(monica.df$age)
boxplot(monica.df$age~monica.df$sex)
plot(density(monica.df$age))
bplt=by(monica.df$age,monica.df$sex,density)
par(mfrow=c(1,2))
plot(bplt$f)

plot(bplt$m)
complete_stats<-function(df){
	variance=var(df)
	mean=mean(df)
	sd=sd(df)
	median=median(df)
	q=quartiles(df)
}
by(monica.df$age, monica.df$sex, function(x){print(paste0((var(x)+" "+mean(x)))})




par(mfrow=c(1,2))
hist(monicaage[whicch(monicasex=='m'], probability=TURE,main="males", breaks=36,, ylim=c(0,0.08) xlab="age")
lines(densiry(monicaage[whichpmonica$ex=='m'