#binomial distribution
binomial_dis<-function(y,n,p){
	q<-1-p
	prob<-((factorial(n))/(factorial(y)*factorial(n-y)))*p^(y)*(q^(n-y))
	return(prob)
}
dbinom(y,n,p)

k=c(0:3)
cumulative_binomial<-function(k,n,p,c_bino){
	c_bino=c()
	for(i in 1:length(k)){
		temp=binomial_dis(n,p,k[i])
		if(i==1)c_bino=c(c_bino,temp)else c_bino=c(c_bino,(temp+c_bino[i-1]))
	}
	return (c_bino)
}
cumulative_binomial(k,3,0.25,c_bino)
cumdens <- pbinom(k,3,0.25)


barplot(dens,xlab="y",ylab="probability", col="lightpink")

#poisson distribution
poisson_dis<-function(l,y){
	prob=(((l)^y)*exp(-l))/factorial(y)
	return (prob)
}
dpois(17,lambda=12)

cumulative_poisson<-function(k,l){
	c_pois=c()
	for(i in 1:length(k)){
		temp=poisson_dis(l,k[i])
		if(i==1)c_pois=c(c_pois,temp)else c_pois=c(c_pois,(temp+c_pois[i-1]))
	}
	return (c_pois)
}