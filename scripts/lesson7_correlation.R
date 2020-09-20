#Chapter 7-Correlation
warm_temp=read.table(file=file.choose(), header=TRUE)
fix(warm_temp)
names(warm_temp)
names(warm_temp)[1]
annnual<-warm_temp$annual
lat<-warm_temp$Latitude
long<-warm_temp$Longitude
combine<-data.frame(annual,lat, long)
combine<-data.frame(annnual,lat, long)
# Check a scatterplot first to see if there could be a linear relationship
pairs(combine)
cor(combine)
# this is a t test with df=n-2 and we need X and Y to be normally distributed
# for cor.tes H0: there is no linear relationship H0: there is a linear relationship
# Shapiro H0: normally distributed H1: not normally distributed
shapiro.test(annnual) # p value <0.05 normallity is rejected
shapiro.test(long) # p value < 0.05 normality rejected
shapiro.test(lat) # p value > 0.05 normality accepted  however bc one of them is not normal 
# we need to use a non parametric correlation coefficient called spearmans coefficient
cor.test(annnual,long)
cor.test(annnual,lat)
cor(combine, method="spearman")
cor.test(annnual, long, method="spearman")
cor.test(annnual, lat, method="spearman")

