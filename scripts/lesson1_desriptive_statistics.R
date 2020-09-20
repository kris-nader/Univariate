temperature=read.table(file=file.choose(),header=TRUE)
temperature=read.table(temp_warm.txt, header=TRUE)
list.files()
#trimmed mean is trimming a certain number of largest and smallest outliers 
#trimming a certain percentage of the smallest and largest values from the data. 
percentage=x
tm=c(39,92,75,61,45,87,59,51,87,12,8,93,74,16,32,39,87,12,47,50)
sort(tm)
remove=(percentage/100)*length(tm)
remove=round(remove)
mean(tm[remove:(length(tm)-remove)])
# sort by column
sort_jan=temperature[order(temperature$January),]

#if remove =1 then set the morker at 2
sort_jan_trimmed=sort_jan[(2:(length(sort_jan$January)-round)),]
new_data=sort_jan_trimmed[order(sort_jan_trimmed$row.names),]

#range
# is the difference between the largest and the smallest value in the sample.
#which.xx will give you the position of what you are looking for in thE
# Array or list 
max=which.max(temperature$January)
min=which.min(temperature$January)
range=temperature$January[max]-temperature$January[min]

#variance using R
varT=var(temperature$January)
varT
sdT=sd(temperature$October)
sdT
#interquartile range which is IQ3-IQ1(upper quartile-lower quartile)
IQRT=IQR(temperature$October)
IQRT
#also
quantile(temperature$October,0.75)-quantile(temperature$October,0.25)

#variance by myself
mean_jan=mean(temperature$January)
mean_jan
variance_lt=c()
variance_lt
for (i in 1:length(temperature$January)){
	variance_lt=c(variance_lt,((temperature$January[i]-mean_jan)^2))
}
sum=0
for( i in 1:length(variance_lt)){
	sum=sum+variance_lt[i]
}
variance=sum/(length(temperature$January)-1)

#standard deviation is the squart root of the variance
standard_dev=sqrt(variance)
standard_dev
sd_jan=sd(temperature$January)
sd_jan