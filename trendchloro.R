png("chltrend.png",width = 28,height = 12,units = "cm",res = 600)
setwd("~/Fathi/CHLOROPHYLL")
library(raster)
chl<-stack("chlorophyll.nc")
a=c(77,78,7,8)
acrop<-crop(chl,a)
#converting it to points
abox<-rasterToPoints(acrop)
#eliminating first and second coloum(lat$long)
a_el<-abox[,c(-1,-2)]
#eliminating the nulls
a_na<-na.omit(a_el)
#finding the mean of columns
a<-colMeans(a_el,na.rm = TRUE)
#extract julien date from data
adate<- names(a)
#the X character of julien date is removed
datea1<- as.numeric(substr(adate,2,11))
#convert unix time stamp to gregorian date
a_unixtime<-as.POSIXct(datea1,origin = "1970-01-01")
#date and upwelling index is converted to data frame
a_chlor<- data.frame(dates=a_unixtime,a)
#upwelling months are extracted ,june-september
trendmonths<- c("-05-","-06-","-07-","-08-","-09-","-10-")
#plyr package is used to arrange the dates accordinginly
library(plyr)
#trend months are filtered(filters the months -06-","-07-","-08-","-09)
chloro<- ldply(lapply(trendmonths,function(x)a_chlor[grep(x,a_chlor$dates),]))
chloroph<-chloro[order(as.Date(chloro$dates, format="%d/%m/%Y")),]
par(mfrow=c(2,3))
plot(chloroph$a,type="l",xaxt="n",xlab="Year",ylab="Chlorophyll",ylim=c(0,22),main="CHLOROPHYLL TREND-A")
axis(1,at = seq(from=1,to=84,by=6),labels = 2003:2016)
x<-1:84
trendfita1<- lm(chloroph$a[1:36]~x[1:36])
lines(x[1:36],predict(trendfita1),col="red",lwd=2)
trendfita2<- lm(chloroph$a[36:60]~x[36:60])
lines(x[36:60],predict(trendfita2),col="red",lwd=2)
trendfita3<- lm(chloroph$a[60:84]~x[60:84])
lines(x[60:84],predict(trendfita3),col="red",lwd=2)
trenda<- lm(chloroph$a[1:84]~x[1:84])
lines(x[1:84],predict(trenda),col="darkblue",lwd=2)


trendfita1$fitted.values[length(trendfita1$fitted.values)]-trendfita1$fitted.values[1]
trendfita2$fitted.values[length(trendfita2$fitted.values)]-trendfita2$fitted.values[1]
trendfita3$fitted.values[length(trendfita3$fitted.values)]-trendfita3$fitted.values[1]
trenda$fitted.values[length(trenda$fitted.values)]-trenda$fitted.values[1]


#b
a=rbind(c(76.2,7.5),c(77.2,8),c(76.5,9.5),c(75.5,9))

acrop<-crop(chl,a)
#converting it to points
abox<-rasterToPoints(acrop)
#eliminating first and second coloum(lat$long)
a_el<-abox[,c(-1,-2)]
#eliminating the nulls
a_na<-na.omit(a_el)
#finding the mean of columns
a<-colMeans(a_el,na.rm = TRUE)
#extract julien date from data
adate<- names(a)
#the X character of julien date is removed
datea1<- as.numeric(substr(adate,2,11))
#convert unix time stamp to gregorian date
a_unixtime<-as.POSIXct(datea1,origin = "1970-01-01")
#date and upwelling index is converted to data frame
a_chlor<- data.frame(dates=a_unixtime,a)
#upwelling months are extracted ,june-september
trendmonths<- c("-05-","-06-","-07-","-08-","-09-","-10-")
#plyr package is used to arrange the dates accordinginly
library(plyr)
#trend months are filtered(filters the months -06-","-07-","-08-","-09)
chloro<- ldply(lapply(trendmonths,function(x)a_chlor[grep(x,a_chlor$dates),]))
chloroph<-chloro[order(as.Date(chloro$dates, format="%d/%m/%Y")),]
plot(chloroph$a,type="l",xaxt="n",xlab="Year",ylab="Chlorophyll",ylim=c(0,22),main="CHLOROPHYLL TREND-B")
axis(1,at = seq(from=1,to=84,by=6),labels = 2003:2016)
x<-1:84
trendfita1<- lm(chloroph$a[1:36]~x[1:36])
lines(x[1:36],predict(trendfita1),col="red",lwd=2)
trendfita2<- lm(chloroph$a[36:60]~x[36:60])
lines(x[36:60],predict(trendfita2),col="red",lwd=2)
trendfita3<- lm(chloroph$a[60:84]~x[60:84])
lines(x[60:84],predict(trendfita3),col="red",lwd=2)
trenda<- lm(chloroph$a[1:84]~x[1:84])
lines(x[1:84],predict(trenda),col="darkblue",lwd=2)


trendfita1$fitted.values[length(trendfita1$fitted.values)]-trendfita1$fitted.values[1]
trendfita2$fitted.values[length(trendfita2$fitted.values)]-trendfita2$fitted.values[1]
trendfita3$fitted.values[length(trendfita3$fitted.values)]-trendfita3$fitted.values[1]
trenda$fitted.values[length(trenda$fitted.values)]-trenda$fitted.values[1]

#c
a<-rbind(c(75.5,9),c(76.5,9.5),c(76.1,10.5),c(75.1,10))
acrop<-crop(chl,a)
#converting it to points
abox<-rasterToPoints(acrop)
#eliminating first and second coloum(lat$long)
a_el<-abox[,c(-1,-2)]
#eliminating the nulls
a_na<-na.omit(a_el)
#finding the mean of columns
a<-colMeans(a_el,na.rm = TRUE)
#extract julien date from data
adate<- names(a)
#the X character of julien date is removed
datea1<- as.numeric(substr(adate,2,11))
#convert unix time stamp to gregorian date
a_unixtime<-as.POSIXct(datea1,origin = "1970-01-01")
#date and upwelling index is converted to data frame
a_chlor<- data.frame(dates=a_unixtime,a)
a_chlor<-a_chlor[c(-67),]
#upwelling months are extracted ,june-september
trendmonths<- c("-05-","-06-","-07-","-08-","-09-","-10-")
#plyr package is used to arrange the dates accordinginly
library(plyr)
#trend months are filtered(filters the months -06-","-07-","-08-","-09)
chloro<- ldply(lapply(trendmonths,function(x)a_chlor[grep(x,a_chlor$dates),]))
chloroph<-chloro[order(as.Date(chloro$dates, format="%d/%m/%Y")),]

plot(chloroph$a,type="l",xaxt="n",xlab="Year",ylab="Chlorophyll",ylim=c(0,22),main="CHLOROPHYLL TREND-C")
axis(1,at = seq(from=1,to=84,by=6),labels = 2003:2016)
x<-1:84
trendfita1<- lm(chloroph$a[1:36]~x[1:36])
lines(x[1:36],predict(trendfita1),col="red",lwd=2)
trendfita2<- lm(chloroph$a[36:60]~x[36:60])
lines(x[36:60],predict(trendfita2),col="red",lwd=2)
trendfita3<- lm(chloroph$a[60:83]~x[60:83])
lines(x[60:83],predict(trendfita3),col="red",lwd=2)
trenda<- lm(chloroph$a[1:83]~x[1:83])
lines(x[1:83],predict(trenda),col="darkblue",lwd=2)


trendfita1$fitted.values[length(trendfita1$fitted.values)]-trendfita1$fitted.values[1]
trendfita2$fitted.values[length(trendfita2$fitted.values)]-trendfita2$fitted.values[1]
trendfita3$fitted.values[length(trendfita3$fitted.values)]-trendfita3$fitted.values[1]
trenda$fitted.values[length(trenda$fitted.values)]-trenda$fitted.values[1]
#d
a<-rbind(c(75.1,10),c(76.1,10.5),c(75,12.5),c(74,12))
acrop<-crop(chl,a)
#converting it to points
abox<-rasterToPoints(acrop)
#eliminating first and second coloum(lat$long)
a_el<-abox[,c(-1,-2)]
#eliminating the nulls
a_na<-na.omit(a_el)
#finding the mean of columns
a<-colMeans(a_el,na.rm = TRUE)
#extract julien date from data
adate<- names(a)
#the X character of julien date is removed
datea1<- as.numeric(substr(adate,2,11))
#convert unix time stamp to gregorian date
a_unixtime<-as.POSIXct(datea1,origin = "1970-01-01")
#date and upwelling index is converted to data frame
a_chlor<- data.frame(dates=a_unixtime,a)
#upwelling months are extracted ,june-september
trendmonths<- c("-05-","-06-","-07-","-08-","-09-","-10-")
#plyr package is used to arrange the dates accordinginly
library(plyr)
#trend months are filtered(filters the months -06-","-07-","-08-","-09)
chloro<- ldply(lapply(trendmonths,function(x)a_chlor[grep(x,a_chlor$dates),]))
chloroph<-chloro[order(as.Date(chloro$dates, format="%d/%m/%Y")),]
cl<-na.omit(chloroph)
plot(cl$a,type="l",xaxt="n",xlab="Year",ylab="Chlorophyll",ylim=c(0,22),main="CHLOROPHYLL TREND-D")
tkpt=c("1","6","12","18","23","28","33","39","45","51","57","63","68","74")
axis(1,at = tkpt,labels = 2003:2016)
x<-1:80
trendfita1<- lm(cl$a[1:28]~x[1:28])
lines(x[1:28],predict(trendfita1),col="red",lwd=2)
trendfita2<- lm(cl$a[28:57]~x[28:57])
lines(x[28:57],predict(trendfita2),col="red",lwd=2)
trendfita3<- lm(cl$a[57:80]~x[57:80])
lines(x[57:80],predict(trendfita3),col="red",lwd=2)
trenda<- lm(cl$a[1:80]~x[1:80])
lines(x[1:80],predict(trenda),col="darkblue",lwd=2)
trendfita1$fitted.values[length(trendfita1$fitted.values)]-trendfita1$fitted.values[1]
trendfita2$fitted.values[length(trendfita2$fitted.values)]-trendfita2$fitted.values[1]
trendfita3$fitted.values[length(trendfita3$fitted.values)]-trendfita3$fitted.values[1]
trenda$fitted.values[length(trenda$fitted.values)]-trenda$fitted.values[1]


#e
a<-rbind(c(74,12),c(75,12.5),c(74.3,14.5),c(73.3,14))
acrop<-crop(chl,a)
#converting it to points
abox<-rasterToPoints(acrop)
#eliminating first and second coloum(lat$long)
a_el<-abox[,c(-1,-2)]
#eliminating the nulls
a_na<-na.omit(a_el)
#finding the mean of columns
a<-colMeans(a_el,na.rm = TRUE)
#extract julien date from data
adate<- names(a)
#the X character of julien date is removed
datea1<- as.numeric(substr(adate,2,11))
#convert unix time stamp to gregorian date
a_unixtime<-as.POSIXct(datea1,origin = "1970-01-01")
#date and upwelling index is converted to data frame
a_chlor<- data.frame(dates=a_unixtime,a)
#upwelling months are extracted ,june-september
trendmonths<- c("-05-","-06-","-07-","-08-","-09-","-10-")
#plyr package is used to arrange the dates accordinginly
library(plyr)
#trend months are filtered(filters the months -06-","-07-","-08-","-09)
chloro<- ldply(lapply(trendmonths,function(x)a_chlor[grep(x,a_chlor$dates),]))
chloroph<-chloro[order(as.Date(chloro$dates, format="%d/%m/%Y")),]
cl<-na.omit(chloroph)
c<-data.frame(date=cl$dates,cl$a,sno=1:length(cl$dates))
plot(cl$a,type="l",xaxt="n",xlab="Year",ylab="Chlorophyll",ylim=c(0,22),main="CHLOROPHYLL TREND-E")
tkpt=c("1","7","12","18","23","28","33","38","44","49","55","60","65","69")
axis(1,at = tkpt,labels = 2003:2016)
x<-1:80
trendfita1<- lm(cl$a[1:28]~x[1:28])
lines(x[1:28],predict(trendfita1),col="red",lwd=2)
trendfita2<- lm(cl$a[28:55]~x[28:55])
lines(x[28:55],predict(trendfita2),col="red",lwd=2)
trendfita3<- lm(cl$a[55:73]~x[55:73])
lines(x[55:73],predict(trendfita3),col="red",lwd=2)
trenda<- lm(cl$a[1:73]~x[1:73])
lines(x[1:73],predict(trenda),col="darkblue",lwd=2)

trendfita1$fitted.values[length(trendfita1$fitted.values)]-trendfita1$fitted.values[1]
trendfita2$fitted.values[length(trendfita2$fitted.values)]-trendfita2$fitted.values[1]
trendfita3$fitted.values[length(trendfita3$fitted.values)]-trendfita3$fitted.values[1]
trenda$fitted.values[length(trenda$fitted.values)]-trenda$fitted.values[1]


#f

a<-rbind(c(73.3,14),c(74.3,14.5),c(73.5,16.5),c(72.5,16))
acrop<-crop(chl,a)
#converting it to points
abox<-rasterToPoints(acrop)
#eliminating first and second coloum(lat$long)
a_el<-abox[,c(-1,-2)]
#eliminating the nulls
a_na<-na.omit(a_el)
#finding the mean of columns
a<-colMeans(a_el,na.rm = TRUE)
#extract julien date from data
adate<- names(a)
#the X character of julien date is removed
datea1<- as.numeric(substr(adate,2,11))
#convert unix time stamp to gregorian date
a_unixtime<-as.POSIXct(datea1,origin = "1970-01-01")
#date and upwelling index is converted to data frame
a_chlor<- data.frame(dates=a_unixtime,a)
#upwelling months are extracted ,june-september
trendmonths<- c("-05-","-06-","-07-","-08-","-09-","-10-")
#plyr package is used to arrange the dates accordinginly
library(plyr)
#trend months are filtered(filters the months -06-","-07-","-08-","-09)
chloro<- ldply(lapply(trendmonths,function(x)a_chlor[grep(x,a_chlor$dates),]))
chloroph<-chloro[order(as.Date(chloro$dates, format="%d/%m/%Y")),]
cl<-na.omit(chloroph)
c<-data.frame(date=cl$dates,cl$a,sno=1:length(cl$dates))
plot(cl$a,type="l",xaxt="n",xlab="Year",ylab="Chlorophyll",ylim=c(0,22),main="CHLOROPHYLL TREND-F")
tkpt=c("1","5","9","15","20","26","31","35","40","45","50","54","60","65")
axis(1,at = tkpt,labels = 2003:2016)
x<-1:80
trendfita1<- lm(cl$a[1:26]~x[1:26])
lines(x[1:26],predict(trendfita1),col="red",lwd=2)
trendfita2<- lm(cl$a[26:50]~x[26:50])
lines(x[26:50],predict(trendfita2),col="red",lwd=2)
trendfita3<- lm(cl$a[50:69]~x[50:69])
lines(x[50:69],predict(trendfita3),col="red",lwd=2)
trenda<- lm(cl$a[1:69]~x[1:69])
lines(x[1:69],predict(trenda),col="darkblue",lwd=2)
trendfita1$fitted.values[length(trendfita1$fitted.values)]-trendfita1$fitted.values[1]
trendfita2$fitted.values[length(trendfita2$fitted.values)]-trendfita2$fitted.values[1]
trendfita3$fitted.values[length(trendfita3$fitted.values)]-trendfita3$fitted.values[1]
trenda$fitted.values[length(trenda$fitted.values)]-trenda$fitted.values[1]


dev.off()
