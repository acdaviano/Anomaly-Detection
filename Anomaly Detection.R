nepy<-read.csv(file='NEPY.csv',header=T)#read in data.
str(nepy)# looking at the structure of the data to start maniulation.
summary(nepy)#looking at the summary for the data before starting.
plot(nepy)#looking at the data to start in order to get a general idea of what it looks like.
names(data)[2]<-c('Earthquakes') boxplot(data$Earthquakes)# we simply want to see anomalies
#in the data through a boxplot.
#I then transformed the data to be able to use the anomalydetection library and package.
Year<-NULL
Year$year<-as.data.frame(as.POSIXlt(nepy$Year,format = '%Y'))
quakes<-as.data.frame(cbind(Year$year,nepy$Earthquakes))
names(quakes)<-c('Year','NumberOfQuakes')
ggplot(quakes, aes(Year, NumberOfQuakes)) + geom_line() + scale_x_datetime() + xlab("") + ylab("Number of Earthquakes")
#we then wanted to use the AnomalyDetection package to look at the anomalies in the data
m1<-AnomalyDetectionTs(quakes,max_anoms=.02,direction='both', plot=T,na.rm = T)
m1$plot
#we only found one anomaly which is less than the boxplot found so we will lower our alpha.
m2<-AnomalyDetectionTs(quakes,max_anoms=.1,alpha =.25,direction='both', plot=T,na.rm = T)
m2$plot
#we then explore the tsoutliers package and transform the data to fit a plot for it.
library(tsoutliers)
library(forecast)
quakesTS<-ts(quakes$NumberOfQuakes,start = c(1900))
quakesTSL<-log(quakesTS)
plot(quakesTS)
plot(quakesTSL)
#we then use the ARIMA functions to look at other possible outliers and find only 1.
ts1<-tso(quakesTS,types = c("AO","LS","TC","IO",'SLS'),tsmethod = 'auto.arima')
ts1
ts2<-tso(quakesTS,types = c("AO","LS","TC","IO",'SLS'),discard.method = 'en-masse',tsmethod = 'auto.arima')
ts2
ts3<-tso(quakesTS,types = c("AO","LS","TC","IO",'SLS'),discard.method = 'bottom-up',tsmethod = 'auto.arima')
ts3
ts4<-tso(quakesTSL,types = c("AO","LS","TC","IO",'SLS'),discard.method = 'bottom-up',args.tsmethod = list(1,0,1))
ts4
#we then looked at the h2o packages results for anomalies to see if we missed anything
localH2O = h2o.init()
nepy1.hex<-as.h2o(nepy, destination_frame="train.hex")
#we then create our seed for reproducibility, and speed, as well as our frame in order
#to get our parameters.
nepy.dl = h2o.deeplearning(x = feature_names, training_frame = nepy1.hex,
                               autoencoder = TRUE,
                               reproducible = T,
                               seed = 3452,
                               hidden = c(6,5,6), epochs = 50)
#We then plot our reconstructed dataset, after createing the reconstructed one.
# prostate.anon = h2o.anomaly(prostate.dl, prostate.hex, per_feature=TRUE)
# head(prostate.anon)
nepy.anon = h2o.anomaly(nepy.dl, nepy1.hex, per_feature=FALSE)
head(nepy.anon)
err <- as.data.frame(nepy.anon)
plot(sort(err$Reconstruction.MSE), main='Reconstruction Error')