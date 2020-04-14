library("caret")
library("ggplot2")
library("GGally")
library("scatterplot3d")
setwd("/home/simranh/Desktop/DataSc")
mydata=read.csv(file="real_estate.csv", header=TRUE)

rs_data=data.frame(mydata)
head(rs_data)
any(is.na(rs_data))
any(is.character(rs_data))
summary(rs_data)

Latitd=(rs_data$latitude)
HousePrice=(rs_data$house_price_of_unit_area)
Longtd=(rs_data$longitude)
no.conv.stores=(rs_data$number_of_convenience_stores)
HouseAge=(rs_data$house_age)
ggpairs(data=rs_data, columns = 2:8, title="Real Estate Data")

#graphical analysis
med.Lat=median(Latitd)
med.Long=median(Longtd)
med.conv=median(no.conv.stores)
med.age=median(HouseAge)

hist(Latitd, main="Distribution of observations", xlab="Latitude", col="aquamarine3", border="white")
abline(v=med.Lat, col="red", lwd=2)
hist(Longtd, main="Distribution of observations", xlab="Longitude", col="coral1", border="white")
abline(v=med.Long, col="blue", lwd=2)
hist(no.conv.stores, main="Distribution of observations", xlab="No. of stores", col="cornflowerblue", border="white")
abline(v=med.conv, col="red", lwd=2)
hist(HouseAge, main="Distribution of observations", xlab="Age of house", col="darkolivegreen3", border="white")
abline(v=med.age, col="blue", lwd=2)




ggplot(data = rs_data, aes(x = Longtd , y = HousePrice)) +
  geom_point() +
  stat_smooth(method = "lm", col = "dodgerblue3") +
  theme(panel.background = element_rect(fill = "white"),
        axis.line.x=element_line(),
        axis.line.y=element_line()) +
  ggtitle("Linear Model Fitted to Data")

ggplot(data = rs_data, aes(x = no.conv.stores, y = HousePrice)) +
  geom_point() +
  stat_smooth(method = "lm", col = "deeppink") +
  theme(panel.background = element_rect(fill = "white"),
        axis.line.x=element_line(),
        axis.line.y=element_line()) +
  ggtitle("Linear Model Fitted to Data")

ggplot(data = rs_data, aes(x = Latitd, y = HousePrice)) +
  geom_point() +
  stat_smooth(method = "lm", col = "darkviolet") +
  theme(panel.background = element_rect(fill = "white"),
        axis.line.x=element_line(),
        axis.line.y=element_line()) +
  ggtitle("Linear Model Fitted to Data")

ggplot(data = rs_data, aes(x = HouseAge, y = HousePrice)) +
  geom_point() +
  stat_smooth(method = "lm", col = "darkorange1") +
  theme(panel.background = element_rect(fill = "white"),
        axis.line.x=element_line(),
        axis.line.y=element_line()) +
  ggtitle("Linear Model Fitted to Data")

#splitting the data into training and test sets

trainIndex=createDataPartition(rs_data$No, p=0.7,list=FALSE)
training=rs_data[trainIndex,]
testing=rs_data[-trainIndex,]

linearMod=lm(HousePrice ~ I(Longtd^2)+ I(Latitd^3)+I(no.conv.stores^5), data=training)


plot( Longtd,HousePrice, main='Linear regression',col="deepskyblue4",pch=19)
lines(smooth.spline(Longtd,predict(linearMod)), col='red', lwd='5')    #to add the regression line to the plot

plot(no.conv.stores,HousePrice, main='Linear regression',col='deepskyblue4',pch=19)
lines(smooth.spline(no.conv.stores,predict(linearMod)), col='green', lwd='5')

plot(Latitd,HousePrice, main='Linear regression',col='deepskyblue4',pch=19)
lines(smooth.spline(Latitd,predict(linearMod)), col='coral', lwd='5')   

plot(HouseAge,HousePrice, main='Linear regression',col='deepskyblue4',pch=19)
lines(smooth.spline(HouseAge,predict(linearMod)), col='darkgoldenrod1', lwd='5')    


summary(linearMod)

ggplot(data=rs_data,aes(linearMod$residuals))+
geom_histogram(binwidth=3,color="white", fill="hotpink")+
theme(panel.background = element_rect(fill = "white"),
axis.line.x=element_line(),
axis.line.y=element_line()) +
ggtitle("Histogram for Model Residuals")



pred=predict(linearMod,testing)
head(pred)
actual_pred=data.frame(cbind(actuals=testing$house_price_of_unit_area,predicteds=pred))

min_max_accuracy= mean(apply(actual_pred,1,min)/apply(actual_pred,1,max))
min_max_accuracy

MAPE=mean(abs((actual_pred$predicteds - actual_pred$actuals)) / actual_pred$actuals)
MAPE    #mean absolute percentage error

