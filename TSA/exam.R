data1 = read.csv("E:/impo/TSA/LabTestDataQ.csv",header = FALSE)
data1
plot(data1,type = "o")

library(forecast)

library(itsmr)
data2 = ts(data1$V2,start = c(1),end = c(84),frequency = 1)
plot.ts(data2)
#acf2(data2[1:32])
#acf2(data2[45:84])

#data2_at = autofit(data2[1:32],p=0:5,q=0:5)
#data2_at

auto.arima(data2)

data2_aim = arima(data2[1:32],order = c(2,0,2))
data2_aim

data2_aim_pred = predict(data2_aim,n.ahead = 12)
data2_aim_pred

data2_aim_pred$pred
as.vector(data2_aim_pred$pred)
data1$V2[33]=1.13384591
data1$V2[34]=-0.25554934
data1$V2[35]=-1.05115956
data1$V2[36]=-0.68130139
data1$V2[37]=0.28450933
data1$V2[38]=0.87482234
data1$V2[39]=0.65175867
data1$V2[40]=-0.01792371
data1$V2[41]=-0.45359565
data1$V2[42]=-0.32300059
data1$V2[43]=0.14016003
data1$V2[44]=0.46016510
data1$V2

data3 = read.csv("E:/impo/TSA/LabTestData.csv",header = FALSE)
data3$V2

mse = sum((data3$V2-data1$V2)^2)
