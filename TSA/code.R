data1 = ts(JohnsonJohnson,start = c(1), end = c(84))

# Plotting -------

par(mfrow=c(1,2))
plot.ts(data1,type = "o")
plot.ts(log(data1), type = "o")

## From the plot we can say the data follows multiplicative model. So we will work with log of the data.
data1 = log(data1)

# Est Trend by Moving Avg -------

data2 = c(rep(data1[1],2),data1,rep(data1[84],2))
data2_tr = filter(data2,filter = rep(1/5,5),sides = 2)
#data2_tr = data2_tr[-c(1,2,87,88)]
data2_tr = ts(na.omit(data2_tr),start = c(1), end = c(84))
data2_tr_rem = data1 - data2_tr
par(mfrow=c(1, 1))
plot.ts(data2_tr_rem,type = "o");abline(0,0)

# Seasonality removing -------

data2_tr_rem = matrix(data2_tr_rem,ncol = 4,byrow = T)
data2_sa=(colMeans(data2_tr_rem,na.rm = TRUE)-mean(colMeans(data2_tr_rem,na.rm = TRUE)))
data2_sa_rem = ts(as.vector(data2_tr_rem - data2_sa),start = c(1), end = c(84))
plot.ts(data2_sa_rem,type = "o");abline(0,0)

# Trend removing -------

data2_sa_rem1 = c(rep(data2_sa_rem[1],2),data2_sa_rem,rep(data2_sa_rem[84],2))
data2_tr2 = filter(data2_sa_rem1,filter = rep(1/5,5),sides = 2)
data2_tr2 = ts(na.omit(data2_tr2),start = c(1), end = c(84))
data2_tr2_rem = data2_sa_rem - data2_tr2
plot.ts(data2_tr2_rem,type = "o");abline(0,0)

# Testing for Randomness -------

data2_res = data2_tr2_rem
library(randtests)
turning.point.test(data2_res)

# ACF,PACF & fitting -------

acf(data2_res)
pacf(data2_res)
library(astsa)
acf2(data2_res)
