#data1 = ts(JohnsonJohnson,start = c(1), end = c(84))
data1 = log(JohnsonJohnson)
d=4
par(mfrow=c(3,1))
plot.ts(data1,type="o",ylab="Death in Thousands")
data1_df_ds=ts(data1[(d+1):length(data1)]
                   -data1[1:(length(data1)-d)],
                   start(data1)+c(1,0),end(data1),4)               # 12 period difference
plot.ts(data1_df_ds,type="o",ylab="Del_12(acdDatats)")            
D1data1_df_ds=ts(data1_df_ds[2:length(data1_df_ds)]-
                   data1_df_ds[1:(length(data1_df_ds)-1)],
                     start(data1_df_ds)+c(0,1),end(data1_df_ds),4) # 1st difference
plot.ts(D1data1_df_ds,type="o",ylab="D1(Del_12(acdDatats))");abline(0,0)

library(randtests)
turning.point.test(D1data1_df_ds)
library(itsmr)
library(astsa)
acf2(D1data1_df_ds)
dowj_d1.at=autofit(D1data1_df_ds,p=0:4,q=0:4);print(dowj_d1.at)
p=length(dowj_d1.at$phi)
if(p==1 && dowj_d1.at$phi[1]==0)
  p=0
# MA order selection
q=length(dowj_d1.at$theta)
if(q==1 && dowj_d1.at$theta[1]==0)
  q=0
dowj_d1.aim = arima(D1data1_df_ds, order=c(p,0,q))    
print(dowj_d1.aim);dowj_d1.at$aicc