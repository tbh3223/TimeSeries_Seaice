library(zoo)
library(vars)
library(tseries)

dat3=cbind(tem,sisouth,sinorth)
dat4=ts(as.matrix(dat3), start=c(1979,1), frequency=12)

#定阶
vs=VARselect(y=dat4, lag.max = 10, type = 'both',season=12)
#aic准则是2阶

#拟合模型
var =  VAR(dat4 ,p = 2, type = "both",season=12)

#Granger因果检验 
causality(var,cause=c('tem'))
causality(var,cause=c('sisouth'))
causality(var,cause=c('sinorth'))
causality(var,cause=c('sinorth','sisouth'))

#模型诊断
sta = stability(var, type = c("OLS-CUSUM"), h = 0.15, dynamic = FALSE, rescale = TRUE)
plot(sta)


#脉冲响应分析
var.irf<-irf(var)
plot(var.irf)

#预测
var.predict<-predict(var,n.ahead=24,ci=0.95)
var.predict

#提取2010.1-2015.12
plt.tem=window(temall,c(2010,1),c(2015,12))
plt.nor=window(sinorthall,c(2010,1),c(2015,12))
plt.sou=window(sisouthall,c(2010,1),c(2015,12))
par(mfrow=c(3,1))

#预测的图
plot(plt.tem,main='prediction of temperature')
pre_tem=ts(var.predict$fcst$tem[,1],start=c(2014,1),freq=12)
lines(pre_tem,type='c',col='red')
plot(plt.sou,main='prediction of sea ice in Antarctic')
pre_south=ts(var.predict$fcst$sisouth[,1],start=c(2014,1),freq=12)
lines(pre_south,type='c',col='red')
plot(plt.nor,main='prediction of sea ice in Arctic')
pre_north=ts(var.predict$fcst$sinorth[,1],start=c(2014,1),freq=12)
lines(pre_north,type='c',col='red')


#误差
ori_tem=window(temall,c(2014,1),c(2015,12))
ori_north=window(sinorthall,c(2014,1),c(2015,12))
ori_south=window(sisouthall,c(2014,1),c(2015,12))

sum((var.predict$fcst$tem[,1]-c(ori_tem))^2)/24
sum((var.predict$fcst$sisouth[,1]-c(ori_south))^2)/24
sum((var.predict$fcst$sinorth[,1]-c(ori_north))^2)/24



