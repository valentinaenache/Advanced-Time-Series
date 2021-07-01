# Load libraries
library(tidyverse)
library(ggthemes)
library(forecast)
library(tseries)
library(gridExtra)
library(rugarch)
library(sos)
library(ggplot2)
findFn("select")
library(dplyr)
library(rlang)
getwd()
# Load data
stocks = read.csv('GOOGL.csv' , header = T)
qplot(x = 1:1341 , y = stocks$Close , geom = 'line') + geom_line(color = 'darkblue') + 
  labs(x = '' , y = 'Pret' , title = "Google") + geom_hline(yintercept = mean(stocks$Close) , color = 'red')

rets = diff(stocks$Close) / stocks$Close[-length(stocks$Close)]

p1 = qplot(x = 1:length(rets) , y = rets , geom = 'line') + geom_line(color = 'darkblue') + 
  geom_hline(yintercept = mean(rets) , color = 'red' , size = 1) + 
  labs(x = '' , y = 'Randamente zilnice')

p2 = qplot(rets , geom = 'density') + coord_flip() + geom_vline(xintercept = mean(rets) , color = 'red' , size = 1) +
  geom_density(fill = 'lightblue' , alpha = 0.4) + labs(x = '')

grid.arrange(p1 , p2 , ncol = 2)
# Stationaritate
adf.test(rets)

model.arima = auto.arima(rets , max.order = c(3 , 0 ,3) , stationary = TRUE , trace = T , ic = 'aicc')
model.arima

# Diagnosticarea reziduurilor
# Potrivit testului Ljung-Box, p-value < 0.05, ceea ce inseamna ca reziduurile modelului
# nu sunt independente si sunt autocorelate. 
model.arima$residuals %>% ggtsdisplay(plot.type = 'hist' , lag.max = 14)
ar.res = model.arima$residuals
Box.test(model.arima$residuals , lag = 14 , fitdf = 2 , type = 'Ljung-Box')
# GARCH model

tsdisplay(ar.res^2 , main = 'Squared Residuals')

#Another way to test the Heteroscedasticity of the squared residuals is to 
#perform significance testing on a1 and ??1 parameters.
# Model specification
model_spec = ugarchspec(variance.model = list(model = 'sGARCH' , garchOrder = c(1 , 1)) , 
                        mean.model = list(armaOrder = c(0 , 0)))

model_fit = ugarchfit(spec = model_spec , data = ar.res , solver = 'solnp')

options(scipen = 999)
model_fit@fit$matcoef
# VAR
quantile(rets , 0.05)

qplot(rets , geom = 'histogram') + geom_histogram(fill = 'lightblue' , bins = 30) +
  geom_histogram(aes(rets[rets < quantile(rets , 0.05)]) , fill = 'red' , bins = 30) +
  labs(x = 'Randamente zilnice')
jarque.bera.test(rets)

p2_1 = qplot(rets , geom = 'density') + geom_density(fill = 'blue' , alpha = 0.4) + 
  geom_density(aes(rnorm(200000 , 0 , sd(rets))) , fill = 'red' , alpha = 0.25) + 
  labs(x = '')

p2_2 = qplot(rets , geom = 'density') + geom_density(fill = 'blue' , alpha = 0.4) + 
  geom_density(aes(rnorm(200000 , 0 , sd(rets))) , fill = 'red' , alpha = 0.25) + 
  coord_cartesian(xlim = c(-0.07 , -0.02) , ylim = c(0 , 10)) + 
  geom_vline(xintercept = c(qnorm(p = c(0.01 , 0.05) , mean = mean(rets) , sd = sd(rets))) , 
             color = c('darkgreen' , 'green') , size = 1) + labs(x = 'Randamente zilnice')

grid.arrange(p2_1 , p2_2 , ncol = 1)

#t-student distribution
fitdist(distribution = 'std' , x = rets)$pars

cat("For a = 0.05 the quantile value of normal distribution is: " , 
    qnorm(p = 0.05) , "\n" ,
    "For a = 0.05 the quantile value of t-distribution is: " ,
    qdist(distribution = 'std' , shape = 2.650031504  , p = 0.05) , "\n" , "\n" , 
    'For a = 0.01 the quantile value of normal distribution is: ' , 
    qnorm(p = 0.01) , "\n" , 
    "For a = 0.01 the quantile value of t-distribution is: " , 
    qdist(distribution = 'std' , shape = 2.650031504  , p = 0.01) , sep = "")

#Garch VaR vs Delta-normal approach
qplot(y = rets , x = 1:1340 , geom = 'point') + geom_point(colour = 'lightgrey' , size = 2) + 
  geom_line(aes(y = model_fit@fit$sigma*(-1.23106) , x = 1:1340) , colour = 'red') +
  geom_hline(yintercept = sd(rets)*qnorm(0.05) , colour = 'darkgreen' , size = 1.2) + theme_light() + 
  labs(x = '' , y = 'Randamente zilnice' , title = 'Value at Risk Comparison')
#VaR forecasting
model_roll = ugarchroll(spec = model_spec , data = rets , n.start = 840 , refit.every = 50 ,
                        refit.window = 'moving')

# Test set 500 observations
VaR95_td = mean(rets) + model_roll@forecast$density[,'Sigma']*qdist(distribution='std', shape=2.650031504, p=0.05)

p = c()
p[1] = pbinom(q = 0 , size = 500 , prob = 0.05)
for(i in 1:50){
  p[i] = (pbinom(q = (i-1) , size = 500 , prob = 0.05) - pbinom(q = (i-2) , size = 500 , prob = 0.05))
  }
qplot(y = p , x = 1:50 , geom = 'line') + scale_x_continuous(breaks = seq(0 , 50 , 2)) + 
  annotate('segment' , x = c(16 , 35) , xend = c(16 , 35) , y = c(0 , 0) , yend = p[c(16 , 35)] , color = 'red' , 
           size = 1) + labs(y = 'Probability' , x = 'Number of Exceptions') + theme_light()

qplot(y = VaR95_td , x = 1:500 , geom = 'line') +
  geom_point(aes(x = 1:500 , y = rets[841:1340] , color = as.factor(rets[841:1340] < VaR95_td)) , size = 2) + scale_color_manual(values = c('gray' , 'red')) + 
  labs(y = 'Randamente zilnice' , x = 'Test set Observation') + theme_light() + 
  theme(legend.position = 'none')


cat('Number of exceptions with delta-normal approach: ' , (sum(rets[841:1340] < (mean(rets) + qnorm(p = 0.05)*sd(rets[1:840])))) , '\n' , 'Number of exceptions with GARCH approach: ' , (sum(rets[841:1340] < VaR95_td)) , sep = '')
