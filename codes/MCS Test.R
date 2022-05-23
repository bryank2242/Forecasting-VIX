
### Model Confidence Set Test (Hansen, 2011) ###


# The code is set for the 5-days-ahead setting, and the corresponding numbers should be changed for other horizon settings.

setwd("C:/Users/BK/Desktop/Codes_VIX")

library(readxl)
library(MCS)


### First Sample ###

## Predicted Values
rw5 = as.matrix(read.csv('predict/first sample/predict_rw5.csv', header=F))
arx5 = as.matrix(read.csv('predict/first sample/predict_arx5.csv', header=F))
har5 = as.matrix(read.csv('predict/first sample/predict_har5.csv', header=F))
harx5 = as.matrix(read.csv('predict/first sample/predict_harx5.csv', header=F))
rf5_14 = as.matrix(read.csv('predict/first sample/predict_rf5_14.csv', header=F))
rf5_298 = as.matrix(read.csv('predict/first sample/predict_rf5_298.csv', header=F))
rf5_selected = as.matrix(read.csv('predict/first sample/predict_rf5_selected.csv', header=F))

PRED=cbind(rw5,arx5,har5,harx5,rf5_14,rf5_298,rf5_selected)

colnames(PRED) <- c("rw","arx","har","harx","rf_14", "rf_298", "rf_selected")


## Actual Values
data = read_excel("datasets/dataset_HARX(14).xlsx", na = ("remove"))
data = as.matrix(data[,-1])

Y=data[-(1:66),]
real = Y[2501:5740,1]

REAL=cbind(real, real, real, real, real, real, real)

## Loss Matrix
LOSS=PRED-REAL
LOSS1=LOSS^2
LOSS2=abs(LOSS)

## The MCS Test
SSM <- MCSprocedure(LOSS1, alpha=0.05, B=5000, statistic="Tmax")




### Second Sample ###

## Predicted Values
rw5 = as.matrix(read.csv('predict/second sample/predict_rw5_second.csv', header=F))
arx5 = as.matrix(read.csv('predict/second sample/predict_arx5_second.csv', header=F))
har5 = as.matrix(read.csv('predict/second sample/predict_har5_second.csv', header=F))
harx5 = as.matrix(read.csv('predict/second sample/predict_harx5_second.csv', header=F))
lasso5 = as.matrix(read.csv('predict/second sample/predict_lasso5.csv', header=F))
adalasso5 = as.matrix(read.csv('predict/second sample/predict_adalasso5.csv', header=F))
elasticnet5 = as.matrix(read.csv('predict/second sample/predict_elasticnet5.csv', header=F))
adaelasticnet5 = as.matrix(read.csv('predict/second sample/predict_adaelasticnet5.csv', header=F))
csr5 = as.matrix(read.csv('predict/second sample/predict_csr5.csv', header=F))
tfact5 = as.matrix(read.csv('predict/second sample/predict_tfact5.csv', header=F))
nn5 = as.matrix(read.csv('predict/second sample/predict_nn5.csv', header=F))
rf5_14 = as.matrix(read.csv('predict/second sample/predict_rf5_14_second.csv', header=F))
rf5_298 = as.matrix(read.csv('predict/second sample/predict_rf5_298_second.csv', header=F))
rf5_selected = as.matrix(read.csv('predict/second sample/predict_rf5_selected_second.csv', header=F))

# Actual Values
real = tail(Y[,1],505)

REAL=cbind(real, real, real, real, real, real, real, real, real, real, real, real, real, real)

PRED5 = cbind(rw5,arx5,har5,harx5,lasso5,adalasso5,elasticnet5,adaelasticnet5,csr5,tfact5,nn5,rf5_14,rf5_298,rf5_selected)

colnames(PRED5) <- c("rw","arx","har","harx","lasso", "adalasso", "elasticnet", "adaelasticnet", "csr", "tfact", "nn", 
                    "rf_14", "rf_298", "rf_selected")

LOSS=PRED5-REAL
LOSS1=LOSS^2
LOSS2=abs(LOSS)

SSM <- MCSprocedure(Loss=LOSS1, alpha=0.5, B=5000, statistic="Tmax")
