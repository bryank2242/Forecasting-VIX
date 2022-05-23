
### Codes for TABLE 6 (2) ###

setwd("C:/Users/BK/Desktop/Codes_VIX")


# Installing 'HDeconometrics' Package from Github
library(devtools)
install_github("gabrielrvsc/HDeconometrics")
1

library(HDeconometrics)


# Loading Data

data = read_excel("datasets/dataset_HARX(14).xlsx", na = ("remove"))
data = as.matrix(data[,-1])

Y1 = tail(data,3005)
Y3 = cbind(Y1[,1],Y1[,6:19])

npred=505


## LASSO ##
source("functions/func-lasso.R")
alpha=1

lasso1=lasso.rolling.window(Y3,npred,1,1,alpha,type="lasso")
lasso5=lasso.rolling.window(Y3npred,1,5,alpha,type="lasso")
lasso10=lasso.rolling.window(Y3,npred,1,10,alpha,type="lasso")
lasso22=lasso.rolling.window(Y3,npred,1,22,alpha,type="lasso")



## Adaptive LASSO ##

adalasso1=lasso.rolling.window(Y3,npred,1,1,alpha,type="adalasso")
adalasso5=lasso.rolling.window(Y3,npred,1,5,alpha,type="adalasso")
adalasso10=lasso.rolling.window(Y3,npred,1,10,alpha,type="adalasso")
adalasso22=lasso.rolling.window(Y3,npred,1,22,alpha,type="adalasso")



## Elastic Net ##

alpha=0.5

elasticnet1=lasso.rolling.window(Y3,npred,1,1,alpha,type="lasso")
elasticnet5=lasso.rolling.window(Y3,npred,1,5,alpha,type="lasso")
elasticnet10=lasso.rolling.window(Y3,npred,1,10,alpha,type="lasso")
elasticnet22=lasso.rolling.window(Y3,npred,1,22,alpha,type="lasso")



## Adaptive Elastic Net ##

adaelasticnet1=lasso.rolling.window(Y3,npred,1,1,alpha,type="adalasso")
adaelasticnet5=lasso.rolling.window(Y3,npred,1,5,alpha,type="adalasso")
adaelasticnet10=lasso.rolling.window(Y3,npred,1,10,alpha,type="adalasso")
adaelasticnet22=lasso.rolling.window(Y3,npred,1,22,alpha,type="adalasso")



## Complete Subset Regression (CSR) ##
source("functions/func-csr.R")

csr1=csr.rolling.window(Y3,npred,1,1)
csr5=csr.rolling.window(Y3,npred,1,5)
csr10=csr.rolling.window(Y3,npred,1,10)
csr22=csr.rolling.window(Y3,npred,1,22)



## Target Factors ##
source("functions/func-fact.R")
source("functions/func-tfact.R")
source("functions/func-baggit.R")

tfact1=tfact.rolling.window(Y3,npred,1,1)
tfact5=tfact.rolling.window(Y3,npred,1,5)
tfact10=tfact.rolling.window(Y3,npred,1,10)
tfact22=tfact.rolling.window(Y3,npred,1,22)



## Neural Network (32,16) ##
source("functions/func-nn_keras.R")

library(dplyr)
library(keras)

nn1=nn.rolling.window(Y3,npred,1,1)
nn5=nn.rolling.window(Y3,npred,1,5)
nn10=nn.rolling.window(Y3,npred,1,10)
nn22=nn.rolling.window(Y3,npred,1,22)



### Predicted Values ###

write.table(lasso1$pred,"predict_lasso1.csv", row.names = FALSE, col.names = FALSE)
write.table(adalasso1$pred,"predict_adalasso1.csv", row.names = FALSE, col.names = FALSE)
write.table(elasticnet1$pred,"predict_elasticnet1.csv", row.names = FALSE, col.names = FALSE)
write.table(adaelasticnet1$pred,"predict_adaelasticnet1.csv", row.names = FALSE, col.names = FALSE)
write.table(csr1$pred,"predict_csr1.csv", row.names = FALSE, col.names = FALSE)
write.table(tfact1$pred,"predict_tfact1.csv", row.names = FALSE, col.names = FALSE)
write.table(nn1$pred,"predict_nn1.csv", row.names = FALSE, col.names = FALSE)

