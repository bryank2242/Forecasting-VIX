
### Codes for TABLE 6 (1) ###

setwd("C:/Users/BK/Desktop/Codes_VIX")

library(readxl)

npred=505

## Random Walk Model
source("functions/func-rw_1.R")

data = read_excel("datasets/dataset_HARX(14).xlsx", na = ("remove"))
data = as.matrix(data[,-1])

Y1 = tail(data,3005)

rw1=rw.rolling.window(Y1,npred,1,1)
rw5=rw.rolling.window(Y1,npred,1,5)
rw10=rw.rolling.window(Y1,npred,1,10)
rw22=rw.rolling.window(Y1,npred,1,22)



## HAR & HARX Model
source("functions/func-ar_111.R")

harx1 = ar.rolling.window(Y1,npred,1,1,type="fixed")
harx5 = ar.rolling.window(Y1,npred,1,5,type="fixed")
harx10 = ar.rolling.window(Y1,npred,1,10,type="fixed")
harx22 = ar.rolling.window(Y1,npred,1,22,type="fixed")


Y2 = Y1[,1:5]

har1 = ar.rolling.window(Y2,npred,1,1,type="fixed")
har5 = ar.rolling.window(Y2,npred,1,5,type="fixed")
har10 = ar.rolling.window(Y2,npred,1,10,type="fixed")
har22 = ar.rolling.window(Y2,npred,1,22,type="fixed")



## ARX Model
source("functions/func-ar_112.R")

Y3 = cbind(Y1[,1],Y1[,6:19])

arx1 = ar.rolling.window(Y3,npred,1,1,type="fixed")
arx5 = ar.rolling.window(Y3,npred,1,5,type="fixed")
arx10 = ar.rolling.window(Y3,npred,1,10,type="fixed")
arx22 = ar.rolling.window(Y3,npred,1,22,type="fixed")



## Random Forest (14)
source("functions/func-rf_111.R")

library(randomForest)
library(foreach)
library(doSNOW)
registerDoSNOW(makeCluster(3, type="SOCK")) # For parallel computing


rf1_14=rf.rolling.window(Y3,npred,1,1)
rf5_14=rf.rolling.window(Y3,npred,1,5)
rf10_14=rf.rolling.window(Y3,npred,1,10)
rf22_14=rf.rolling.window(Y3,npred,1,22)



## Random Forest (298)

data3 = read_excel("datasets/dataset_0203.xlsx", na = ("remove"))
data3 = as.matrix(data3[,-1])
dim(data3)

a = log(data3[-1,1])
b = data3[-1,2:11]
c = as.matrix(data3[,12:299])
c <- diff(log(c))

Y5 = as.matrix(cbind(a,b,c))
Y5 = tail(Y5,3005)


rf1_298=rf.rolling.window(Y5,npred,1,1)
rf5_298=rf.rolling.window(Y5,npred,1,5)
rf10_298=rf.rolling.window(Y5,npred,1,10)
rf22_298=rf.rolling.window(Y5,npred,1,22)




### Variable Selection using Boruta and Rolling Window

library(Boruta)

Y = Y5                  ## Using the Whole Sample for Boruta
#Y = Y5[1:2500,]        ## Using only the First Window for Boruta


## 
lag = 1

aux = embed(Y,2+lag)    ## Using 2 lags of each Variables  
y=aux[,1]
X=aux[,-c(1:(ncol(Y)*lag))]

boruta <- Boruta(X, y, maxRuns = 100)

attstats1 = attStats(boruta)
plot(boruta)

# Cross Validation for Optimal Number of Variables

varOrder = order(attstats1$meanImp, decreasing = T)

Errors = rep(NA,30)          

for (i in 2:30){
  
  selected = varOrder[1:i]
  
  model=randomForest(X[,selected], y, importance=TRUE)
  
  pred = model$predicted     
  error = mean((pred-y)^2)
  
  Errors[i] <- error
}

plot(c(1:30), Errors1, xlab="# of Variables", ylab="Fitted Squared Error")
Errors1 = Errors

# Rolling Window with Selected Variables
source("functions/func-rf_selected.R")

varOrder = order(attstats1$meanImp, decreasing = T)   # Ordering of Variables
which.min(Errors1)                                    # Optimal Number of Variables 
selected = varOrder[1:which.min(Errors1)]             # The Set of Optimal Number of Variables

rf1_selected = rf.rolling.window(Y5,npred,1,1,selected)



##
lag = 5

aux = embed(Y,2+lag)    ## Using 2 lags of each Variables  
y=aux[,1]
X=aux[,-c(1:(ncol(Y)*lag))]

boruta <- Boruta(X, y, maxRuns = 100)

attstats5 = attStats(boruta)
plot(boruta)

# Cross Validation for Optimal Number of Variables

varOrder = order(attstats5$meanImp, decreasing = T)

Errors = rep(NA,30)          

for (i in 2:30){
  
  selected = varOrder[1:i]
  
  model=randomForest(X[,selected], y, importance=TRUE)
  
  pred = model$predicted     
  error = mean((pred-y)^2)
  
  Errors[i] <- error
}

plot(c(1:30), Errors, xlab="# of Variables", ylab="Fitted Squared Error")
Errors5 = Errors

# Rolling Window with Selected Variables
source("functions/func-rf_selected.R")

varOrder = order(attstats5$meanImp, decreasing = T)   # Ordering of Variables
which.min(Errors5)                                    # Optimal Number of Variables 
selected = varOrder[1:which.min(Errors5)]             # The Set of Optimal Number of Variables

rf5_selected = rf.rolling.window(Y5,npred,1,5,selected)



##
lag = 10

aux = embed(Y,2+lag)    ## Using 2 lags of each Variables  
y=aux[,1]
X=aux[,-c(1:(ncol(Y)*lag))]

boruta <- Boruta(X, y, maxRuns = 100)

attstats10 = attStats(boruta)
plot(boruta)

# Cross Validation for Optimal Number of Variables

varOrder = order(attstats10$meanImp, decreasing = T)

Errors = rep(NA,30)          

for (i in 2:30){
  
  selected = varOrder[1:i]
  
  model=randomForest(X[,selected], y, importance=TRUE)
  
  pred = model$predicted     
  error = mean((pred-y)^2)
  
  Errors[i] <- error
}

plot(c(1:30), Errors, xlab="# of Variables", ylab="Fitted Squared Error")
Errors10 = Errors

# Rolling Window with Selected Variables
source("functions/func-rf_selected.R")

varOrder = order(attstats10$meanImp, decreasing = T)   # Ordering of Variables
which.min(Errors10)                                    # Optimal Number of Variables 
selected = varOrder[1:which.min(Errors10)]             # The Set of Optimal Number of Variables

rf10_selected = rf.rolling.window(Y5,npred,1,10,selected)



##
lag = 22

aux = embed(Y,2+lag)    ## Using 2 lags of each Variables  
y=aux[,1]
X=aux[,-c(1:(ncol(Y)*lag))]

boruta <- Boruta(X, y, maxRuns = 100)

attstats22 = attStats(boruta)
plot(boruta)

# Cross Validation for Optimal Number of Variables

varOrder = order(attstats22$meanImp, decreasing = T)

Errors = rep(NA,30)          

for (i in 2:30){
  
  selected = varOrder[1:i]
  
  model=randomForest(X[,selected], y, importance=TRUE)
  
  pred = model$predicted     
  error = mean((pred-y)^2)
  
  Errors[i] <- error
}

plot(c(1:30), Errors, xlab="# of Variables", ylab="Fitted Squared Error")
Errors22 = Errors

# Rolling Window with Selected Variables
source("functions/func-rf_selected.R")

varOrder = order(attstats22$meanImp, decreasing = T)   # Ordering of Variables
which.min(Errors22)                                    # Optimal Number of Variables 
selected = varOrder[1:which.min(Errors22)]             # The Set of Optimal Number of Variables

rf22_selected = rf.rolling.window(Y5,npred,1,22,selected)



### Predicted Values ###

write.table(rw1$pred,"predict_rw1_second.csv", row.names = FALSE, col.names = FALSE)
write.table(arx1$pred,"predict_arx1_second.csv", row.names = FALSE, col.names = FALSE)
write.table(har1$pred,"predict_har1_second.csv", row.names = FALSE, col.names = FALSE)
write.table(harx1$pred,"predict_harx1_second.csv", row.names = FALSE, col.names = FALSE)
write.table(rf1_14$pred,"predict_rf1_14_second.csv", row.names = FALSE, col.names = FALSE)
write.table(rf1_298$pred,"predict_rf1_298_second.csv", row.names = FALSE, col.names = FALSE)
write.table(rf1_selected$pred,"predict_rf1_selected_second.csv", row.names = FALSE, col.names = FALSE)
