runlasso=function(Y,indice,lag,alpha=1,type="lasso"){
  #comp=princomp(scale(Y,scale=FALSE))
  Y2=Y
  aux=embed(Y2,2+lag)
  y=aux[,indice]
  X=aux[,-c(1:(ncol(Y2)*lag))]  
  
  
  
  if(lag==1){
    X.out=tail(aux,1)[1:ncol(X)]  
  }else{
    X.out=aux[,-c(1:(ncol(Y2)*(lag-1)))]
    X.out=tail(X.out,1)[1:ncol(X)]
  }
  
  
  
  model=ic.glmnet(X,y,alpha = alpha)
  coef=model$coef
  if(type=="adalasso"){
    penalty=(abs(coef[-1])+1/sqrt(length(y)))^(-1)
    model=ic.glmnet(X,y,penalty.factor = penalty,alpha=alpha)
  }
  
  if(type=="fal"){
    taus=c(seq(0.1,1,0.1),1.25,1.5,2,3,4,5,7,10)
    alphas=seq(0,1,0.1)
    bb=Inf
    for(alpha in alphas){
      m0=ic.glmnet(X,y,alpha = alpha)
      coef=m0$coef
      for(tau in taus){
        penalty=(abs(coef[-1])+1/sqrt(length(y)))^(-tau)
        m=ic.glmnet(X,y,penalty.factor = penalty)
        crit=m$bic
        if(crit<bb){
          model=m
          bb=crit
        }
      }
    }
  }
  pred=predict(model,X.out)
  
  return(list("model"=model,"pred"=pred))
}


lasso.rolling.window=function(Y,npred,indice=1,lag=1,alpha=1,type="lasso"){
  
  save.coef=matrix(NA,npred,21+ncol(Y[,-1])*4)
  save.pred=matrix(NA,npred,1)
  for(i in npred:1){
    Y.window=Y[(1+npred-i):(nrow(Y)-i),]
    lasso=runlasso(Y.window,indice,lag,alpha,type)
    #save.coef[(1+npred-i),]=lasso$model$coef
    save.pred[(1+npred-i),]=lasso$pred
    cat("iteration",(1+npred-i),"\n")
  }
  
  real=Y[,indice]
  plot(real,type="l")
  lines(c(rep(NA,length(real)-npred),save.pred),col="red")
  
  rmse=sqrt(mean((tail(real,npred)-save.pred)^2))
  mae=mean(abs(tail(real,npred)-save.pred))
  errors=c("rmse"=rmse,"mae"=mae)
  
  return(list("pred"=save.pred,"coef"=save.coef,"errors"=errors))
}

