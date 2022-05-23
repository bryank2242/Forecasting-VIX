
runcsr=function(Y,indice,lag){
  #comp=princomp(scale(Y,scale=FALSE))
  Y2=Y
  aux=embed(Y2,2+lag)
  y=aux[,indice]
  X=aux[,-c(1:(ncol(Y2)*lag))]  
  
  f.seq=seq(indice,ncol(X),ncol(Y2))
  
  if(lag==1){
    X.out=tail(aux,1)[1:ncol(X)]  
  }else{
    X.out=aux[,-c(1:(ncol(Y2)*(lag-1)))]
    X.out=tail(X.out,1)[1:ncol(X)]
  }
  
  model=HDeconometrics::csr(x=X,y,fixed.controls = f.seq)
  pred=predict(model,X.out)
  
  return(list("model"=model,"pred"=pred))
}


csr.rolling.window=function(Y,npred,indice=1,lag=1){
  
  save.pred=matrix(NA,npred,1)
  for(i in npred:1){
    Y.window=Y[(1+npred-i):(nrow(Y)-i),]
    cs=runcsr(Y.window,indice,lag)
    save.pred[(1+npred-i),]=cs$pred
    cat("iteration",(1+npred-i),"\n")
  }
  
  real=Y[,indice]
  plot(real,type="l")
  lines(c(rep(NA,length(real)-npred),save.pred),col="red")
  
  rmse=sqrt(mean((tail(real,npred)-save.pred)^2))
  mae=mean(abs(tail(real,npred)-save.pred))
  errors=c("rmse"=rmse,"mae"=mae)
  
  return(list("pred"=save.pred,"errors"=errors))
}
