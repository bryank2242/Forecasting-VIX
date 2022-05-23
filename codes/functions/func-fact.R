runfact=function(Y,indice,lag){
  #comp=princomp(scale(Y,scale=FALSE))
  Y2=Y
  aux=embed(Y2,2+lag)
  y=aux[,1]
  X=aux[,-c(1:(ncol(Y2)*lag))]  
  
  if(lag==1){
    X.out=tail(aux,1)[1:ncol(X)]  
  }else{
    X.out=aux[,-c(1:(ncol(Y2)*(lag-1)))]
    X.out=tail(X.out,1)[1:ncol(X)]
  }
  
  bb=Inf
  for(i in seq(5,20,5)){
    m=lm(y~X[,1:i])
    crit=BIC(m)
    if(crit<bb){
      bb=crit
      model=m
      f.coef=coef(model)
    }
  }
  coef=rep(0,ncol(X)+1)
  coef[1:length(f.coef)]=f.coef
  
  pred=c(1,X.out)%*%coef
  
  return(list("model"=model,"pred"=pred,"coef"=coef))
}



fact.rolling.window=function(Y,npred,indice=1,lag=1){
  
  save.coef=matrix(NA,npred,21)
  save.pred=matrix(NA,npred,1)
  for(i in npred:1){
    Y.window=Y[(1+npred-i):(nrow(Y)-i),]
    fact=runfact(Y.window,indice,lag)
    #save.coef[(1+npred-i),]=fact$coef
    save.pred[(1+npred-i),]=fact$pred
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

