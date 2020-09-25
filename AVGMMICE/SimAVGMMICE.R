source("Sim/PP.R")
source("Sim/AVGM.R")

SimAVGMMICE = function(scen,R,n,M,init,iter,batch=0)
{
  K = length(n)
  method = sprintf("AVGMMICE%d",K)
  
  Theta = matrix(0,6,R)
  comm = rep(0,R)
  
  for ( r in 1:R )
  {
    message(paste("Dataset=",r+batch,sep=""))
    load(sprintf("SimData/Scen%d/data%04d",scen,r+batch))
    
    fit = AVGMMICE(d,M,init,iter,n)
    Theta[,r] = fit$theta
    comm[r] = fit$comm
  }
  
  list(method=method,R=batch+1:R,scen=scen,Theta=Theta,comm=comm)
}


AVGMMICE = function(d,M,init,iter,n)
{
  p = d$p+1
  N = sum(n)
  K = length(n)
  ni = diffinv(n)
  thetas = matrix(0,p,M)
  comm = 0
  
  d$all = cbind(d$y,1,d$Xp_obs)
  miss = is.na(d$all)
  for ( k in 1:K )
    for ( j in 3:5 )
    {
      idx1 = ni[k] + which(miss[(ni[k]+1):ni[k+1],j])
      idx0 = ni[k] + which(!miss[(ni[k]+1):ni[k+1],j])
      d$all[idx1,j] = mean(d$all[idx0,j])
    }

  for ( m in 1:M )
  {
    for ( it in 1:init )
    {
      for ( j in 3:5 )
      {
        d$all[miss[,j],j] = NA
        
        Xidx = setdiff(c(2,1,3:(p+1)),j)
        fit.imp = AVGMLS(d,j,Xidx,n)
        comm = comm + fit.imp$comm + 1
        
        for ( k in 1:K )
        {
          sig = sqrt(1/rgamma(1,(fit.imp$df+1)/2,(fit.imp$SSE+1)/2))
          alpha = fit.imp$beta + sig * backsolve(fit.imp$cgram,rnorm(p))
          idx = ni[k] + which(miss[(ni[k]+1):ni[k+1],j])
          d$all[idx,j] = d$all[idx,Xidx] %*% alpha + rnorm(length(idx),0,sig)
        }        
      }
    }
    
    init = iter

    if ( d$model == "logistic" )
      fit = PPLogit(d,1,2:(p+1),sum(n),d$theta)
    else
      fit = PPLS(d,1,2:(p+1),sum(n))
    thetas[,m] = fit$beta
  }
  
  theta = apply(thetas,1,mean)
  
  list(theta=theta,comm=comm)
}
