source("Sim/PP.R")

SimCC <- function(scen,R,n,batch=0)
{
  method = "CC"
  
  if ( scen == 3 )
    Theta = matrix(0,6,R)
  else
    Theta = matrix(0,3,R)
  comm = rep(0,R)
  
  for ( r in 1:R )
  {
    message(paste("Dataset=",r+batch,sep=""))
    load(sprintf("SimData/Scen%d/data%04d",scen,r+batch))
    
    d$all = cbind(d$y,1,d$Xp_obs)
    
    if ( d$model == "logistic" )
      fit = PPLogit(d,1,2:(d$p+2),n,d$theta)
    else
      fit = PPLS(d,1,2:(d$p+2),n)
    Theta[,r] = fit$beta
    comm[r] = fit$comm
  }
  
  list(method=method,R=batch+1:R,scen=scen,Theta=Theta,comm=comm)
}
