
R = 1000
n1 = c(250,500,1000)

n5 = list()
for ( n in n1 )
{
  n5 = c(n5,list(c(n-4*15,rep(15,4))))
  n5 = c(n5,list(rep(n/5,5)))
}

n10 = list()
for ( n in n1 )
{
  n10 = c(n10,list(c(n-9*15,rep(15,9))))
  n10 = c(n10,list(rep(n/10,10)))
}

n10CSLMICE = n10
n10CSLMICE[[2]] = NULL




#################
# Complete Data #
#################

R = 1000
source("GS/SimGS.R")
resGS = list()
for ( s in 1:3 )
  for ( n in n1 )
    resGS = c(resGS,list(SimGS(s,R,n,0)))




##################
# Complete Cases #
##################

R = 1000
source("CC/SimCC.R")
resCC = list()
for ( s in 1:3 )
  for ( n in n1 )
    resCC = c(resCC,list(SimCC(s,R,n,0)))




#######
# IMI #
#######

# K = 5
R = 1000
M = 20
source("IMI/SimIMI.R")
resIMI5 = list()
for ( s in 1:2 )
  for ( n in n5 )
    resIMI5 = c(resIMI5,list(SimIMI(s,R,n,M,0)))

# K = 10
R = 1000
M = 20
source("IMI/SimIMI.R")
resIMI10 = list()
for ( s in 1:2 )
  for ( n in n10 )
    resIMI10 = c(resIMI10,list(SimIMI(s,R,n,M,0)))




##########
# AVGMMI #
##########

# K = 5
R = 1000
M = 20
source("AVGMMI/SimAVGMMI.R")
resAVGMMI5 = list()
for ( s in 1:2 )
  for ( n in n5 )
    resAVGMMI5 = c(resAVGMMI5,list(SimAVGMMI(s,R,n,M,0)))

# K = 10
R = 1000
M = 20
source("AVGMMI/SimAVGMMI.R")
resAVGMMI10 = list()
for ( s in 1:2 )
  for ( n in n10 )
    resAVGMMI10 = c(resAVGMMI10,list(SimAVGMMI(s,R,n,M,0)))



#########
# CSLMI #
#########

# K = 5
R = 1000
M = 20
source("CSLMI/SimCSLMI.R")
resCSLMI5 = list()
for ( s in 1:2 )
  for ( n in n5 )
    resCSLMI5 = c(resCSLMI5,list(SimCSLMI(s,R,n,M,0)))

# K = 10
R = 1000
M = 20
source("CSLMI/SimCSLMI.R")
resCSLMI10 = list()
for ( s in 1:2 )
  for ( n in n10 )
    resCSLMI10 = c(resCSLMI10,list(SimCSLMI(s,R,n,M,0)))



########
# SIMI #
########

# K = 5
R = 1000
M = 20
source("PPMI/SimPPMI.R")
resPPMI5 = list()
for ( s in 1:2 )
  for ( n in n5 )
    resPPMI5 = c(resPPMI5,list(SimPPMI(s,R,n,M,0)))

# K = 10
R = 1000
M = 20
source("PPMI/SimPPMI.R")
resPPMI10 = list()
for ( s in 1:2 )
  for ( n in n10 )
    resPPMI10 = c(resPPMI10,list(SimPPMI(s,R,n,M,0)))




#########
# IMICE #
#########

# K = 5
R = 1000
M = 20
init = 25
iter = 10
source("IMICE/SimIMICE.R")
resIMICE5 = list()
for ( s in 3 )
  for ( n in n5 )
    resIMICE5 = c(resIMICE5,list(SimIMICE(s,R,n,M,init,iter,0)))

# K = 10
R = 1000
M = 20
init = 25
iter = 10
source("IMICE/SimIMICE.R")
resIMICE10 = list()
for ( s in 3 )
  for ( n in n10 )
    resIMICE10 = c(resIMICE10,list(SimIMICE(s,R,n,M,init,iter,0)))



############
# AVGMMICE #
############

# K = 5
R = 1000
M = 20
init = 25
iter = 10
source("AVGMMICE/SimAVGMMICE.R")
resAVGMMICE5 = list()
for ( s in 3 )
  for ( n in n5 )
    resAVGMMICE5 = c(resAVGMMICE5,list(SimAVGMMICE(s,R,n,M,init,iter,0)))

# K = 10
R = 1000
M = 20
init = 25
iter = 10
source("AVGMMICE/SimAVGMMICE.R")
resAVGMMICE10 = list()
for ( s in 3 )
  for ( n in n10 )
    resAVGMMICE10 = c(resAVGMMICE10,list(SimAVGMMICE(s,R,n,M,init,iter,0)))





###########
# CSLMICE #
###########

# K = 5
R = 1000
M = 20
init = 25
iter = 10
source("CSLMICE/SimCSLMICE.R")
resCSLMICE5 = list()
for ( s in 3 )
  for ( n in n5 )
    resCSLMICE5 = c(resCSLMICE5,list(SimCSLMICE(s,R,n,M,init,iter,0)))

# K = 10
R = 1000
M = 20
init = 25
iter = 10
source("CSLMICE/SimCSLMICE.R")
resCSLMICE10 = list()
for ( s in 3 )
  for ( n in n10CSLMICE )
    resCSLMICE10 = c(resCSLMICE10,list(SimCSLMICE(s,R,n,M,init,iter,0)))



##########
# SIMICE #
##########

# K = 5
R = 1000
M = 20
init = 25
iter = 10
source("PPMICE/SimPPMICE.R")
resPPMICE5 = list()
for ( s in 3 )
  for ( n in n5 )
    resPPMICE5 = c(resPPMICE5,list(SimPPMICE(s,R,n,M,init,iter,0)))

# K = 10
R = 1000
M = 20
init = 25
iter = 10
source("PPMICE/SimPPMICE.R")
resPPMICE10 = list()
for ( s in 3 )
  for ( n in n10 )
    resPPMICE10 = c(resPPMICE10,list(SimPPMICE(s,R,n,M,init,iter,0)))





##########
# Report #
##########


report <- function(res)
{
  L = length(res)
  # cat(" & & & method & bias & sd & rmse & \\#c\\\\\n")
  cat("Method\tBias\tSD\tRMSE\tComm\n")
  
  for ( l in 1:L )
  {
    p = dim(res[[l]]$Theta)[1]
    theta = rep(1,p)
    R = length(res[[l]]$R)
    m = apply(res[[l]]$Theta,1,mean)
    bias = sqrt(sum((m - theta)^2))
    se = sqrt(sum((res[[l]]$Theta-m)^2)/R)
    rmse = sqrt(sum((res[[l]]$Theta-theta)^2)/R)
    comm = mean(res[[l]]$comm)
    #cat(sprintf(" & & & %s & & %.3f & %.3f & %.3f & & %.1f\\\\\n",
    cat(sprintf("%s\t%.3f\t%.3f\t%.3f\t%.1f\n",
                res[[l]]$method,bias,se,rmse,comm))
  }
}



# Table 2
report(list(resGS[[1]],resGS[[2]],resGS[[3]]))
report(list(resCC[[1]],resCC[[2]],resCC[[3]]))
report(list(resIMI5[[1]],resIMI5[[3]],resIMI5[[5]]))
report(list(resAVGMMI5[[1]],resAVGMMI5[[3]],resAVGMMI5[[5]]))
report(list(resCSLMI5[[1]],resCSLMI5[[3]],resCSLMI5[[5]]))
report(list(resPPMI5[[1]],resPPMI5[[3]],resPPMI5[[5]]))
report(list(resIMI10[[1]],resIMI10[[3]],resIMI10[[5]]))
report(list(resAVGMMI10[[1]],resAVGMMI10[[3]],resAVGMMI10[[5]]))
report(list(resCSLMI10[[1]],resCSLMI10[[3]],resCSLMI10[[5]]))
report(list(resPPMI10[[1]],resPPMI10[[3]],resPPMI10[[5]]))
report(list(resIMI5[[2]],resIMI5[[4]],resIMI5[[6]]))
report(list(resAVGMMI5[[2]],resAVGMMI5[[4]],resAVGMMI5[[6]]))
report(list(resCSLMI5[[2]],resCSLMI5[[4]],resCSLMI5[[6]]))
report(list(resPPMI5[[2]],resPPMI5[[4]],resPPMI5[[6]]))
report(list(resIMI10[[2]],resIMI10[[4]],resIMI10[[6]]))
report(list(resAVGMMI10[[2]],resAVGMMI10[[4]],resAVGMMI10[[6]]))
report(list(resCSLMI10[[2]],resCSLMI10[[4]],resCSLMI10[[6]]))
report(list(resPPMI10[[2]],resPPMI10[[4]],resPPMI10[[6]]))

# Table 3
report(list(resGS[[4]],resGS[[5]],resGS[[6]]))
report(list(resCC[[4]],resCC[[5]],resCC[[6]]))
report(list(resIMI5[[7]],resIMI5[[9]],resIMI5[[11]]))
report(list(resAVGMMI5[[7]],resAVGMMI5[[9]],resAVGMMI5[[11]]))
report(list(resCSLMI5[[7]],resCSLMI5[[9]],resCSLMI5[[11]]))
report(list(resPPMI5[[7]],resPPMI5[[9]],resPPMI5[[11]]))
report(list(resIMI10[[7]],resIMI10[[9]],resIMI10[[11]]))
report(list(resAVGMMI10[[7]],resAVGMMI10[[9]],resAVGMMI10[[11]]))
report(list(resCSLMI10[[7]],resCSLMI10[[9]],resCSLMI10[[11]]))
report(list(resPPMI10[[7]],resPPMI10[[9]],resPPMI10[[11]]))
report(list(resIMI5[[8]],resIMI5[[10]],resIMI5[[12]]))
report(list(resAVGMMI5[[8]],resAVGMMI5[[10]],resAVGMMI5[[12]]))
report(list(resCSLMI5[[8]],resCSLMI5[[10]],resCSLMI5[[12]]))
report(list(resPPMI5[[8]],resPPMI5[[10]],resPPMI5[[12]]))
report(list(resIMI10[[8]],resIMI10[[10]],resIMI10[[12]]))
report(list(resAVGMMI10[[8]],resAVGMMI10[[10]],resAVGMMI10[[12]]))
report(list(resCSLMI10[[8]],resCSLMI10[[10]],resCSLMI10[[12]]))
report(list(resPPMI10[[8]],resPPMI10[[10]],resPPMI10[[12]]))


# Table 4
report(list(resGS[[7]],resGS[[8]],resGS[[9]]))
report(list(resCC[[7]],resCC[[8]],resCC[[9]]))
report(list(resIMICE5[[1]],resIMICE5[[3]],resIMICE5[[5]]))
report(list(resAVGMMICE5[[1]],resAVGMMICE5[[3]],resAVGMMICE5[[5]]))
report(list(resCSLMICE5[[1]],resCSLMICE5[[3]],resCSLMICE5[[5]]))
report(list(resPPMICE5[[1]],resPPMICE5[[3]],resPPMICE5[[5]]))
report(list(resIMICE10[[1]],resIMICE10[[3]],resIMICE10[[5]]))
report(list(resAVGMMICE10[[1]],resAVGMMICE10[[3]],resAVGMMICE10[[5]]))
#report(list(resCSLMICE10[[1]],resCSLMICE10[[3]],resCSLMICE10[[5]]))
report(list(resCSLMICE10[[1]],resCSLMICE10[[2]],resCSLMICE10[[4]]))
report(list(resPPMICE10[[1]],resPPMICE10[[3]],resPPMICE10[[5]]))
report(list(resIMICE5[[2]],resIMICE5[[4]],resIMICE5[[6]]))
report(list(resAVGMMICE5[[2]],resAVGMMICE5[[4]],resAVGMMICE5[[6]]))
report(list(resCSLMICE5[[2]],resCSLMICE5[[4]],resCSLMICE5[[6]]))
report(list(resPPMICE5[[2]],resPPMICE5[[4]],resPPMICE5[[6]]))
report(list(resIMICE10[[2]],resIMICE10[[4]],resIMICE10[[6]]))
report(list(resAVGMMICE10[[2]],resAVGMMICE10[[4]],resAVGMMICE10[[6]]))
#report(list(resCSLMICE10[[2]],resCSLMICE10[[4]],resCSLMICE10[[6]]))
report(list(resCSLMICE10[[3]],resCSLMICE10[[5]]))
report(list(resPPMICE10[[2]],resPPMICE10[[4]],resPPMICE10[[6]]))






