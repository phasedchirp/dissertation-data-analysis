setwd("~/Dropbox/Dissertation/dissertation-data-analysis")
library(coda)
library(rstan)
source("~/Dropbox/Programming/R/doing bayesian analysis/DBDA2Eprograms/DBDA2E-utilities2.R")
source("processing.R")

# ------------------------------------------------------------------------------
# testing block (correct/incorrect version)

dataList = with(na.omit(diss_testing),
                list(N = length(response),
                     G = 2,
                     Nsubj = length(unique(subject)),
                     S = as.numeric(subject),
                     group = cbind(
                         ifelse(group=="b",1,0),
                         ifelse(group=="f0",1,0)),
                     d0 = log(d0),
                     d2 = log(d2),
                     #d0 = log(testRat0),
                     #d2 = log(testRat2),
                     y=response))

dataList$iD0 = dataList$group*cbind(dataList$d0,dataList$d0)
dataList$iD2 = dataList$group*cbind(dataList$d2,dataList$d2)
dataList$iDiff = dataList$d0*dataList$d2
dataList$gDI = dataList$group*cbind(dataList$iDiff,dataList$iDiff)

testStan <- stan_model(file="stan-files/test2.stan", model_name="testing")

testMCMC = sampling( object=testStan , 
                     data = dataList , 
                     chains = 3 ,
                     iter = 20000 , 
                     warmup = 10000 , 
                     thin = 1 ,
                     sample_file = "test_ranef.csv")


print(testMCMC,pars=testMCMC@model_pars[!testMCMC@model_pars%in%c("b0","b2","a")],
      probs=c(0.025,0.5,0.975))
traceplot(testMCMC,pars=testMCMC@model_pars[!testMCMC@model_pars%in%c("b0","b2","a")])

testCoda = mcmc.list( lapply( 1:ncol(testMCMC) , 
                              function(x) { mcmc(as.array(testMCMC)[,x,]) } ) )
parameterNames = testMCMC@model_pars[!testMCMC@model_pars%in%c("yInt","a")]


par(mfrow=c(1,3))
plotPost(testCoda[,"bD0"],compVal=0)
title("d0 main effect\n(ref = f2-biased)")
plotPost(testCoda[,"gD0[1]"],compVal=0)
title("balanced:d0")
plotPost(testCoda[,"gD0[2]"],compVal=0)
title("f0-biased:d0")

plotPost(testCoda[,"bD2"],compVal=0)
title("d2 main effect\n(ref = f2-biased)")
plotPost(testCoda[,"gD2[1]"],compVal=0)
title("balanced:d2")
plotPost(testCoda[,"gD2[2]"],compVal=0)
title("f0-biased:d2")

# ------------------------------------------------------------------------------
# testing block (A/B version)

dataList = with(na.omit(diss_testing),
                list(N = length(response),
                     G = 2,
                     Nsubj = length(unique(subject)),
                     S = as.numeric(subject),
                     group = cbind(
                         ifelse(group=="b",1,0),
                         ifelse(group=="f0",1,0)),
                     #d0 = log(d0),
                     #d2 = log(d2),
                     d0 = log(testRat0),
                     d2 = log(testRat2),
                     y=resp.RESP-1))

dataList$iD0 = dataList$group*cbind(dataList$d0,dataList$d0)
dataList$iD2 = dataList$group*cbind(dataList$d2,dataList$d2)
dataList$iDiff = dataList$d0*dataList$d2
dataList$gDI = dataList$group*cbind(dataList$iDiff,dataList$iDiff)

testStan2 <- stan_model(file="stan-files/test2.stan", model_name="testing")

testMCMC2 = sampling( object=testStan2 , 
                     data = dataList , 
                     chains = 3 ,
                     iter = 6000 , 
                     warmup = 3000 , 
                     thin = 1 )


print(testMCMC2,pars=testMCMC2@model_pars[!testMCMC2@model_pars%in%c("yInt","a")],
      probs=c(0.025,0.5,0.975))
traceplot(testMCMC2,pars=testMCMC@model_pars[!testMCMC2@model_pars%in%c("yInt","a")])

testCoda2 = mcmc.list( lapply( 1:ncol(testMCMC2) , 
                              function(x) { mcmc(as.array(testMCMC2)[,x,]) } ) )
parameterNames = testMCMC2@model_pars[!testMCMC2@model_pars%in%c("yInt","a")]


par(mfrow=c(1,3))
plotPost(trainCoda[,"bD0"],compVal=0)
# note that F2 is the reference level when discussing
title("d0 main effect\n(ref = f2-biased)")
plotPost(trainCoda[,"gD0[1]"],compVal=0)
title("balanced:d0")
plotPost(trainCoda[,"gD0[2]"],compVal=0)
title("f0-biased:d0")

plotPost(trainCoda[,"bD2"],compVal=0)
title("d2 main effect\n(ref = f2-biased)")
plotPost(trainCoda[,"gD2[1]"],compVal=0)
title("balanced:d2")
plotPost(trainCoda[,"gD2[2]"],compVal=0)
title("f0-biased:d2")

#-------------------------------------------------------------------------------
# Training (same-different version)

dataList = with(na.omit(diss_training),
                list(N = length(response),
                     G = 3,
                     gID = ifelse(grepl("f2",unique(diss_training$subject)),1,
                                  ifelse(grepl("b",unique(diss_training$subject)),2,3)),
                     Nsubj = length(unique(subject)),
                     S = as.numeric(subject),
                     group = as.numeric(group),
                     d0 = abs(d0),
                     d2 = abs(d2),
                     response = response,
                     trialBlock = ifelse(trial<=282,0,1),
                     y=diff))

dataList$iD0 = dataList$group*cbind(dataList$d0,dataList$d0)
dataList$iD2 = dataList$group*cbind(dataList$d2,dataList$d2)
dataList$iBlock = dataList$group*cbind(dataList$trialBlock,dataList$trialBlock)
dataList$iDiff = dataList$d0*dataList$d2



#trainStan <- stan_model(file="stan-files/training3.stan", model_name="training")
trainStan <- stan_model(file="stan-files/training3_b.stan", model_name="training")


trainMCMC = sampling( object=trainStan , 
                      data = dataList , 
                      chains = 3 ,
                      iter = 6000 , 
                      warmup = 3500 , 
                      thin = 1 )#,
#                       sample_file = "train-diff-MCMC-2/train-rslopesB.csv")

print(trainMCMC,pars=trainMCMC@model_pars[!trainMCMC@model_pars%in%c("a","aD","a0","a2","aResp")],probs=c(0.025,0.5,0.975))

traceplot(trainMCMC,pars=trainMCMC@model_pars[!trainMCMC@model_pars%in%c("a","aD","a0","a2","aResp")])
par(mfrow=c(1,1))

trainCoda = mcmc.list( lapply( 1:ncol(trainMCMC) , 
                               function(x) { mcmc(as.array(trainMCMC)[,x,]) } ) )

plotPost(trainCoda[,"diff_b0"],compVal=0)
diagMCMC(trainCoda[,"diff_b0"])
plotPost(trainCoda[,"diff_i"],compVal=0)
#-------------------------------------------------------------------------------
# Training (labeling version)

dataList = with(na.omit(diss_training),
                list(N = length(response),
                     G = 2,
                     Nsubj = length(unique(subject)),
                     S = as.numeric(subject),
                     group = cbind(
                         ifelse(group=="b",1,0),
                         ifelse(group=="f0",1,0)),
                     d0 = f0,
                     d2 = f2,
                     response = prev,
                     trialBlock = ifelse(trial<=282,1,0),
                     y=response))

dataList$iD0 = dataList$group*cbind(dataList$d0,dataList$d0)
dataList$iD2 = dataList$group*cbind(dataList$d2,dataList$d2)
dataList$iBlock = dataList$group*cbind(dataList$trialBlock,dataList$trialBlock)
dataList$iDiff = dataList$d0*dataList$d2


trainStan <- stan_model(file="stan-files/training4.stan", model_name="training")

# trainOPT1 = optimizing(object=trainStan,data=dataList,as_vector=FALSE)
# trainOPT2 = optimizing(object=trainStan,data=dataList,as_vector=FALSE)
# trainOPT3 = optimizing(object=trainStan,data=dataList,as_vector=FALSE)
# optInit = list(trainOPT1$par,trainOPT2$par,trainOPT3$par)

trainMCMC = sampling( object=trainStan , 
                      data = dataList , 
                      chains = 3 ,
                      iter = 100 , 
                      warmup = 50 , 
                      thin = 1)

print(trainMCMC,pars=trainMCMC@model_pars[
    !trainMCMC@model_pars%in%c("a","b0","b2")],probs=c(0.025,0.5,0.975))

traceplot(trainMCMC,pars=trainMCMC@model_pars[!trainMCMC@model_pars%in%c("a","b0","b2")])

plot(trainMCMC,pars=trainMCMC@model_pars[!trainMCMC@model_pars%in%c("a","b0","b2")])


trainCoda = mcmc.list( lapply( 1:ncol(trainMCMC) , 
                               function(x) { mcmc(as.array(trainMCMC)[,x,]) } ) )

parameterNames = varnames(trainCoda)[!grepl("(yInt)|(a\\[)",varnames(trainCoda))]

# add [saveName=parName,saveType="pdf"] to arguments of diagMCMC to save plots

print(summary(trainMCMC,pars=c("alpha","bF0","bF2","bD2","bD0","f2i0","f0i0")))

plotPost(trainCoda[,"alpha"])
plotPost(trainCoda[,"bD0"],compVal=0)
plotPost(trainCoda[,"bD2"],compVal=0)