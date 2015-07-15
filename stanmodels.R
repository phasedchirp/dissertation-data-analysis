library(coda)
library(rstan)
source("DBDA2E-utilities2.R")
source("processing.R")

# ------------------------------------------------------------------------------
# Discrimination Task Analysis

dataList = with(na.omit(diss_testing),
                list(N = length(response),
                     G = 2,
                     Nsubj = length(unique(subject)),
                     S = as.numeric(subject),
                     group = cbind(
                         ifelse(group=="b",1,0),
                         ifelse(group=="f0",1,0)),
                     d0 = d0,
                     d2 = d2,
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
                     iter = 8000 , 
                     warmup = 4000 , 
                     thin = 1 ,
                     sample_file = "test-rslopes.csv")


print(testMCMC,pars=testMCMC@model_pars[!testMCMC@model_pars%in%c("b0","b2","a")],
      probs=c(0.025,0.5,0.975))
traceplot(testMCMC,pars=testMCMC@model_pars[!testMCMC@model_pars%in%c("b0","b2","a")])

testCoda = mcmc.list( lapply( 1:ncol(testMCMC) , 
                              function(x) { mcmc(as.array(testMCMC)[,x,]) } ) )
parameterNames = testMCMC@model_pars[!testMCMC@model_pars%in%c("yInt","a")]



plotPost(testCoda[,"mu_a"],compVal=0)
plotPost(testCoda[,"b0_mu"],compVal=0)
plotPost(testCoda[,"b2_mu"],compVal=0)
plotPost(testCoda[,"bDiff"],compVal=0)

plotPost(testCoda[,"aG[2]"],compVal=0)
plotPost(testCoda[,"gD0[2]"],compVal=0)
plotPost(testCoda[,"gD2[2]"],compVal=0)
plotPost(testCoda[,"gDiff[2]"],compVal=0)

plotPost(testCoda[,"aG[1]"],compVal=0)
plotPost(testCoda[,"gD0[1]"],compVal=0)
plotPost(testCoda[,"gD2[1]"],compVal=0)
plotPost(testCoda[,"gDiff[1]"],compVal=0)



plotPost(testCoda[,"bD2"],compVal=0)
title("d2 main effect\n(ref = f2-biased)")
plotPost(testCoda[,"gD2[1]"],compVal=0)
title("balanced:d2")
plotPost(testCoda[,"gD2[2]"],compVal=0)
title("f0-biased:d2")



#-------------------------------------------------------------------------------
# Labeling Task Analysis

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


trainStan <- stan_model(file="stan-files/training3_b.stan", model_name="training")


trainMCMC = sampling( object=trainStan , 
                      data = dataList , 
                      chains = 3 ,
                      iter = 5000 , 
                      warmup = 2500 , 
                      thin = 1 ,
                      sample_file = "train-rslopes.csv")

print(trainMCMC,pars=trainMCMC@model_pars[!trainMCMC@model_pars%in%c("a","aD","a0","a2","aResp")],probs=c(0.025,0.5,0.975))

traceplot(trainMCMC,pars=trainMCMC@model_pars[!trainMCMC@model_pars%in%c("a","aD","a0","a2","aResp")])
par(mfrow=c(1,1))

trainCoda = mcmc.list( lapply( 1:ncol(trainMCMC) , 
                               function(x) { mcmc(as.array(trainMCMC)[,x,]) } ) )

plotPost(trainCoda[,"alpha[1]"],compVal=0)
plotPost(trainCoda[,"b0[1]"],compVal=0)
plotPost(trainCoda[,"b2[1]"],compVal=0)
plotPost(trainCoda[,"bD[1]"],compVal=0)
plotPost(trainCoda[,"aBlock[1]"],compVal=0)
plotPost(trainCoda[,"nA"],compVal=30)

plotPost(trainCoda[,"diff_F0"],compVal=0)
plotPost(trainCoda[,"diff_b0f"],compVal=0)
plotPost(trainCoda[,"diff_b2f"],compVal=0)
plotPost(trainCoda[,"diff_if"],compVal=0)
plotPost(trainCoda[,"diff_bf"],compVal=0)

plotPost(trainCoda[,"diff_B"],compVal=0)
plotPost(trainCoda[,"diff_b0b"],compVal=0)
plotPost(trainCoda[,"diff_b2b"],compVal=0)
plotPost(trainCoda[,"diff_ib"],compVal=0)
plotPost(trainCoda[,"diff_bb"],compVal=0)
#-------------------------------------------------------------------------------
