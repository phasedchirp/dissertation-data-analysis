library(coda)
library(rstan)
source("~/Dropbox/Programming/R/doing bayesian analysis/DBDA2Eprograms/DBDA2E-utilities2.R")

# define group indicators as:
# F2 = ifelse(group=="f2",1,0), etc

dataList = with(diss_testing[!is.na(diss_testing$response),],
                list(N = length(response),
                     F2 = ifelse(group=="f2",1,0),
                     F0 = ifelse(group=="f0",1,0),
                     d0 = ,
                     d2 = ,
                     y=response))



testStan <- stan_model(file="stan-files/testing.stan", model_name="testing")

testMCMC = sampling( object=testStan , 
                      data = dataList , 
                      chains = 3 ,
                      iter = 3000 , 
                      warmup = 500 , 
                      thin = 1 )

traceplot(testMCMC,pars=c("alpha"))

testCoda = mcmc.list( lapply( 1:ncol(testMCMC) , 
                              function(x) { mcmc(as.array(testMCMC)[,x,]) } ) )
diagMCMC( testCoda , parName=c("bF0") )

print(summary(testMCMC,pars=c("alpha","bF0","bF2")))

plotPost(testCoda[,1])
plotPost(testCoda[,2])
plotPost(testCoda[,3])


#---------------------------------------------------------------------------------------------
# define group indicators as:
# F2 = ifelse(group=="f2",1,0), etc

dataList = with(na.omit(diss_training),
                list(N = length(response),
                     Nsubj = length(unique(subject)),
                     S = as.numeric(subject),
                     F2 = ifelse(group=="f2",1,0),
                     F0 = ifelse(group=="f0",1,0),
                     d0 = d0,
                     d2 = d2,
                     response = response,
                     trial = trial,
                     y=diff))

# dataList = with(na.omit(diss_training),
#                 list(N = length(response),
#                      P = 6
#                      Nsubj = length(unique(subject)),
#                      S = as.numeric(subject),
#                      Dat = cbind(ifelse(group=="f2",1,0),
#                                  ifelse(group=="f0",1,0),
#                                  d0,d2,response,trial)
#                      y=diff))

trainStan <- stan_model(file="stan-files/training.stan", model_name="training")

trainMCMC = sampling( object=trainStan , 
                      data = dataList , 
                      chains = 3 ,
                      iter = 2000 , 
                      warmup = 500 , 
                      thin = 1 )

traceplot(trainMCMC,pars=c("alpha","bF2","bF0","bD2","bD0","f2i0","f0i0"))

trainCoda = mcmc.list( lapply( 1:ncol(trainMCMC) , 
                              function(x) { mcmc(as.array(trainMCMC)[,x,]) } ) )

for ( parName in c("alpha","bF2","bF0","bD2","bD0","f2i0","f0i0") ) {
  diagMCMC( codaObject=trainCoda , parName=parName)
}

# add [saveName=parName,saveType="pdf"] to arguments of diagMCMC to save plots

print(summary(trainMCMC,pars=c("alpha","bF0","bF2","bD2","bD0","f2i0","f0i0")))

plotPost(trainCoda[,"alpha"])
plotPost(trainCoda[,"bD0"],compVal=0)
plotPost(trainCoda[,"bD2"],compVal=0)


#---------------------------------------------------------------------------------------------
# messing around with no-intercept model


dataList2 = with(diss_testing[!is.na(diss_testing$response),],
                 list(N = length(response),
                      F2 = ifelse(group=="f2",1,0),
                      F0 = ifelse(group=="f0",1,0),
                      Ba = ifelse(group=="b",1,0),
                      y=response))


testStan2 <- stan_model(file="stan-files/testing_NI",model_name="test no intercept")

modelMCMC2 = sampling( object=testStan2 , 
                      data = dataList2 , 
                      chains = 3 ,
                      iter = 3000 , 
                      warmup = 500 , 
                      thin = 1 )


print(summary(modelMCMC2,pars=c("bBa","bF0","bF2")))
