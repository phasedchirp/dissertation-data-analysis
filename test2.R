# Find cases with apparent reversals:
library(lme4)
test = lmList(response~d0*d2+block|subject,data=diss_training,family="binomial")
coef(test)

#flips = c("f0E1","f0E2","f0E3","f0E4","f0E6","f0E7","f0E8","f0OE2","f0OE4","bE6","bE9","bOE2","bOE3")
dataList = with(na.omit(diss_training),
                list(N = length(response),
                     G = 3,
                     gID = ifelse(grepl("f2",unique(diss_training$subject)),1,
                           ifelse(grepl("b",unique(diss_training$subject)),2,3)),
                     Nsubj = length(unique(subject)),
                     S = as.numeric(subject),
                     group = as.numeric(group),
                     d0 = f0,
                     d2 = f2,
                     prev = prev2,
                     trialBlock = ifelse(trial<=282,0,1),
                     y=resp2))

dataList$iD0 = dataList$group*cbind(dataList$d0,dataList$d0)
dataList$iD2 = dataList$group*cbind(dataList$d2,dataList$d2)
dataList$iBlock = dataList$group*cbind(dataList$trialBlock,dataList$trialBlock)
dataList$iDiff = dataList$d0*dataList$d2




trainStan <- stan_model(file="stan-files/training4.stan", model_name="training")


trainMCMC = sampling( object=trainStan , 
                      data = dataList , 
                      chains = 3 ,
                      iter = 6000 , 
                      warmup = 4000 , 
                      thin = 1 ,
                      sample_file = "train-resp-MCMC/train-rslopes.csv")

print(trainMCMC,pars=trainMCMC@model_pars[!trainMCMC@model_pars%in%c("a","aD","a0","a2","aResp")],probs=c(0.025,0.5,0.975))

traceplot(trainMCMC,pars=trainMCMC@model_pars[!trainMCMC@model_pars%in%c("a","aD","a0","a2","aResp")])
par(mfrow=c(1,1))

trainCoda = mcmc.list( lapply( 1:ncol(trainMCMC) , 
                               function(x) { mcmc(as.array(trainMCMC)[,x,]) } ) )

plotPost(trainCoda[,"diff_b0"],compVal=0)
diagMCMC(trainCoda[,"diff_b0"])
