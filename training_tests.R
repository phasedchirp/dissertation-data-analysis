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
                      iter = 500 , 
                      warmup = 250 , 
                      thin = 1)

print(trainMCMC,pars=trainMCMC@model_pars[
    !trainMCMC@model_pars%in%c("a")],probs=c(0.025,0.5,0.975))
