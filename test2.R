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

testStan <- stan_model(file="stan-files/test3.stan", model_name="testing")

testMCMC = sampling( object=testStan , 
                     data = dataList , 
                     chains = 3 ,
                     iter = 6000 , 
                     warmup = 3000 , 
                     thin = 1 )
 

print(testMCMC,pars=testMCMC@model_pars[!testMCMC@model_pars%in%c("a")],
      probs=c(0.025,0.5,0.975))
traceplot(testMCMC,pars=testMCMC@model_pars[!testMCMC@model_pars%in%c("a")])

testCoda = mcmc.list( lapply( 1:ncol(testMCMC) , 
                              function(x) { mcmc(as.array(testMCMC)[,x,]) } ) )
parameterNames = testMCMC@model_pars[!testMCMC@model_pars%in%c("yInt","a")]

#parameterNames = varnames(testCoda)
for ( parName in parameterNames ) {
    diagMCMC( codaObject=testCoda , parName=parName)
}

library(lme4)
mlTest = glmer(response~group*log(testRat0)+group*log(testRat2)+log(testRat0):log(testRat2)+(1|subject),data=diss_testing, family="binomial")
summary(mlTest)
