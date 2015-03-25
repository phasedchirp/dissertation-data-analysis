dataList = with(na.omit(diss_testing),
                list(N1 = length(response[na.omit(diss_testing)$group=="f2"]),
                     N2 = length(response[na.omit(diss_testing)$group=="f0"]),
                     N3 = length(response[na.omit(diss_testing)$group=="b"]),
                     d0A_f2 = d0A[na.omit(diss_testing)$group=="f2"],
                     d2A_f2 = d2A[na.omit(diss_testing)$group=="f2"],
                     d0B_f2 = d0B[na.omit(diss_testing)$group=="f2"],
                     d2B_f2 = d2B[na.omit(diss_testing)$group=="f2"],
                     yF2=response[na.omit(diss_testing)$group=="f2"],
                     d0A_f0 = d0A[na.omit(diss_testing)$group=="f0"],
                     d2A_f0 = d2A[na.omit(diss_testing)$group=="f0"],
                     d0B_f0 = d0B[na.omit(diss_testing)$group=="f0"],
                     d2B_f0 = d2B[na.omit(diss_testing)$group=="f0"],
                     yF0=response[na.omit(diss_testing)$group=="f0"],
                     d0A_b = d0A[na.omit(diss_testing)$group=="b"],
                     d2A_b = d2A[na.omit(diss_testing)$group=="b"],
                     d0B_b = d0B[na.omit(diss_testing)$group=="b"],
                     d2B_b = d2B[na.omit(diss_testing)$group=="b"],
                     yBA=response[na.omit(diss_testing)$group=="b"]))



testStan <- stan_model(file="stan-files/testMod", model_name="testing")

testMCMC = sampling( object=testStan , 
                     data = dataList , 
                     chains = 3 ,
                     iter = 5000 , 
                     warmup = 500 , 
                     thin = 1 )

traceplot(testMCMC)

testCoda = mcmc.list( lapply( 1:ncol(testMCMC) , 
                              function(x) { mcmc(as.array(testMCMC)[,x,]) } ) )

for ( parName in c("alpha","bF2","bF0","bD2A","bD0A","bD2B","bD0B") ) {
    diagMCMC( codaObject=testCoda , parName=parName)
}

parameterNames = varnames(testCoda)
for ( parName in parameterNames ) {
    diagMCMC( codaObject=testCoda , parName=parName)
}

plotPost(testCoda[,"b2B_f2"],compVal=0)
