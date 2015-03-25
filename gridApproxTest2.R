#testCoda = read_stan_csv()
library(runjags)
testCoda = mcmc.list( lapply( 1:ncol(testMCMC) , 
                              function(x) { mcmc(as.array(testMCMC)[,x,]) } ) )
testCoda = combine.mcmc(testCoda)
parameterNames = varnames(testCoda)[!grepl("a\\[",varnames(testCoda))]

range2 = range(dataList$d2,na.rm=T)
comb = seq(range2[1],range2[2],0.02)

#-------------------------------------------------------------------------------
qPred2 = function(){
  pred2 = numeric(length=length(comb))
  for(i in 1:length(testCoda[,1])){
    pars = testCoda[i,parameterNames]
    pred2 = rbind(pred2,
          (-pars[10]/(pars[3]+comb*pars[5]) - comb*pars[4]/(pars[3]+comb*pars[5])))
  }
  out = data.frame(d2 = comb)
  out$lower = apply(pred2,2,quantile,probs=0.025)
  out$m = apply(pred2,2,quantile,probs=0.5)
  out$upper = apply(pred2,2,quantile,probs=0.975)
  return(out)
}

#-------------------------------------------------------------------------------
qPredB = function(){
  predB = NULL
  for(i in 1:length(testCoda[,1])){
    pars = testCoda[i,parameterNames]
    predB = rbind(predB,
          -(pars[10]+pars[1])/(pars[6]+pars[3]+comb*pars[5]) - comb*(pars[4]+pars[8])/(pars[6]+pars[3]+comb*(pars[5]+pars[12])))
  }
  out = data.frame(d2 = comb)
  out$lower = apply(predB,2,quantile,probs=0.025)
  out$m = apply(predB,2,quantile,probs=0.5)
  out$upper = apply(predB,2,quantile,probs=0.975)
  return(out)
}

#-------------------------------------------------------------------------------
qPred0 = function(){
  pred0 = NULL
  for(i in 1:length(testCoda[,1])){
    pars = testCoda[i,parameterNames]
    pred0 = rbind(pred0,
          -(pars[10]+pars[2])/(pars[7]+pars[3]+comb*pars[5]) - comb*(pars[4]+pars[9])/(pars[7]+pars[3]+comb*(pars[5]+pars[13])))
  }
  out = data.frame(d2 = comb)
  out$lower = apply(pred0,2,quantile,probs=0.025)
  out$m = apply(pred0,2,quantile,probs=0.5)
  out$upper = apply(pred0,2,quantile,probs=0.975)
  return(out)
}
#-------------------------------------------------------------------------------
test2 = qPred2()
test2$group = "f2"
testB = qPredB()
testB$group= "b"
test0 = qPred0()
test0$group="f0"

plotData = with(na.omit(diss_testing),data.frame(d0=log(d0),
                                        d2=log(d2),
                                        group=group))

plotData = droplevels(subset(plotData,group%in%c("f0","f2")))
bounds = rbind(test2,test0)
#bounds = rbind(test2,testB,test0)
bounds$group=factor(bounds$group,levels=c("f2","f0"))
bounds$d0 = bounds$m
#bounds$group=factor(bounds$group,levels=c("f2","b","f0"))

cbPalette <- c("#999999", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
plot = ggplot(data=plotData,aes(x=d2,y=d0,color=group)) +
    geom_point(alpha=0.2,shape=20) +
    geom_line(data=bounds,size=2) +
    geom_ribbon(data=bounds,aes(ymax=upper,ymin=lower,fill=group),alpha=0.25) +
    scale_y_continuous("similar to B <- d0 -> similar to A") +
    scale_x_continuous("similar to B <- d2 -> similar to A") +
    ggtitle("Decision Boundaries by condition") +
    theme_bw() +coord_flip(ylim = range(plotData$d2),xlim=range(plotData$d0)) +
    #coord_cartesian() +
    scale_color_manual(values=cbPalette)+scale_fill_manual(values=cbPalette)
plot

plot(dataList$d0,dataList$d2,pch=".")
with(test2,lines(m,d2,col="red"))
with(testB,lines(m,d2,col="green"))
with(test0,lines(m,d2,col="blue"))
