sample_files = list.files('train-diff-MCMC',full.names=TRUE,pattern='*.csv')
trainMCMC = read_stan_csv(sample_files)
library(runjags)
trainCoda = mcmc.list( lapply( 1:ncol(trainMCMC) , 
                               function(x) { mcmc(as.array(trainMCMC)[,x,]) } ) )
trainCoda = combine.mcmc(trainCoda)
parameterNames = varnames(trainCoda)[!grepl("a\\[",varnames(trainCoda))]

range2 = range(abs(diss_training$d2),na.rm=T)
comb = seq(range2[1],range2[2],0.02)

#-------------------------------------------------------------------------------
qPred2_1 = function(){
  pred2 = numeric(length=length(comb))
  for(i in 1:length(trainCoda[,1])){
    pars = trainCoda[i,parameterNames]
    pred2 = rbind(pred2,
          (-(pars[1])/(pars[5]+comb*pars[11]) - comb*pars[6]/(pars[5]+comb*pars[11])))
  }
  out = data.frame(d2 = comb)
  out$lower = apply(pred2,2,quantile,probs=0.025)
  out$m = apply(pred2,2,quantile,probs=0.5)
  out$upper = apply(pred2,2,quantile,probs=0.975)
  return(out)
}
#-------------------------------------------------------------------------------
qPred2_2 = function(){
    pred2 = numeric(length=length(comb))
    for(i in 1:length(trainCoda[,1])){
        pars = trainCoda[i,parameterNames]
        pred2 = rbind(pred2,
                      (-(pars[1]+pars[12])/(pars[5]+comb*pars[11]) - comb*pars[6]/(pars[5]+comb*pars[11])))
    }
    out = data.frame(d2 = comb)
    out$lower = apply(pred2,2,quantile,probs=0.025)
    out$m = apply(pred2,2,quantile,probs=0.5)
    out$upper = apply(pred2,2,quantile,probs=0.975)
    return(out)
}

#-------------------------------------------------------------------------------
qPredB_1 = function(){
  predB = NULL
  for(i in 1:length(trainCoda[,1])){
    pars = trainCoda[i,parameterNames]
    predB = rbind(predB,
          -(pars[1]+pars[3])/(pars[7]+pars[5]+comb*pars[11]) - comb*(pars[6]+pars[9])/(pars[7]+pars[5]+comb*pars[11]))
  }
  out = data.frame(d2 = comb)
  out$lower = apply(predB,2,quantile,probs=0.025)
  out$m = apply(predB,2,quantile,probs=0.5)
  out$upper = apply(predB,2,quantile,probs=0.975)
  return(out)
}
#-------------------------------------------------------------------------------
qPredB_2 = function(){
    predB = NULL
    for(i in 1:length(trainCoda[,1])){
        pars = trainCoda[i,parameterNames]
        predB = rbind(predB,
                      -(pars[1]+pars[3]+pars[12]+pars[13])/(pars[7]+pars[5]+comb*pars[11]) - comb*(pars[6]+pars[9])/(pars[7]+pars[5]+comb*pars[11]))
    }
    out = data.frame(d2 = comb)
    out$lower = apply(predB,2,quantile,probs=0.025)
    out$m = apply(predB,2,quantile,probs=0.5)
    out$upper = apply(predB,2,quantile,probs=0.975)
    return(out)
}

#-------------------------------------------------------------------------------
qPred0_1 = function(){
  pred0 = NULL
  for(i in 1:length(trainCoda[,1])){
    pars = trainCoda[i,parameterNames]
    pred0 = rbind(pred0,
          -(pars[1]+pars[4])/(pars[8]+pars[5]+comb*pars[11]) - comb*(pars[6]+pars[10])/(pars[8]+pars[5]+comb*pars[11]))
  }
  out = data.frame(d2 = comb)
  out$lower = apply(pred0,2,quantile,probs=0.025)
  out$m = apply(pred0,2,quantile,probs=0.5)
  out$upper = apply(pred0,2,quantile,probs=0.975)
  return(out)
}
#-------------------------------------------------------------------------------
qPred0_2 = function(){
    pred0 = NULL
    for(i in 1:length(trainCoda[,1])){
        pars = trainCoda[i,parameterNames]
        pred0 = rbind(pred0,
              -(pars[1]+pars[4]+pars[12]+pars[14])/(pars[8]+pars[5]+comb*pars[11]) - comb*(pars[6]+pars[10])/(pars[8]+pars[5]+comb*pars[11]))
    }
    out = data.frame(d2 = comb)
    out$lower = apply(pred0,2,quantile,probs=0.025)
    out$m = apply(pred0,2,quantile,probs=0.5)
    out$upper = apply(pred0,2,quantile,probs=0.975)
    return(out)
}
#-------------------------------------------------------------------------------
test2_1 = qPred2_1()
test2_1$group = "f2"
test2_1$block = 1
testB_1 = qPredB_1()
testB_1$group= "b"
testB_1$block = 1
test0_1 = qPred0_1()
test0_1$group="f0"
test0_1$block = 1

test2_2 = qPred2_2()
test2_2$group = "f2"
test2_2$block = 2
testB_2 = qPredB_2()
testB_2$group= "b"
testB_2$block = 2
test0_2 = qPred0_2()
test0_2$group="f0"
test0_2$block = 2

plotData = with(na.omit(diss_training),data.frame(d0=abs(d0),
                                                 d2=abs(d2),
                                                 group=group,
                                                 block = ifelse(trial<=282,2,1)))

#plotData = droplevels(subset(plotData,group%in%c("f0","f2")))
#bounds = rbind(test2_1,test2_2,test0_1,test0_2)
bounds = rbind(test2_1,test0_1,testB_1)
#bounds$group=factor(bounds$group,levels=c("f2","f0"))
bounds$block = as.factor(bounds$block)
bounds$d0 = bounds$m
bounds$group=factor(bounds$group,levels=c("f2","f0","b"))
plotData$group=factor(plotData$group,levels=c("f2","f0","b"))

cbPalette <- c("#999999", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
plot = ggplot(data=plotData,aes(x=d2,y=d0,color=group)) +
    geom_point(alpha=0.2,shape=20) +
    geom_line(data=bounds,size=2) +
    geom_ribbon(data=bounds,aes(ymax=upper,ymin=lower,fill=group),alpha=0.25) +
    scale_y_continuous("more similar <- d0 -> less similar") +
    scale_x_continuous("more similar <- d2 -> less similar") +
    ggtitle("Decision Boundaries by condition") +
    theme_bw() +coord_flip(ylim = range(plotData$d2),xlim=range(plotData$d0)) +
    #coord_cartesian() +
    scale_color_manual(values=cbPalette)+scale_fill_manual(values=cbPalette)# +
    #facet_wrap("block")
plot
