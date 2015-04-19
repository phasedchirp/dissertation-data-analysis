sample_files = list.files('train-diff-MCMC-2',full.names=TRUE,pattern='*.csv')
trainMCMC = read_stan_csv(sample_files)
library(runjags)
trainCoda = mcmc.list( lapply( 1:ncol(trainMCMC) , 
                               function(x) { mcmc(as.array(trainMCMC)[,x,]) } ) )
trainCoda = combine.mcmc(trainCoda)
parameterNames = varnames(trainCoda)[grepl("(alpha)|(b)|(diff)|(aBlock)",varnames(trainCoda),perl=TRUE)]

range2 = range(abs(diss_training$d2),na.rm=T)
comb = seq(range2[1],range2[2],0.02)

#-------------------------------------------------------------------------------
qPred2 = function(block="1",resp="1"){
  pred2 = NULL
  for(i in 1:length(trainCoda[,1])){
    pars = trainCoda[i,parameterNames]
    if(resp=="1"){
      if(block=="1"){
        pred2 = rbind(pred2, (-(pars[1])/(pars[4]+comb*pars[10]) - comb*pars[7]/(pars[4]+comb*pars[10])))
      } else {
        pred2 = rbind(pred2,(-(pars[1]+pars[14])/(pars[4]+comb*pars[10]) - comb*pars[7]/(pars[4]+comb*pars[10])))
      }
    } else{
      if(block=="1"){
        pred2 = rbind(pred2, (-(pars[1]+pars[13])/(pars[4]+comb*pars[10]) - comb*pars[7]/(pars[4]+comb*pars[10])))
      } else {
        pred2 = rbind(pred2,(-(pars[1]+pars[13]+pars[14])/(pars[4]+comb*pars[10]) - comb*pars[7]/(pars[4]+comb*pars[10])))
      }
    }
    
  }
  out = data.frame(d2 = comb)
  out$lower = apply(pred2,2,quantile,probs=0.025)
  out$m = apply(pred2,2,quantile,probs=0.5)
  out$upper = apply(pred2,2,quantile,probs=0.975)
  return(out)
}
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
qPredB = function(block="1",resp="1"){
  predB = NULL
  for(i in 1:length(trainCoda[,1])){
    pars = trainCoda[i,parameterNames]
    if(resp=="1"){
      if(block=="1"){
        predB = rbind(predB,(-(pars[2])/(pars[5]+comb*pars[11]) - comb*pars[8]/(pars[5]+comb*pars[11])))
      } else{
        predB = rbind(predB,(-(pars[2]+pars[15])/(pars[5]+comb*pars[11]) - comb*pars[8]/(pars[5]+comb*pars[11])))
      }
    } else{
      if(block=="1"){
        predB = rbind(predB,(-(pars[2]+pars[13])/(pars[5]+comb*pars[11]) - comb*pars[8]/(pars[5]+comb*pars[11])))
      } else{
        predB = rbind(predB,(-(pars[2]+pars[13]+pars[15])/(pars[5]+comb*pars[11]) - comb*pars[8]/(pars[5]+comb*pars[11])))
      }
    }
    
    
  }
  out = data.frame(d2 = comb)
  out$lower = apply(predB,2,quantile,probs=0.025)
  out$m = apply(predB,2,quantile,probs=0.5)
  out$upper = apply(predB,2,quantile,probs=0.975)
  return(out)
}
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
qPred0 = function(block="1",resp="1"){
  pred0 = NULL
  for(i in 1:length(trainCoda[,1])){
    pars = trainCoda[i,parameterNames]
    if(resp=="1"){
      if(block=="1"){
        pred0 = rbind(pred0,(-(pars[3])/(pars[6]+comb*pars[12]) - comb*pars[9]/(pars[6]+comb*pars[12])))
      } else{
        pred0 = rbind(pred0,(-(pars[3]+pars[16])/(pars[6]+comb*pars[12]) - comb*pars[9]/(pars[6]+comb*pars[12])))
      }
    } else{
      if(block=="1"){
        pred0 = rbind(pred0,(-(pars[3]+pars[13])/(pars[6]+comb*pars[12]) - comb*pars[9]/(pars[6]+comb*pars[12])))
      } else{
        pred0 = rbind(pred0,(-(pars[3]+pars[13]+pars[16])/(pars[6]+comb*pars[12]) - comb*pars[9]/(pars[6]+comb*pars[12])))
      }
    }
    
    
  }
  out = data.frame(d2 = comb)
  out$lower = apply(pred0,2,quantile,probs=0.025)
  out$m = apply(pred0,2,quantile,probs=0.5)
  out$upper = apply(pred0,2,quantile,probs=0.975)
  return(out)
}
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
test2_1 = qPred2()
test2_1$group = "f2"
test2_1$block = 1
test2_1$resp = 1
testB_1 = qPredB()
testB_1$group= "b"
testB_1$block = 1
testB_1$resp = 1
test0_1 = qPred0()
test0_1$group="f0"
test0_1$block = 1
test0_1$resp = 1

test2_2 = qPred2("2")
test2_2$group = "f2"
test2_2$block = 2
test2_2$resp = 1
testB_2 = qPredB("2")
testB_2$group= "b"
testB_2$block = 2
testB_2$resp = 1
test0_2 = qPred0("2")
test0_2$group="f0"
test0_2$block = 2
test0_2$resp = 1
bounds = rbind(test2_1,test2_2,test0_1,test0_2,testB_1,testB_2)

test2_1 = qPred2("1","2")
test2_1$group = "f2"
test2_1$block = 1
test2_1$resp = 2
testB_1 = qPredB("1","2")
testB_1$group= "b"
testB_1$block = 1
testB_1$resp = 2
test0_1 = qPred0("1","2")
test0_1$group="f0"
test0_1$block = 1
test0_1$resp = 2

test2_2 = qPred2("2","2")
test2_2$group = "f2"
test2_2$block = 2
test2_2$resp = 2
testB_2 = qPredB("2","2")
testB_2$group= "b"
testB_2$block = 2
testB_2$resp = 2
test0_2 = qPred0("2","2")
test0_2$group="f0"
test0_2$block = 2
test0_2$resp = 2

bounds = rbind(bounds,test2_1,test2_2,test0_1,test0_2,testB_1,testB_2)

plotData = with(na.omit(diss_training),data.frame(d0=abs(d0),
                                                 d2=abs(d2),
                                                 group=group,
                                                 resp = response+1,
                                                 block = ifelse(trial<=282,1,2)))

plotData = droplevels(subset(plotData,group%in%c("f0","f2")))

bounds = droplevels(subset(bounds,group%in%c("f0","f2")))
#bounds$group=factor(bounds$group,levels=c("f2","f0"))
bounds$block = as.factor(bounds$block)
bounds$resp = as.factor(bounds$resp)
bounds$d0 = bounds$m
#bounds$group=factor(bounds$group,levels=c("f2","f0","b"))
bounds$group=factor(bounds$group,levels=c("f2","f0"))
#plotData$group=factor(plotData$group,levels=c("f2","f0","b"))

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
    scale_color_manual(values=cbPalette)+scale_fill_manual(values=cbPalette) +
    facet_grid(resp~block)
plot
