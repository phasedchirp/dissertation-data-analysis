diss_data = data.frame(
    ExperimentName=factor(),
    cat=factor(levels=c(1,2)),
    catA=factor(levels=c(1,2)),
    catB=factor(levels=c(1,2)),
    cons=factor(levels=c('b','d','g')),
    correct_resp=factor(levels=c()),
    dur=numeric(),
    durA=numeric(),
    durB=numeric(),
    f0=numeric(),
    f0A=numeric(),
    f0B=numeric(),
    f2=numeric(),
    f2A=numeric(),
    f2B=numeric(),
    resp.CRESP=factor(levels=c()),
    resp.RESP=factor(levels=c()),
    respond.RESP=factor(levels=c()),
    Running=factor(levels=c("PracticeList","Block1","Block2","Block3","testList")),
    subject=factor())

# loadData = function(){
#   subject_list = list.files('~/Dropbox/Dissertation/diss_exp_results',pattern='*.csv')
#   for(subj in subject_list){
#     temp = read.csv(paste('~/Dropbox/Dissertation/diss_exp_results/',subj,sep=''))[,c(1,28:41,46,47,51,54)]
#     temp$subject = gsub(".csv",'',subj)
#     diss_data = rbind(diss_data,temp)
#   }
# }
subject_list = list.files('~/Dropbox/Dissertation/diss_exp_results',pattern='*.csv')
for(subj in subject_list){
    temp = read.csv(paste('~/Dropbox/Dissertation/diss_exp_results/',subj,sep=''))[,c(1,28:41,46,47,51,54)]
    temp$subject = gsub(".csv",'',subj)
    diss_data = rbind(diss_data,temp)
}

diss_data$subject = as.factor(diss_data$subject)
diss_data$group = gsub("(E|OE)[0-9][0-9]?",'',diss_data$subject)
diss_data$group = as.factor(diss_data$group)
diss_training = droplevels(subset(diss_data,Running%in%c("Block2","Block3")))[,-c(3,4,6,8,9,11,12,14:17,19)]
diss_testing = droplevels(subset(diss_data,Running=="testList"))[,-c(6,18,19)]

# Check response rates:
# round(100*(564 - colSums(xtabs(~respond.RESP+subject,data=diss_training)))/564,1)
# round(100*(60 - colSums(xtabs(~resp.RESP+subject,data=diss_testing)))/60,1)

# Check for notable response bias:
# t = xtabs(~response+subject,data=diss_training)
# round(100*t[1,]/564,1)

# t2 = xtabs(~response+subject,data=diss_testing)
# round(100*t2[1,]/60,1)

# subjects to exclude from analysis (see subject_notes.txt for reasons)
dropSubj = c("f2OE1","f0OE1","f2OE8")



diss_training = droplevels(subset(diss_training,!(subject %in% dropSubj)))
diss_testing = droplevels(subset(diss_testing,!(subject %in% dropSubj)))

nSubj = length(unique(diss_training$subject))

diss_testing$response = with(diss_testing,ifelse(resp.RESP==resp.CRESP,1,0))
diss_training$trial = rep(1:564,nSubj)

# Rename for convenience
colnames(diss_training)[7] = "response"
# convert [1,2] to [0,1]
diss_training$response = diss_training$response -1

# create variable with previous response
diss_training$prev = NA
for(i in 2:length(diss_training$response)){
  diss_training$prev[i] = diss_training$response[i-1]
}
diss_training[diss_training$trial==1,]$prev=NA

diss_training$diff = ifelse(diss_training$response == diss_training$prev,1,0)

# create variable with F0 difference
diss_training$d0 = NA
for(i in 2:length(diss_training$response)){
  diss_training$d0[i] = diss_training$f0[i] - diss_training$f0[i-1]
}
diss_training[diss_training$trial==1,]$d0=NA

# create variable with F0 difference
diss_training$d2 = NA
for(i in 2:length(diss_training$response)){
  diss_training$d2[i] = diss_training$f2[i] - diss_training$f2[i-1]
}
diss_training[diss_training$trial==1,]$d2=NA

# create block variable:
diss_training$block = ifelse(diss_training$trial<=282,1,2)


diss_training$group = relevel(diss_training$group, ref="f2")
diss_testing$group = relevel(diss_testing$group, ref="f2")

# Cue differences for Testing:
diss_testing$d0A = diss_testing$f0-diss_testing$f0A
diss_testing$d0B = diss_testing$f0-diss_testing$f0B
diss_testing$d2A = diss_testing$f2-diss_testing$f2A
diss_testing$d2B = diss_testing$f2-diss_testing$f2B

d0_1 = 1/(with(diss_testing,ifelse(cat==catA, abs(d0A), abs(d0B)))+0.01)
d0_2 =  1/(with(diss_testing,ifelse(cat==catA, abs(d0B), abs(d0A)))+0.01)
diss_testing$d0 = d0_1/d0_2

d2_1 =  1/(with(diss_testing,ifelse(cat==catA, abs(d2A), abs(d2B)))+0.01)
d2_2 =  1/(with(diss_testing,ifelse(cat==catA, abs(d2B), abs(d2A)))+0.01)
diss_testing$d2 = d2_1/d2_2


testA = with(diss_testing,1/(abs(d0A)+0.01))
testB = with(diss_testing,1/(abs(d0B)+0.01))
diss_testing$testRat0 = testA/testB

testA = with(diss_testing,1/(abs(d2A)+0.01))
testB = with(diss_testing,1/(abs(d2B)+0.01))
diss_testing$testRat2 = testA/testB

rm(list=c("i","dropSubj","nSubj","subj","subject_list","temp","diss_data","testA","testB","d0_1","d0_2","d2_1","d2_2"))

