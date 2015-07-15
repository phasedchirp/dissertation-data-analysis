data {
    int<lower=0> G ; //number of groups
    int<lower=1> N ;
    int<lower=1> group[N] ; // group factors
    int<lower=1> Nsubj; // number of participants
    int<lower=1> gID[Nsubj] ; // map subject to group
    int<lower=1,upper=Nsubj>S[N] ; // subject
    vector<lower=0,upper=1>[N] response ; // response category
    vector[N] d0 ; // f0 difference
    vector[N] d2 ; // f2 difference
    vector[N] iDiff ; // interaction of differences
    vector<lower=0,upper=1>[N] trialBlock;
    int<lower=0,upper=1> y[N] ;
}
parameters {
    // fixed-ish effects:
    real alpha[G] ; // intercept mean
    real b0[G] ; // mean d0 effect
    real b2[G] ; // mean d2 effect
    real bD[G] ; // interaction of differences
//     real bResp ; // response category
    real aBlock[G] ; // block effect
    
    // random-ish effects:
    // by-subject intercepts
    real<lower=0,upper=20> sigma_a ;
//     real<lower=0,upper=20> sigma_a[G] ;
    real a[Nsubj] ;
    // by-subject d0 effects
    real<lower=0,upper=20> sigma_b0 ;
//     real<lower=0,upper=20> sigma_b0[G] ;
    real a0[Nsubj] ;
    // by-subject d2 effects
    real<lower=0,upper=20> sigma_b2 ;
//     real<lower=0,upper=20> sigma_b2[G] ;
    real a2[Nsubj] ;
    // by subject response category effects
//     real<lower=0> sigma_Resp ;
//     vector[Nsubj] aResp ;   
    // by-subject interaction of differences
//     real<lower=0> sigma_D ;
//     vector[Nsubj] aD ;
    // intercept degrees of freedom:
    real<lower=0> nA[G] ;
}
model {
    // intercept:
    alpha ~ normal(0,50) ;
//     nA ~ exponential(0.05) ;
    nA ~ gamma(2,0.1) ;
    for(i in 1:Nsubj){
         a[i] ~ student_t(nA[gID[i]],alpha[gID[i]],sigma_a) ;
//         a[i] ~ normal(alpha[gID[i]],sigma_a) ;
    }
    // d0 effects:
    b0 ~ normal(0,50) ;
    for(i in 1:Nsubj){
        a0[i] ~ normal(b0[gID[i]],sigma_b0) ;
    }
    // d2 effects:
    b2 ~ normal(0,50) ;
    for(i in 1:Nsubj){
        a2[i] ~ normal(b2[gID[i]],sigma_b2) ;
    }
    bD ~ normal(0,50) ;
//     for(i in 1:Nsubj){
// 	aD[i] ~ normal(bD[gID[i]],sigma_D) ;
//     }
    aBlock ~ normal(0,50) ;
//     bResp ~ normal(0,50) ;
//     aResp ~ normal(bResp,sigma_Resp) ;
//      + aResp[S[i]]*response[i]
//      aD[S[i]]*iDiff[i] 
    {
        vector[N] yHat ;
        for(i in 1:N){
           yHat[i]  <- a[S[i]] + a0[S[i]]*d0[i] + a2[S[i]]*d2[i] + bD[group[i]]*iDiff[i] + aBlock[group[i]]*trialBlock[i] ;
        }
    y ~ bernoulli_logit( yHat );
    }
}
generated quantities {
    // Parameter differences
    real diff_F0 ;
    real diff_B ;
    real diff_b0b ;
    real diff_b0f ;
    real diff_b2b ;
    real diff_b2f ;
    real diff_ib ;
    real diff_if ;
    real diff_bb ;
    real diff_bf ;
    diff_F0 <- alpha[3] - alpha[1] ;
    diff_B <- alpha[2] - alpha[1] ;
    diff_b0b <- b0[2] - b0[1] ;
    diff_b0f <- b0[3] - b0[1] ;
    diff_b2b <- b2[2] - b2[1] ;
    diff_b2f <- b2[3] - b2[1] ;
    diff_ib <- bD[2] - bD[1];
    diff_if <- bD[3] - bD[1];
    diff_bb <- aBlock[2] - aBlock[1] ;
    diff_bf <- aBlock[3] - aBlock[1] ;
}

