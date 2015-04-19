// Try non-common degrees of freedom parameters for b0,b2.
// Also consider by-group d0*d2 interaction effects.
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
//     int<lower=0> gridSize ;
//     vector[gridSize] grid ;
}
parameters {
    // fixed-ish effects:
    real alpha[G] ; // intercept mean
    real b0[G] ; // mean d0 effect
    real b2[G] ; // mean d2 effect
    real bD[G] ; // interaction of differences
    real bResp ; // response category
    real aBlock[G] ; // block effect
    
    // random-ish effects:
    // by-subject intercepts
    real<lower=1> n_a[G] ;
    real<lower=0,upper=20> sigma_a[G] ;
    real a[Nsubj] ;
    // by-subject d0 effects
    real<lower=1> n0[G] ;
    real<lower=0,upper=20> sigma_b0[G] ;
    real a0[Nsubj] ;
    // by-subject d2 effects
    real<lower=1> n2[G] ;
    real<lower=0,upper=20> sigma_b2[G] ;
    real a2[Nsubj] ;
    // by subject response category effects
    real<lower=0> sigma_Resp ;
    vector[Nsubj] aResp ;   
    // by-subject interaction of differences
    real<lower=0> sigma_D ;
    vector[Nsubj] aD ;
}
model {
    // intercept:
    alpha ~ normal(0,50) ;
    n_a ~ exponential(0.1) ;
    for(i in 1:Nsubj){
//         a[i] ~ normal(alpha[gID[i]],sigma_a[gID[i]]) ;
        a[i] ~ student_t(n_a[gID[i]],alpha[gID[i]],sigma_a[gID[i]]) ;
    }
    // d0 effects:
    b0 ~ normal(0,50) ;
    n0 ~ exponential(0.1) ;
    for(i in 1:Nsubj){
//         a0[i] ~ normal(b0[gID[i]],sigma_b0[gID[i]]) ;
        a0[i] ~ student_t(n0[gID[i]],b0[gID[i]],sigma_b0[gID[i]]) ;
    }
    // d2 effects:
    b2 ~ normal(0,50) ;
    n2 ~ exponential(0.1) ;
    for(i in 1:Nsubj){
//         a2[i] ~ normal(b2[gID[i]],sigma_b2[gID[i]]) ;
        a2[i] ~ student_t(n2[gID[i]],b2[gID[i]],sigma_b2[gID[i]]) ;
    }
    bD ~ normal(0,50) ;
    for(i in 1:Nsubj){
	aD[i] ~ normal(bD[gID[i]],sigma_D) ;
    }
    aBlock ~ normal(0,50) ;
    bResp ~ normal(0,50) ;
    aResp ~ normal(bResp,sigma_Resp) ;
    {
        vector[N] yHat ;
        for(i in 1:N){
           yHat[i]  <- a[S[i]] + a0[S[i]]*d0[i] + a2[S[i]]*d2[i] + aD[S[i]]*iDiff[i] + aBlock[group[i]]*trialBlock[i] + aResp[S[i]]*response[i] ;
        }
    y ~ bernoulli_logit( yHat );
    }
}
generated quantities {
    // Parameter differences
    real diff_alpha ;
    real diff_b0 ;
    real diff_i ;
    diff_alpha <- alpha[3] - alpha[1] ; 
    diff_b0 <- b0[3] - b0[1] ;
    diff_i <- bD[3] - bD[1];
}

