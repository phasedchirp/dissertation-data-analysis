data {
    int<lower=0> G ; //number of groups
    int<lower=1> N ;
    int<lower=1>group[N] ; // group factors
//     matrix[N,G] iD0 ; // interaction of group and d0
//     matrix[N,G] iD2 ; // interaction of group and d2
    int<lower=1> Nsubj; // number of participants
    int<lower=1,upper=Nsubj>S[N] ; // subject
    vector<lower=0,upper=1>[N] response ; // response category
    vector[N] d0 ; // f0 difference
    vector[N] d2 ; // f2 difference
//     vector[N] iDiff ; // interaction of differences
//     vector<lower=0,upper=1>[N] trialBlock;
//     matrix[N,G] iBlock ;
    int<lower=0,upper=1> y[N] ;
}
parameters {
    real  alpha[G] ;
    real<lower=1> n_a ;
    real b0[G] ;
    real b2[G] ;
//     vector[G] aG; //vector of group intercept adjustments
//     real b0 ; // d0 main effect
//     real<lower=0,upper=20>b0_sigma ;
//     vector[Nsubj] a0;
//     real b2 ; // d0 main effect
/*    real<lower=0,upper=20>b2_sigma ;
    vector[Nsubj] a2;
    vector[G] gD0 ; // group:d0 interaction
    vector[G] gD2 ; // group:d2 interaction*/
//     real bDiff ; // interaction of differences
//     real aBlock ;
//     vector[G] gBlock ;
//     real bResp ;
//     vector[Nsubj] aResp ;
//     real<lower=0,upper=20> respSigma ;
//     real<lower=1> n0 ;
//     real<lower=1> n2 ;
}
model {
    n_a ~ exponential(0.1) ;
    for(i in 1:G){ alpha[i] ~ student_t(n_a,0,50) ; }
    for(i in 1:G){ b0[i] ~ normal(0,50) ; }
    for(i in 1:G){ b2[i] ~ normal(0,50) ; }
//     a0 ~ normal(0,b0_sigma) ;
//     a2 ~ normal(0,b2_sigma) ;
//     aG ~ normal(0,50) ;
//     gD0 ~ normal(0,50) ;
//     gD2 ~ normal(0,50) ;
//     bDiff ~ normal(0,50) ;
//     aBlock ~ normal(0,50) ;
//     gBlock ~ normal(0,50) ;
//     bResp ~ normal(0,50) ;
//     aResp ~ normal(0,respSigma) ;
    {
        vector[N] yHat ;
        for(i in 1:N){
           yHat[i]  <- alpha[group[i]] + b0[group[i]]*d0[i] + b2[group[i]]*d2[i];
        }
    y ~ bernoulli_logit( yHat );
    }
    // add back in: + trialBlock*aBlock + iBlock*gBlock
    
}