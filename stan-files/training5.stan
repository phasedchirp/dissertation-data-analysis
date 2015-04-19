data {
    int<lower=0> G ; //number of groups
    int<lower=1> N ;
    matrix<lower=0,upper=1>[N,G] group ; // group factors
    matrix[N,G] iD0 ; // interaction of group and d0
    matrix[N,G] iD2 ; // interaction of group and d2
    int<lower=1> Nsubj; // number of participants
    int<lower=1,upper=Nsubj>S[N] ; // subject
//     vector<lower=0,upper=1>[N] response ; // response category
    vector[N] d0 ; // f0 difference
    vector[N] d2 ; // f2 difference
    vector[N] iDiff ; // interaction of differences
//     vector<lower=0,upper=1>[N] trialBlock;
//     matrix[N,G] iBlock ;
    int<lower=0,upper=1> y[N] ;
}
parameters {
    real alpha ;
    real<lower=0,upper=20> sigma_a ;
    vector[Nsubj] a ;
    real b0 ;
    real b2 ;
    vector[G] aG; //vector of group intercept adjustments
//     real b0 ; // d0 main effect
//     real<lower=0,upper=20>b0_sigma ;
//     vector[Nsubj] a0;
//     real b2 ; // d0 main effect
//     real<lower=0,upper=20>b2_sigma ;
//     vector[Nsubj] a2;
    vector[G] g0 ; // group:d0 interaction
    vector[G] g2 ; // group:d2 interaction
    real bD; // interaction of differences
//     real aBlock ;
//     vector[G] gBlock ;
//     real bResp ;
//     vector[Nsubj] aResp ;
//     real<lower=0,upper=20> respSigma ;
//     real<lower=1> n0 ;
//     real<lower=1> n2 ;
}
model {
    alpha ~ normal(0,20) ;
    a ~ normal(alpha,sigma_a) ;
    b0 ~ normal(0,20) ;
    b2 ~ normal(0,20) ;
//     n_a ~ exponential(0.1) ;
//     for(i in 1:G){ alpha[i] ~ student_t(n_a,0,50) ; }
//     for(i in 1:G){ alpha[i] ~ normal(0,50) ; }
//     for(i in 1:G){ b0[i] ~ normal(0,50) ; }
//     for(i in 1:G){ b2[i] ~ normal(0,50) ; }
//     a0 ~ normal(0,b0_sigma) ;
//     a2 ~ normal(0,b2_sigma) ;
    aG ~ normal(0,20) ;
    g0 ~ normal(0,50) ;
    g2 ~ normal(0,50) ;
    bD ~ normal(0,50) ;
//     aBlock ~ normal(0,50) ;
//     gBlock ~ normal(0,50) ;
//     bResp ~ normal(0,50) ;
//     aResp ~ normal(0,respSigma) ;
    {
        vector[N] yHat ;
        for(i in 1:N){
           yHat[i]  <- a[S[i]] + group[i]*aG + b0*d0[i] + b2*d2[i] + iD0[i]*g0 + iD2[i]*g2 + bD*iDiff[i];
        }
    y ~ bernoulli_logit( yHat );
    }
    // add back in: + trialBlock*aBlock + iBlock*gBlock
//     y ~ bernoulli_logit(alpha + group*aG + b0*d0 + b2*d2 + iD0*g0 + iD2*g2 + bD*iDiff) ;
}