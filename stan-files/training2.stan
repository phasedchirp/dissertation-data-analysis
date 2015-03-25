data {
    int<lower=0> G ; //number of groups
    int<lower=1> N ;
    matrix[N,G] group; // group factors
    matrix[N,G] iD0 ; // interaction of group and d0
    matrix[N,G] iD2 ; // interaction of group and d2
    int<lower=1> Nsubj; // number of participants
    int<lower=1,upper=Nsubj>S[N] ; // subject
    vector<lower=0,upper=1>[N] response ; // response category
    vector[N] d0 ; // f0 difference
    vector[N] d2 ; // f2 difference
    vector[N] iDiff ; // interaction of differences
    vector<lower=0,upper=1>[N] trialBlock;
    matrix[N,G] iBlock ;
    int<lower=0,upper=1> y[N] ;
}
parameters {
    real alpha ;
    //real a_mu ; // mean intercept
    //real<lower=0,upper=20> sigma_a ; // intercept variance
    //vector[Nsubj] a; // by-subject intercepts
    vector[G] aG; //vector of group intercept adjustments
    real bD0 ; // d0 main effect
    real bD2 ; // d2 main effect
    vector[G] gD0 ; // group:d0 interaction
    vector[G] gD2 ; // group:d2 interaction
    real bDiff ; // interaction of differences
    real aBlock ;
    vector[G] gBlock ;
}
model {
    alpha ~ normal(0,50) ;
    //a_mu ~ normal(0,50) ;
    //a ~ normal(a_mu,sigma_a) ;
    bD0 ~ normal(0,50) ;
    bD2 ~ normal(0,50) ;
    aG ~ normal(0,50) ;
    gD0 ~ normal(0,50) ;
    gD2 ~ normal(0,50) ;
    bDiff ~ normal(0,50) ;
    aBlock ~ normal(0,50) ;
    gBlock ~ normal(0,50) ;
    y ~ bernoulli_logit(alpha + group*aG + trialBlock*aBlock + iBlock*gBlock + bD0*d0 + bD2*d2 + iD0*gD0 +iD2*gD2 + bDiff*iDiff) ;
    // non-random intercept version:
//     y ~ bernoulli_logit(alpha + group*aG + bD0*d0 + bD2*d2 + iD0*gD0 +iD2*gD0);
}