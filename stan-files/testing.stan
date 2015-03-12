data {
    int<lower=0> N ;
    //int<lower=0> P ; // number of predictors
    //matrix[N , p] ; //predictors as matrix
    vector<lower=0,upper=1>[N] F2 ; // F2 vs. everything else
    vector<lower=0,upper=1>[N] F0 ; // F0 vs. everything else
    //vector<lower=0,upper=1>[N] Ba ; // Balanced vs. everything else
    vector[N] d0 ; // f0 difference
    vector[N] d2 ; // f2 difference
    int<lower=0,upper=1> y[N] ; 
}
parameters {
    real bD0 ;
    real bD2 ;
    real bF2 ;
    real bF0 ;
    real alpha ;
    //vector[P] beta ; // vector version of predictors
}
model {
    alpha ~ normal(0,100);
    bF2 ~ normal(0,100) ;
    bF0 ~ normal(0,100) ;
    bD0 ~ normal(0,100) ;
    bD2 ~ normal(0,100) ;
    y ~ bernoulli_logit(alpha + bF2*F2 + bF0*F0 + bD0*d0 + bd2*d2) ;
}