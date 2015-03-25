data {
    int<lower=1> N ;
    int<lower=1> Nsubj; // number of participants
    int<lower=1,upper=Nsubj>S[N] ; // subject 
    vector<lower=0,upper=1>[N] F2 ; // F2 vs. everything else
    vector<lower=0,upper=1>[N] F0 ; // F0 vs. everything else
    vector[N] d0A ; // f0A difference
    vector[N] d2A ; // f2A difference
    vector[N] d0B ; // f0B difference
    vector[N] d2B ; // f2B difference
    int<lower=0,upper=1> y[N] ; 
}
transformed data {
    vector[N] i02A ; // F0A interaction for F2 group
    vector[N] i00A ; // F0A interaction for F0 group
    vector[N] i02B ; // F0B interaction for F2 group
    vector[N] i00B ; // F0B interaction for F0 group
    i02A <- d0A .* F2 ;
    i00A <- d0A .* F0 ;
    i02B <- d0B .* F2 ;
    i00B <- d0B .* F0 ;
}
parameters {
    real bD0A ;
    real bD2A ;
    real bD0B ;
    real bD2B ;
    real bF2 ;
    real bF0 ;
    real f2i0A;
    real f0i0A;
    real f2i2B;
    real f0i0B;
    real alpha ; // intercept
}
transformed parameters{
    vector[N] interaction
}
model {
    alpha ~ normal(0,100) ;
    bF2 ~ normal(0,100) ;
    bF0 ~ normal(0,100) ;
    bD0A ~ normal(0,100) ;
    bD2A ~ normal(0,100) ;
    bD0B ~ normal(0,100) ;
    bD2B ~ normal(0,100) ;
    f0i0A ~ normal(0,100) ;
    f2i0A ~ normal(0,100) ;
    f0i2B ~ normal(0,100) ;
    f2i2B ~ normal(0,100) ;
    y ~ bernoulli_logit(alpha + bF2*F2 + bF0*F0 + bD2A*d2A + bD0A*d0A + bD2B*d2B + bD0B*d0B + f2i0A*i02A + f0i0A*i00A + f2i0B*i02B + f0i0B*i00B);

}