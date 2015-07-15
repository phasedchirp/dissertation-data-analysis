data {
    int<lower=0> G ; //number of groups
    int<lower=1> N ;
    matrix[N,G] group; // group factors
    matrix[N,G] iD0 ; // interaction of group and d0
    matrix[N,G] iD2 ; // interaction of group and d2
    matrix[N,G] gDI ; // interaction of group and cue interaction
    int<lower=1> Nsubj; // number of participants
    int<lower=1,upper=Nsubj>S[N] ; // subject
    vector[N] d0 ; // f0 difference
    vector[N] d2 ; // f2 difference
    vector[N] iDiff ; // interaction of cue differences
    int<lower=0,upper=1> y[N] ; 
}
parameters {
    real mu_a ;
    real<lower=0,upper=50> sigma_a ;
    vector[Nsubj] a; // random intercept adjustments
    real b0_mu ;
    real<lower=0,upper=20> sigma_b0 ;
    vector[Nsubj] b0 ;
    real b2_mu ;
    real<lower=0,upper=20> sigma_b2 ;
    vector[Nsubj] b2 ;
    // within subjects effects:
    vector[G] aG; //vector of group intercept adjustments
    real bDiff ;
    vector[G] gD0 ;
    vector[G] gD2 ;
    vector[G] gDiff ;
}
model {
    mu_a ~ normal(0,10) ;
    a ~ normal(mu_a,sigma_a) ;
    b0_mu ~ normal(0,10) ;
    b0 ~ normal(b0_mu,sigma_b0) ;
    b2_mu ~ normal(0,10) ;
    b2 ~ normal(b2_mu,sigma_b2) ;
    // Between subjects priors:
    bDiff ~ normal(0,50) ;
    aG ~ normal(0,50) ;
    gD0 ~ normal(0,50) ;
    gD2 ~ normal(0,50) ;
    gDiff ~ normal(0,50) ;
    {
        vector[N] ranEF ;
        for(i in 1:N){
           ranEF[i]  <- a[S[i]] + b0[S[i]]*d0[i] + b2[S[i]]*d2[i] ; 
        }
        y ~ bernoulli_logit(ranEF + group*aG + iD0*gD0 + iD2*gD2 + bDiff*iDiff + gDI*gDiff);
    }
}