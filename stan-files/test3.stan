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
    vector[G] aG; //vector of group intercept adjustments
    real bD0 ;
    real bD2 ;
    real bDiff ;
    vector[G] gD0 ;
    vector[G] gD2 ;
    real mu_a ;
    real<lower=0,upper=50> sigma_a ;
    vector[G] gDiff ;
    //real alpha ; // intercept
    vector[Nsubj] a; // random intercept adjustments
}
// transformed parameters{
// //     vector[N] yHat ;
//     vector[N] yInt ;
//     for(i in 1:N){
//        yInt[i]  <- a[S[i]] ;
//     }
// }
model {
    mu_a ~ normal(0,50) ;
    a ~ normal(mu_a,sigma_a) ;
    bD0 ~ normal(0,100) ;
    bD2 ~ normal(0,100) ;
    bDiff ~ normal(0,100) ;
    aG ~ normal(0,100) ;
    gD0 ~ normal(0,100) ;
    gD2 ~ normal(0,100) ;
    gDiff ~ normal(0,100) ;
//     y ~ bernoulli_logit(yInt + group*aG + bD0*d0 + bD2*d2 + iD0*gD0 +iD2*gD2 + bDiff*iDiff + gDI*gDiff) ;
    {
        vector[N] yInt ;
        for(i in 1:N){
           yInt[i]  <- a[S[i]] ; 
        }
        y ~ bernoulli_logit(yInt + group*aG + bD0*d0 + bD2*d2 + iD0*gD0 + iD2*gD2 + bDiff*iDiff + gDI*gDiff);
    }
}