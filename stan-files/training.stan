data {
    //int<lower=0> P ; //number of predictors
    int<lower=1> N ;
    //matrix[N,P] Dat; // predictors
    int<lower=1> Nsubj; // number of participants
    int<lower=1,upper=Nsubj>S[N] ; // subject 
    vector<lower=0,upper=1>[N] F2 ; // F2 vs. everything else
    vector<lower=0,upper=1>[N] F0 ; // F0 vs. everything else
    vector<lower=0,upper=1>[N] response ; // response category
    vector [N] trial; // where in the training block?
    vector[N] d0 ; // f0 difference
    vector[N] d2 ; // f2 difference
    int<lower=0,upper=1> y[N] ; 
}
transformed data {
    vector[N] i02 ; // F0 interaction for F2 group
    vector[N] i00 ; // F0 interaction for F0 group
    i02 <- d0 .* F2 ;
    i00 <- d0 .* F0 ;
}
parameters {
    //vector beta[P] ; vector of predictors
    real bD0 ;
    real bD2 ;
    real bF2 ;
    real bF0 ;
    real f2i0;
    real f0i0;
    real alpha ; // intercept
    //real a[Nsubj]; // random intercept adjustments
}
// transformed parameters{
//     vector[N] yHat ;
//     vector[N] yInt ;
//     for(i in 1:N){
//        yInt  <- alpha + a[S[i]] ;
//     }
//     yHat <- yInt + beta*Dat ;
//     for(i in 1:N){
//         //yHat[i] <- alpha + a[S[i]] + bF2*F2[i] + bF0*F0[i] + bD2*d2[i] + bD0*d0[i] ;
//         yHat[i] < alpha + a[S[i]] + beta*Dat[] ;
//     }
// }
model {
    alpha ~ normal(0,100) ;
    //a ~ normal(0,50) ;
    bF2 ~ normal(0,100) ;
    bF0 ~ normal(0,100) ;
    bD0 ~ normal(0,100) ;
    bD2 ~ normal(0,100) ;
    f0i0 ~ normal(0,100) ;
    f2i0 ~ normal(0,100) ;
    y ~ bernoulli_logit(alpha + bF2*F2 + bF0*F0 + bD2*d2 + bD0*d0 + f2i0*i02 + f0i0*i00);

}