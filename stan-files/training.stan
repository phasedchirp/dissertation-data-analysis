data {
    int<lower=0> G ; //number of groups
    int<lower=1> N ;
    matrix[N,G] group; // group factors
    matrix[N,G] iD0 ; // interaction of group and d0
    matrix[N,G] iD2 ; // interaction of group and d2
    int<lower=1> Nsubj; // number of participants
    int<lower=1,upper=Nsubj>S[N] ; // subject
    vector<lower=0,upper=1>[N] response ; // response category
    vector [N] trial; // where in the training block?
    vector[N] d0 ; // f0 difference
    vector[N] d2 ; // f2 difference
    int<lower=0,upper=1> y[N] ; 
}
// transformed data {
//     matrix[N,G] iD0 ; // group:d0 interaction
//     matrix[N,G] iD2 ; // group:d2 interaction
// //     vector[N] i00 ; // group:d2 interaction
// //     vector[N] i22 ; // F2 interaction for F2 group
// //     vector[N] i20 ; // F2 interaction for F0 group
//     iD0 <- group .* append_col(d0,d0) ;
//     iD2 <- group .* append_col(d2,d2) ;
// }
parameters {
    real alpha ; // baseline intercept
    vector[G] aG; //vector of group intercept adjustments
    real bD0 ;
    real bD2 ;
    vector[G] gD0 ;
    vector[G] gD2 ;
    real mu_a ;
    real<lower=0,upper=50> sigma_a ;
    //real alpha ; // intercept
    vector[Nsubj] a; // random intercept adjustments
}
transformed parameters{
//     vector[N] yHat ;
    vector[N] yInt ;
    for(i in 1:N){
       yInt[i]  <- a[S[i]] ;
    }
//     yHat <- yInt + group*aG + bD0*d0 + bD2*d2 + iD0*gD0 ;
//     for(i in 1:N){
//         //yHat[i] <- alpha + a[S[i]] + bF2*F2[i] + bF0*F0[i] + bD2*d2[i] + bD0*d0[i] ;
//         yHat[i] < alpha + a[S[i]] + beta*Dat[] ;
//     }
}
model {
    //alpha ~ normal(0,100);
    aG ~ normal(0,100) ;
//     for(i in 1:G){
//         aG ~ normal(0,100) ;
//     }
    gD0 ~ normal(0,100) ;
//     for(i in 1:G){
//         gD0 ~ normal(0,100) ;
//     }
    gD2 ~ normal(0,100) ;
//     for(i in 1:G){
//         gD2 ~ normal(0,100) ;
//     }
    mu_a ~ normal(0,50) ;
    a ~ normal(mu_a,sigma_a) ;
    bD0 ~ normal(0,100) ;
    bD2 ~ normal(0,100) ;
    y ~ bernoulli_logit(yInt + group*aG + bD0*d0 + bD2*d2 + iD0*gD0 +iD2*gD2) ;
//     y ~ bernoulli_logit(alpha + a + group*aG + bD0*d0 + bD2*d2 + iD0*gD0 +iD2*gD0);

}