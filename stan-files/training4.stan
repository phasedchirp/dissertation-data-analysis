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
//     vector[N] iDiff ; // interaction of differences
//     vector<lower=0,upper=1>[N] trialBlock;
//     matrix[N,G] iBlock ;
    int<lower=0,upper=1> y[N] ;
}
parameters {
    real a_mu ; // mean intercept
    real<lower=0,upper=20> sigma_a ; // intercept variance
    vector[Nsubj] a; // by-subject intercepts
    vector[G] aG; //vector of group intercept adjustments
    real b0_mu ; // d0 main effect
    real<lower=0,upper=20>b0_sigma ;
    vector[Nsubj] b0;
    real b2_mu ; // d0 main effect
    real<lower=0,upper=20>b2_sigma ;
    vector[Nsubj] b2;
    vector[G] gD0 ; // group:d0 interaction
    vector[G] gD2 ; // group:d2 interaction
//     real bDiff ; // interaction of differences
//     real aBlock ;
//     vector[G] gBlock ;
    real bResp ;
    real n0 ;
    real n2 ;
}
model {
    n0 ~ exponential(0.1) ;
    n2 ~ exponential(0.1) ;
    a_mu ~ normal(0,50) ;
    a ~ normal(a_mu,sigma_a) ;
    b0_mu ~student_t(n0+2,0,50) ;
    b2_mu ~ student_t(n2+2,0,50) ;
    b0 ~ normal(b0_mu,b0_sigma) ;
    b2 ~ normal(b2_mu,b2_sigma) ;
    aG ~ normal(0,50) ;
    gD0 ~ normal(0,50) ;
    gD2 ~ normal(0,50) ;
//     bDiff ~ normal(0,50) ;
//     aBlock ~ normal(0,50) ;
//     gBlock ~ normal(0,50) ;
    bResp ~ normal(0,50) ;
    {
        vector[N] ranEF ;
        for(i in 1:N){
           ranEF[i]  <- a[S[i]] + b0[S[i]] + b2[S[i]];
        }
    y ~ bernoulli_logit(ranEF + group*aG + bResp*response + iD0*gD0 +iD2*gD2);
    }
    // add back in: + trialBlock*aBlock + iBlock*gBlock
    
}