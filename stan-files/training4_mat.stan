data {
    int<lower=0> G ; //number of groups
    int<lower=1> N ; // number of data points
    int<lower=1> P ; // number of predictors
    matrix[N,P] X ; // matrix of predictors
    int<lower=1> Nsubj; // number of participants
    int<lower=1,upper=Nsubj>S[N] ; // subject
    vector<lower=0,upper=1>[N] response ; // response category
    int<lower=0,upper=1> y[N] ;
}
parameters {
    // Higher-level priors:
    real n0 ; // prior on normality of b0 prior
    real a_mu ; // mean intercept
    real<lower=0,upper=20> sigma_a ; // intercept variance
    real b0_mu ; // mean d0 main effect
    real<lower=0,upper=20>b0_sigma ; // b0 variance
    real n2 ;  // prior on normality of b2 prior
    real b2_mu ; // mean d2 main effect
    real<lower=0,upper=20>b2_sigma ; // b2 variance
        
    // Within-subject coefficients/partial pooling:
    vector[Nsubj] a; // by-subject intercepts
    
    matrix[Nsubj,P1] beta1 ;
    vector[Nsubj] b0; // by-subject d0
    vector[Nsubj] b2; // by-subject d2
    
 
//     real bDiff ; // interaction of differences
//     real aBlock ;
//     vector[G] gBlock ;
    vector[P] beta2 ; // coefficients for between-subject effects
    vector[G] gD0 ; // group:d0 interaction
    vector[G] gD2 ; // group:d2 interaction
    real bResp ;
    vector[G] aG; //vector of group intercept adjustments
}
model {
    n0 ~ exponential(0.1) ;
    n2 ~ exponential(0.1) ;
    a_mu ~ normal(0,50) ;
    a ~ normal(a_mu,sigma_a) ;
    b0_mu ~ student_t(n0+2,0,50) ;
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