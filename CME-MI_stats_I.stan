data {
        #initial guesses
        int<lower=0> ninits;
        vector[ninits] inits;
        
        #nY: num trials
        int<lower=0> nY;
        
        #nX: num conditions
        int<lower=0> nX;
        
        #X: condition prior index
        int X[nY];
        
        #Y: outcomes
        vector[nY] speed;
        vector[nY] error;

}
parameters {
        #Znoise: trial-by-trial Znoise for outcome
        real<lower=0> Znoise;
        real a0[nX];
        real b0[nX];
        real c0[nX];
        real d0[nX];
}
transformed parameters{
        real a[nX];
        real b[nX];
        real c[nX];
        real d[nX];
        for(j in 1:nX){
                a[j] = inits[1] + inits[1]*0.5*a0[j];
                b[j] = inits[2] + inits[2]*0.5*b0[j];
                c[j] = inits[3] + inits[3]*0.5*c0[j];
                d[j] = inits[4] + inits[4]*0.5*d0[j];
        }
}
model {
        // priors
        for(j in 1:nX){
                a0[j] ~ normal(0,1);
                b0[j] ~ normal(0,1);
                c0[j] ~ normal(0,1);
                d0[j] ~ normal(0,1);
        }
        Znoise ~ weibull(2, inits[5]);
        
        // likelihood
        for(i in 1:nY){
                error[i] ~ normal(
                        b[X[i]]+((a[X[i]]-b[X[i]])/(1+exp(-c[X[i]]*(speed[i]-d[X[i]]))))
                        , Znoise
                );
        }
}
