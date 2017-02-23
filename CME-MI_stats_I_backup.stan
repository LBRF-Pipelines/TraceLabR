data {
        
        #nY: num trials
        int<lower=0> nY ;

        #Y: outcomes
        vector[nY] speed ;
        
        vector[nY] error ;
        
}

parameters {
        #Znoise: trial-by-trial Znoise for outcome
        real<lower=0> Znoise ;
        real a0;
        real b0;
        real c0;
        real d0;
}
transformed parameters{
        real a;
        real b;
        real c;
        real d;
        a = 137 + 68*a0;
        b = 61 + 30*b0;
        c = 0.01 + 0.005*c0;
        d = 1440 + 720*d0;
}
model {
        a0 ~ normal(0,1);
        b0 ~ normal(0,1);
        c0 ~ normal(0,1);
        d0 ~ normal(0,1);
        Znoise ~ weibull(2, 1) ;
        for(i in 1:nY){
                error[i] ~ normal(
                        b+((a-b)/(1+exp(-c*(speed[i]-d))))
                        , Znoise
                ) ;
        }
}
