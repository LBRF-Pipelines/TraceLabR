data {

	#nY: num trials
	int<lower=0> nY ;

	#nW: num within predictors
	int<lower=1> nW ;

	#nS: num subjects
	int<lower=1> nS ;

	#nB: num group predictors
	int<lower=1> nB ;

	#S: trial-by-trial subject labels
	int<lower=1,upper=nS> S[nY] ;

	#W: within predictors
	matrix[nY, nW] W ;

	#B: between predictors
	row_vector[nB] B[nS] ;

	#Y: outcomes
	vector[nY] speed ;
	
	vector[nY] error ;

}
transformed data{

	int nTot ;
	nTot = nW*4 ;

}

parameters {
	#Znoise: trial-by-trial Znoise for outcome
	real<lower=0> Znoise ;
	#Zbetas: beta coefficients (on the z-scale) for between and within subject predictors
	matrix[nB*4, nTot] Zbetas ;
	#ZsdW: population-level sds (on the z-scale) for each within-subject predictor
	vector<lower=0>[nTot] ZsdsW ;
	#corW: population-level correlations amongst within-subject predictors
	corr_matrix[nTot] corsW ;
	#Svals: subject-by-subject values for within-subject predictors
	matrix[nS,nTot] Svals ;
}
model {
	#cov: covariance matrix (determined by corW & ZsdW)
	matrix[nTot,nTot] cov ;

	#compute cov from cor & Zsds
	cov = quad_form_diag(corsW,ZsdsW) ;

	#priors on population parameters
	to_vector(Zbetas) ~ normal(0, 1) ;
	ZsdsW ~ weibull(2, 1) ;
	Znoise ~ weibull(2, 1) ;
	corsW ~ lkj_corr(2) ;

	for (ns in 1:nS){
		Svals[ns] ~ multi_normal(B[ns] * Zbetas , cov) ;
	}
	#ZY ~ normal( rows_dot_product( Svals[S] , W ) , Znoise ) ;
        {
                vector[nY] a;
                vector[nY] b;
                vector[nY] c;
                vector[nY] d;
                a = rows_dot_product( Svals[S,(nW*0+1):(nW*1)] , W[(nW*0+1):(nW*1)] ) ;
                b = rows_dot_product( Svals[S,(nW*1+1):(nW*2)] , W[(nW*1+1):(nW*2)] ) ;
                c = rows_dot_product( Svals[S,(nW*2+1):(nW*3)] , W[(nW*2+1):(nW*3)] ) ;
                d = rows_dot_product( Svals[S,(nW*3+1):(nW*4)] , W[(nW*3+1):(nW*4)] ) ; 
                for(i in 1:nY){
        	        error[i] ~ normal(
        	                (a[i]-b[i])/(1+exp(-c[i]*(speed[i]-d[i])))
        	                , Znoise
        	        ) ;
                }
        }
}
