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
	vector[nY] Y ;

}
transformed data{

	#ZY: rescaled outcomes for each trial
	vector[nY] ZY ;

	#compute Y_scaled to have mean 0 and sd 1
	ZY = (Y-mean(Y)) / sd(Y) ;

}

parameters {
	#Znoise: trial-by-trial Znoise for outcome
	real<lower=0> Znoise ;
	#Zbetas: beta coefficients (on the z-scale) for between and within subject predictors
	matrix[nB, nW] Zbetas ;
	#ZsdW: population-level sds (on the z-scale) for each within-subject predictor
	vector<lower=0>[nW] ZsdsW ;
	#corW: population-level correlations amongst within-subject predictors
	corr_matrix[nW] corsW ;
	#Svals: subject-by-subject values for within-subject predictors
	matrix[nS,nW] Svals ;
}
model {
	#cov: covariance matrix (determined by corW & ZsdW)
	matrix[nW,nW] cov ;

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
	ZY ~ normal( rows_dot_product( Svals[S] , W ) , Znoise ) ;
}
generated quantities{

	#variables to store the parameters on the original scale of Y
	real noise ;
	matrix[nB, nW] coefs ;
	vector[nW] sds ;

	#unscale the noise parameter
	noise = Znoise * sd(Y) ;

	#unscale the sds
	sds = ZsdsW * sd(Y) ;

	#unscale the coefficients
	coefs = Zbetas * sd(Y) ;
	coefs[1,1] = coefs[1,1] + mean(Y) ;

}
