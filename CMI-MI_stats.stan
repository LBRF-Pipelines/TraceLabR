data {
	# N: number of cases
	int<lower=1> N ;
	# K: outcomes per case
	int<lower=2> K ;
	# X: matrix of predictors for each case & variable
	matrix[N,K] X ;
	#Y: vector of outcomes
	vector[N] Y ;
}
parameters {
	# means: vector of population-level means for intercept & slope
	vector[K] means ;
	# sds: vector of population-level sds for intercept & slope
	vector<lower=0>[K] sds ;
	# cor: population-level correlation between intercept & slope
	corr_matrix[K] cor ;
	vector[K] betas ;
	real bdi_intercept ;
	real<lower=0> bdi_sd ;
}
transformed parameters{
	# cov: covariance matrix (derived from cor & sds)
	matrix[K,K] cov ; #used in getting correlation to work
	#compute covariance matrix from cor & sds
	cov = quad_form_diag(cor,sds) ;
}
model {
	#priors on population parameters; presumes data has been scale()ed
	means ~ normal(0,1) ;
	sds ~ weibull(2,1) ;
	cor ~ lkj_corr(1) ;
	bdi_intercept ~ normal(0,1) ;
	bdi_sd ~ weibull(2,1) ;
	betas ~ normal(0,1) ;
	#assert sampling of case-level outcomes on each variable given
	#  multivariate normal population
	for(s in 1:N){
		X[s,] ~ multi_normal(means,cov) ;
		Y[s] ~ normal(
			betas[1] * X[s,1] + betas[2] * X[s,2] + bdi_intercept
			, bdi_sd
		) ;
	}
}
