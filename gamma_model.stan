data {
  int<lower=0> N; //the number of observations
  int<lower=0> K; //the number of columns in the model matrix
  matrix[N,K] X; //the model matrix
  vector[N] y; //the response
}

parameters {
  vector[K] beta; //the regression parameters
  real<lower=0> phi10;
}

transformed parameters {
real<lower=0> phi1;
 phi1   = square(phi10);
}

model {  
  vector[N] alpha1; //shape parameter for the gamma distribution
  vector[N] beta1; //rate parameter for the gamma distribution
  real mu;
  beta ~ normal(0, 1000);
  
    for (i in 1:N){
  //mu = exp(proc[ProcedureGroup[i]] + surg[Surgeon[i]] + anes[AnesthesiaType[i]] + X[i]*beta); 
  mu = exp(X[i,] * beta);
  alpha1[i] = (mu .* mu) / phi1;
  #alpha1[i] = square(mu) / phi1; 
  beta1[i]  = mu / phi1;
  y[i] ~ gamma(alpha1[i], beta1[i]);
}
}
