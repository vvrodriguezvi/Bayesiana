
data {
  
  // tamano de de la muesra
  int<lower=0> N;
  
  // numero de elementos en la matriz de diseno, variables + intercepto
  int<lower=0> P;
  
  // variable de respuesta, conteo de cuartos (debe ser mayor a cero)
  int<lower=0, upper=1> y[N];
  
  // variables de entrada x
  matrix[N, P] X; 

}

// parametros del modelo
parameters {
  
  // vector de coeficientes
  vector[P] beta;
}



model {
  beta ~ normal(0, 1000);

  y ~  bernoulli_logit( X * beta );

}

