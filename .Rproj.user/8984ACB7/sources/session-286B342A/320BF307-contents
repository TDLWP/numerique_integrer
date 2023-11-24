#'monte carlo integration
#'
#Méthode de Monte Carlo pour l'intégration numérique
#'
#'@param  f fonction a integrer
#'@param  a borne inferieur
#'@param  b borne superieur
#'@param  n nombre pour la precision
#'
#'@return integration
#'
#'@export
monte_carlo_integration <- function(f, a, b, n) {
  x <- runif(n, min = a, max = b)
  mean(f(x)) * (b - a)
}
