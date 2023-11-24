#'simpson integration
#'
#Méthode de Simpson pour l'intégration numérique
#'
#'@param  f fonction a integrer
#'@param  a borne inferieur
#'@param  b borne superieur
#'@param  n nombre pour la precision
#'
#'@return integration
#'
#'@export
simpson_integration <- function(f, a, b, n) {
  h <- (b - a) / n
  x <- seq(a, b, by = h)
  y <- f(x)
  sum((y[1] + y[n] + 4*sum(y[seq(2, n-1, by = 2)]) + 2*sum(y[seq(3, n-2, by = 2)])) * h / 3)
}
