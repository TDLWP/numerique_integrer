#'trapeze integration
#'
#'Méthode du trapèze pour l'intégration numérique
#'
#'@param  f fonction a integrer
#'@param  a borne inferieur
#'@param  b borne superieur
#'@param  n nombre pour la precision
#'
#'@return integration
#'
#'@export
trapeze_integration <- function(f, a, b, n) {
  h <- (b - a) / n
  x <- seq(a, b, by = h)
  y <- f(x)
  sum((y[-1] + y[-n]) * h / 2)
}
