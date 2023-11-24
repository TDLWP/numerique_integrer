#'rectangle_1_integration
#'
# Méthode des rectangles 1 pour l'intégration numérique
#'
#'@param  f fonction a integrer
#'@param  a borne inferieur
#'@param  b borne superieur
#'@param  n nombre pour la precision
#'
#'@return integration
#'
#'@export
rectangle_integration <- function(f, a, b, n) {
  h <- (b - a) / n
  x <- seq(a + h/2, b - h/2, by = h)
  sum(f(x) * h)
}
