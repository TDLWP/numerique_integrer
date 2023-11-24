#'rectangles_integration
#'
# Méthode des rectangles 2 pour l'intégration numérique
#'
#'@param  f fonction a integrer
#'@param  a borne inferieur
#'@param  b borne superieur
#'@param  n nombre pour la precision
#'
#'@return integration
#'
#'@export
rectangles_integration <- function(f, a, b, n) {
  h <- (b - a) / n  # Largeur de chaque rectangle
  x <- seq(a, b, length.out = n+1)  # Points d'évaluation des rectangles
  y <- f(x)  # Valeurs de la fonction aux points d'évaluation
  sum(y[-n] * h)  # Calcul de l'approximation de l'intégrale
}
