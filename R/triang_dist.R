#' Density of the triangular distribution
#'
#' @param x Numeric vector where the density is evaluated.
#' @param min Lower limit of the distribution.
#' @param max Upper limit of the distribution.
#' @param mode Mode of the distribution (between min and max).
#'
#' @return Numeric vector with density values.
#' @export
dtriang <- function(x, min, max, mode) {
  if (any(min >= max)) stop("`min` debe ser menor que `max`.")
  if (any(mode < min | mode > max)) stop("`mode` debe estar entre `min` y `max`.")

  x <- as.numeric(x)
  min <- as.numeric(min)
  max <- as.numeric(max)
  mode <- as.numeric(mode)

  f <- numeric(length(x))

  left <- x >= min & x <= mode
  right <- x >= mode & x <= max

  f[left] <- 2 * (x[left] - min[left]) / ((max[left] - min[left]) * (mode[left] - min[left]))
  f[right] <- 2 * (max[right] - x[right]) / ((max[right] - min[right]) * (max[right] - mode[right]))

  f[x < min | x > max] <- 0

  f
}

#' Cumulative distribution function of the triangular distribution
#'
#' @param q Numeric vector of quantiles.
#' @param min Lower limit.
#' @param max Upper limit.
#' @param mode Mode of the distribution.
#'
#' @return Numeric vector with CDF values.
#' @export
ptriang <- function(q, min, max, mode) {
  if (any(min >= max)) stop("`min` debe ser menor que `max`.")
  if (any(mode < min | mode > max)) stop("`mode` debe estar entre `min` y `max`.")

  q <- as.numeric(q)
  min <- rep(min, length(q))
  max <- rep(max, length(q))
  mode <- rep(mode, length(q))

  F_val <- numeric(length(q))

  left <- q >= min & q <= mode
  right <- q >= mode & q <= max

  F_val[q < min] <- 0
  F_val[q > max] <- 1

  F_val[left] <- (q[left] - min[left])^2 / ((max[left] - min[left]) * (mode[left] - min[left]))
  F_val[right] <- 1 - (max[right] - q[right])^2 / ((max[right] - min[right]) * (max[right] - mode[right]))

  F_val
}

#' Quantile function of the triangular distribution
#'
#' @param p Probabilities between 0 and 1.
#' @param min Lower limit.
#' @param max Upper limit.
#' @param mode Mode.
#'
#' @return Numeric vector with quantiles.
#' @export
qtriang <- function(p, min, max, mode) {
  if (any(min >= max)) stop("`min` debe ser menor que `max`.")
  if (any(mode < min | mode > max)) stop("`mode` debe estar entre `min` y `max`.")
  if (any(p < 0 | p > 1)) stop("`p` debe estar entre 0 y 1.")

  p <- as.numeric(p)
  min <- rep(min, length(p))
  max <- rep(max, length(p))
  mode <- rep(mode, length(p))

  Fc <- (mode - min) / (max - min)

  x <- numeric(length(p))

  left <- p <= Fc
  right <- p > Fc

  x[left] <- min[left] + sqrt((max[left] - min[left]) * (mode[left] - min[left]) * p[left])
  x[right] <- max[right] - sqrt((max[right] - min[right]) * (max[right] - mode[right]) * (1 - p[right]))

  x
}

#' Random generation from the triangular distribution
#'
#' @param n Number of observations.
#' @param min Lower limit.
#' @param max Upper limit.
#' @param mode Mode of the distribution.
#'
#' @return Numeric vector of length n.
#' @importFrom stats runif
#' @export
rtriang <- function(n, min, max, mode) {
  if (any(min >= max)) stop("`min` must be smaller than `max`.")
  if (any(mode < min | mode > max)) stop("`mode` must be between `min` and `max`.")

  u <- runif(n)
  qtriang(u, min, max, mode)
}
