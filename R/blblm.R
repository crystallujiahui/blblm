#' @import purrr
#' @import furrr
#' @import stats
#' @import future
#' @importFrom magrittr %>%
#' @importFrom utils capture.output
#' @aliases NULL
#' @details
#' Linear Regression with Little Bag of Bootstraps
"_PACKAGE"


## quiets concerns of R CMD check re: the .'s that appear in pipelines
# from https://github.com/jennybc/googlesheets/blob/master/R/googlesheets.R
utils::globalVariables(c("."))

#' Compute Linear regression Model
#'
#' Find linear regression model using subsamples and bootstrap samples with option for parallelization
#' Users will need to make clusters own their own.Recommanded code is plan(multisession, worker = 4).
#' @param formula variale formula for model
#' @param data data frame we are working on
#' @param m number of subsamples
#' @param B number of bootstrap samples
#'
#' @return fitted model using mutliple clusters and blb technique
#' @export
#' @examples
#' blblm_par(mpg ~ wt * hp, data= mtcars, m=3, B=100)
blblm_par <- function(formula, data, m = 10,B = 5000) {
  data_list <- split_data(data, m)
  estimates <- future_map(
    data_list,
    ~ lm_each_subsample(formula = formula, data = ., n = nrow(data), B = B))
  res <- list(estimates = estimates, formula = formula)
  class(res) <- "blblm"
  invisible(res)
}

#' Split data into Subsamples
#' split data into m parts of approximated equal sizes
#' @param data data frame we are working on
#' @param m number of subsamples
split_data <- function(data, m) {
  idx <- sample.int(m, nrow(data), replace = TRUE)
  data %>% split(idx)
}

#' Compute the estimates
#' Compute the estimates using bootstrap technique
#' @param formula variale formula for model
#' @param data data frame we are working on
#' @param n numeric
#' @param B number of bootstrap samples
lm_each_subsample <- function(formula, data, n, B) {
  replicate(B, lm_each_boot(formula, data, n), simplify = FALSE)
}

#' Compute blb estimates
#' compute the regression estimates for a blb dataset
#' @param formula variale formula for model
#' @param data data frame we are working on
#' @param n numeric
lm_each_boot <- function(formula, data, n) {
  freqs <- rmultinom(1, n, rep(1, nrow(data)))
  lm1(formula, data, freqs)
}

#' Compute regression esimates
#' estimate the regression estimates based on given the number of repetitions
#' @param formula variale formula for model
#' @param data data frame we are working on
#' @param freqs number of repetitions
lm1 <- function(formula, data, freqs) {
  # drop the original closure of formula,
  # otherwise the formula will pick a wrong variable from the global scope.
  environment(formula) <- environment()
  object <- lm(formula, data, weights = freqs)
  list(coef = blbcoef(object), sigma = blbsigma(object))
}

#' Calculate Coefficients
#'
#' Calculate coefficients for input fitted model
#' @param object fitted model
#' @return coefficients for the fit
#' compute the coefficients from fit
#'
blbcoef <- function(object) {
  coef(object)
}

#' Calculate signma
#'
#' Calculate sigma value for input fitted model
#' @param object fitted model
#' @return sigma for the fit
#' compute sigma from fit
blbsigma <- function(object) {
  p <- object$rank
  y <- model.extract(object$model, "response")
  e <- fitted(object) - y
  w <- object$weights
  sqrt(sum(w * (e^2)) / (sum(w) - p))
}

#' Print blblm
#'
#' @param x input
#' @param ... more inputs
#' @export
#'
#' @method print blblm
print.blblm <- function(x, ...) {
  cat("blblm model:", capture.output(x$formula))
  cat("\n")
}


#' @export
#' @method sigma blblm
sigma.blblm <- function(object, confidence = FALSE, level = 0.95, ...) {
  est <- object$estimates
  sigma <- mean(map_dbl(est, ~ mean(map_dbl(., "sigma"))))
  if (confidence) {
    alpha <- 1 - 0.95
    limits <- est %>%
      map_mean(~ quantile(map_dbl(., "sigma"), c(alpha / 2, 1 - alpha / 2))) %>%
      set_names(NULL)
    return(c(sigma = sigma, lwr = limits[1], upr = limits[2]))
  } else {
    return(sigma)
  }
}

#' @export
#' @method coef blblm
coef.blblm <- function(object, ...) {
  est <- object$estimates
  map_mean(est, ~ map_cbind(., "coef") %>% rowMeans())
}


#' @export
#' @method confint blblm
confint.blblm <- function(object, parm = NULL, level = 0.95, ...) {
  if (is.null(parm)) {
    parm <- attr(terms(object$formula), "term.labels")
  }
  alpha <- 1 - level
  est <- object$estimates
  out <- map_rbind(parm, function(p) {
    map_mean(est, ~ map_dbl(., list("coef", p)) %>% quantile(c(alpha / 2, 1 - alpha / 2)))
  })
  if (is.vector(out)) {
    out <- as.matrix(t(out))
  }
  dimnames(out)[[1]] <- parm
  out
}

#' @export
#' @method predict blblm
predict.blblm <- function(object, new_data, confidence = FALSE, level = 0.95, ...) {
  est <- object$estimates
  X <- model.matrix(reformulate(attr(terms(object$formula), "term.labels")), new_data)
  if (confidence) {
    map_mean(est, ~ map_cbind(., ~ X %*% .$coef) %>%
      apply(1, mean_lwr_upr, level = level) %>%
      t())
  } else {
    map_mean(est, ~ map_cbind(., ~ X %*% .$coef) %>% rowMeans())
  }
}


mean_lwr_upr <- function(x, level = 0.95) {
  alpha <- 1 - level
  c(object = mean(x), quantile(x, c(alpha / 2, 1 - alpha / 2)) %>% set_names(c("lwr", "upr")))
}

map_mean <- function(.x, .f, ...) {
  (map(.x, .f, ...) %>% reduce(`+`)) / length(.x)
}

map_cbind <- function(.x, .f, ...) {
  map(.x, .f, ...) %>% reduce(cbind)
}

map_rbind <- function(.x, .f, ...) {
  map(.x, .f, ...) %>% reduce(rbind)
}
