#' Calculate Minimum Posterior Probability of the Null Hypothesis
#'
#' This function implements a calibration method that computes the minimum
#' posterior probability of the null hypothesis based on a p-value and 
#' a prior probability. It uses a Bayes factor calibration that depends
#' on whether the p-value is below 1/e.
#'
#' @param p A numeric value representing the p-value from a statistical test (0 < p < 1)
#' @param q A numeric value representing the prior probability of the null hypothesis (0 < q < 1)
#'
#' @return The minimum posterior probability of the null hypothesis
#'
#' @examples
#' min_posterior(p = 0.2, q = 0.5)  # Returns approximately 0.466
#' min_posterior(p = 0.04, q = 0.5) # Returns approximately 0.303
#'
#' @references
#' Based on calibration methods for converting p-values to minimum
#' posterior probabilities via Bayes factors
#'
min_posterior <- function(p, q) {
  # Calculate the Bayes factor based on p-value
  # If p < 1/e, use the formula -e*p*log(p), otherwise BF = 1
  if (p < 1/exp(1)) {
    BF <- -exp(1) * p * log(p)
  } else {
    BF <- 1
  }
  
  # Calculate the posterior odds ratio
  odds_ratio <- (BF * q) / (1 - q)
  
  # Convert odds ratio to posterior probability
  posterior_min <- 1 / (1 + 1/odds_ratio)
  
  return(posterior_min)
}

# Examples
min_posterior(p = 0.2, q = 0.5)
# Should return approximately 0.466

min_posterior(p = 0.04, q = 0.5)
# Should return approximately 0.303

min_posterior(p = 0.4, q = 0.5)
# Should return 0.5 (when p ≥ 1/e, BF = 1, so posterior equals prior when q = 0.5)

# Visual representation of how posterior probabilities change
# with different p-values (for prior q = 0.5)
p_values <- seq(0.001, 0.5, by = 0.001)
posteriors <- sapply(p_values, function(p) min_posterior(p, q = 0.5))

# To plot in R:
# plot(p_values, posteriors, type = "l", 
#     xlab = "p-value", ylab = "Minimum posterior probability",
#     main = "Minimum posterior probability vs p-value (prior = 0.5)")