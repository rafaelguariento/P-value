#' Calculate Minimum Posterior Probability of the Null Hypothesis
#'
#' This function implements a calibration method that computes the minimum
#' posterior probability of the null hypothesis based on a p-value and
#' the prior probability of the *alternative* hypothesis P(H). It uses
#' a Bayes factor calibration that depends on whether the p-value is
#' below 1/e.
#'
#' @param p A numeric value representing the p-value from a statistical test (0 < p < 1)
#' @param prior_alt A numeric value representing the prior probability of the
#'   *alternative* hypothesis, i.e., P(H) as used in the manuscript (0 < prior_alt < 1).
#'   The prior probability of the null hypothesis is computed internally as
#'   P(H0) = 1 - prior_alt.
#'
#' @return The minimum posterior probability of the null hypothesis, P(H0 | Data)
#'
#' @examples
#' min_posterior(p = 0.2, prior_alt = 0.5)  # Returns approximately 0.466
#' min_posterior(p = 0.04, prior_alt = 0.5)  # Returns approximately 0.259
#'
#' @references
#' Based on calibration methods for converting p-values to minimum
#' posterior probabilities via Bayes factors (Sellke et al. 2001)
#'
min_posterior <- function(p, prior_alt) {
    # Derive prior probability of the null hypothesis
    q <- 1 - prior_alt

    # Calculate the Bayes factor based on p-value
    # If p < 1/e, use the formula -e*p*log(p), otherwise BF = 1
    if (p < 1 / exp(1)) {
        BF <- -exp(1) * p * log(p)
    } else {
        BF <- 1
    }

    # Calculate the posterior odds ratio in favor of H0
    odds_ratio <- (BF * q) / (1 - q)

    # Convert odds ratio to posterior probability
    posterior_min <- 1 / (1 + 1 / odds_ratio)

    return(posterior_min)
}

# Examples
# prior_alt = P(H), the prior probability of the alternative hypothesis.
# The function internally computes the null prior as P(H0) = 1 - prior_alt.

# Example 1: P(H) = 0.10 (i.e. a long-shot hypothesis), p-value = 0.01
min_posterior(p = 0.01, prior_alt = 0.10)
# Should return approximately 0.530

# Example 2: P(H) = 0.50 (non-informative prior), p-value = 0.04
min_posterior(p = 0.04, prior_alt = 0.50)
# Should return approximately 0.259

# Example 3: P(H) = 0.50, p-value = 0.4 (above 1/e, so BF = 1)
min_posterior(p = 0.4, prior_alt = 0.50)
# Should return 0.5 (when p ≥ 1/e, BF = 1, so posterior equals prior when prior_alt = 0.5)

# Visual representation of how posterior probabilities change
# with different p-values (for prior P(H) = 0.5)
p_values <- seq(0.001, 0.5, by = 0.001)
posteriors <- sapply(p_values, function(p) min_posterior(p, prior_alt = 0.50))

# To plot in R:
# plot(p_values, posteriors, type = "l",
#     xlab = "p-value", ylab = "Minimum posterior probability",
#     main = "Minimum posterior probability vs p-value (prior = 0.5)")
