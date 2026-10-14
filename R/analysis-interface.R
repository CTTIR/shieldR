#' Simulate one cohort using an explicit replication seed
#'
#' This entry point supports external schedulers and durable checkpoints.
#' The seed is a replication seed, not a cell/master seed. Obtain a complete
#' schedule with [replication_seeds()] before splitting work into chunks.
#' The caller's RNG kind and state are restored, including on error.
#' @param parameters A [shield_parameters()] object, optionally configured
#'   with [with_decision_controls()] for a separate experiment.
#' @param policy One of P0, P1, P2, P3.
#' @param disruption_model Either static or adaptive.
#' @param replication_seed One nonnegative integer seed.
#' @return One-row data frame of cohort outcomes, without study identifiers.
#' @export
simulate_replication <- function(parameters, policy = c("P0", "P1", "P2", "P3"),
    disruption_model = c("static", "adaptive"), replication_seed = 20260512L) {
  if (!inherits(parameters, "shield_parameters"))
    stop("parameters must be created by shield_parameters()", call. = FALSE)
  policy <- match.arg(policy); disruption_model <- match.arg(disruption_model)
  with_shield_rng(replication_seed, simulate_once_full(policy, disruption_model, parameters))
}

#' Construct the reproducible seed schedule for a complete cell
#' @param n_rep Positive integer total number of cohort replications.
#' @param seed Master/cell seed, default 20260512.
#' @return Integer vector of length n_rep. Generate the complete schedule once;
#'   chunking must not change its declared length or resample the schedule.
#' @export
replication_seeds <- function(n_rep, seed = 20260512L) {
  check_scalar(n_rep, "n_rep", 1, integer = TRUE)
  with_shield_rng(seed, sample.int(.Machine$integer.max, as.integer(n_rep)))
}

#' Bootstrap a named two-arm statistic
#'
#' Percentile intervals for independent arms or explicitly paired cohort
#' outcomes. The caller must establish pairing before passing paired vectors.
#' Prefer [compare_intervention()] when working with complete shield_run objects,
#' because it also verifies settings and replication identities.
#' @param x,y Nonempty finite numeric vectors.
#' @param statistic One of cliffs_delta, median_difference or mean_difference.
#' @param paired Logical. If TRUE, resample matching indices jointly.
#' @param B Positive integer resample count, default 10,000.
#' @param conf Confidence level strictly between zero and one.
#' @param seed Bootstrap seed, default 20260512.
#' @return Named vector with est, lwr and upr; orientation is x minus y.
#'   Caller RNG state is restored. Intervals quantify Monte Carlo uncertainty
#'   conditional on the model and do not propagate parameter uncertainty.
#' @export
bootstrap_statistic <- function(x, y, statistic = c("cliffs_delta", "median_difference",
    "mean_difference"), paired = FALSE, B = 10000L, conf = .95, seed = 20260512L) {
  check_sample(x, "x"); check_sample(y, "y")
  statistic <- match.arg(statistic)
  check_scalar(B, "B", 1, integer = TRUE)
  if (!is.logical(paired) || length(paired) != 1L || is.na(paired))
    stop("paired must be TRUE or FALSE", call. = FALSE)
  if (paired && length(x) != length(y)) stop("paired vectors must have equal lengths", call. = FALSE)
  check_scalar(conf, "conf", 0)
  if (conf <= 0 || conf >= 1) stop("conf must be strictly between zero and one", call. = FALSE)
  fun <- switch(statistic, cliffs_delta = cliffs_delta, median_difference = median_diff,
    mean_difference = if (paired) function(a, b) mean(a - b) else function(a, b) mean(a) - mean(b))
  with_shield_rng(seed, if (paired)
    boot_ci_paired(x, y, fun, B = as.integer(B), conf = conf, seed = seed)
    else boot_ci2(x, y, fun, B = as.integer(B), conf = conf, seed = seed))
}

#' Mann-Whitney U with log-scale tail probabilities
#' @param x,y Finite independent cohort-level observations.
#' @return List U, p, p_log10, z, underflow. The log probability is authoritative
#'   for very small tails. The linear value is floored if necessary. Uses the
#'   same two-sided, tie- and continuity-corrected approximation as
#'   [compare_policies()]. No preliminary normality test is performed.
#' @export
mann_whitney_u <- function(x, y) {
  check_sample(x, "x"); check_sample(y, "y")
  value <- mwu(x, y)
  names(value)[names(value) == "W"] <- "U"
  value
}

#' Describe Cliff's delta using conventional magnitude thresholds
#' @param delta Finite numeric Cliff's delta values in `[-1, 1]`.
#' @return Character magnitude labels. These descriptive thresholds are not
#'   clinical importance margins and do not establish practical benefit.
#' @export
effect_magnitude <- function(delta) {
  check_sample(delta, "delta")
  if (any(abs(delta) > 1)) stop("delta must be in [-1, 1]", call. = FALSE)
  cliff_magnitude(delta)
}
