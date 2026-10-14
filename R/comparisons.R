check_sample <- function(x, name) {
  if (!is.numeric(x) || !length(x) || any(!is.finite(x)))
    stop(name, " must be a nonempty finite numeric vector", call. = FALSE)
}

#' Compare two independent simulation arms
#'
#' Applies a two-sided, tie-corrected Mann-Whitney U approximation, Cliff's
#' delta and percentile bootstrap intervals for delta and the median difference.
#' Positive effects favour `x`. This test compares distributions; it is not
#' specifically a test of medians. Observations must be independent cohorts.
#' Normality testing is not a prerequisite. Intervals describe Monte Carlo
#' sampling uncertainty conditional on the assumed model.
#'
#' @param x,y Nonempty finite numeric vectors from independent arms.
#' @param label_x,label_y Arm labels.
#' @param family Explicit comparison-family label; no automatic FDR grouping.
#' @param B Positive integer bootstrap resamples; default 10,000.
#' @param seed Bootstrap seed; default 20260512.
#' @return One-row data frame containing U, raw and log10 p-values, sample sizes,
#'   medians and quartiles, Cliff's delta and median difference with percentile
#'   95 percent intervals. `p_raw_log10` is authoritative for very small tails;
#'   `p_raw` is floored at the smallest normal positive double when necessary.
#'   Point intervals are flagged. Apply [adjust_fdr()] to an explicitly defined
#'   family of comparisons. The caller's RNG state is restored.
#' @export
compare_policies <- function(x, y, label_x = "x", label_y = "y", family = "comparison",
                            B = 10000L, seed = 20260512L) {
  check_sample(x, "x"); check_sample(y, "y")
  check_scalar(B, "B", 1, integer = TRUE)
  check_scalar(seed, "seed", 0, integer = TRUE)
  if (seed >= .Machine$integer.max) stop("seed must leave room for seed + 1", call. = FALSE)
  for (name in c("label_x", "label_y", "family"))
    if (!is.character(get(name)) || length(get(name)) != 1L ||
        is.na(get(name)) || !nzchar(get(name))) stop(name, " must be one nonempty label", call. = FALSE)
  result <- with_shield_rng(seed,
    compare_arms(x, y, label_x, label_y, family, seed = seed, B = as.integer(B)))
  names(result)[names(result) == "W"] <- "U"
  result
}

#' Kruskal-Wallis comparison of multiple independent arms
#' @param values Finite cohort-level outcomes.
#' @param groups Nonmissing group labels of the same length, at least three arms.
#' @param family Explicit comparison-family label.
#' @return One row containing H, degrees of freedom and raw and log10 p-values.
#'   This is an omnibus test. Pairwise contrasts and multiplicity gates must be
#'   specified by the study; no post-hoc test is selected automatically.
#' @export
compare_multiple <- function(values, groups, family = "comparison") {
  check_sample(values, "values")
  if (length(groups) != length(values) || anyNA(groups) || length(unique(groups)) < 3L)
    stop("groups must identify at least three arms, without missing labels", call. = FALSE)
  if (!is.character(family) || length(family) != 1L || is.na(family) || !nzchar(family))
    stop("family must be one nonempty label", call. = FALSE)
  kw_omnibus(values, groups, family)
}

#' Adjust one explicitly defined family using Benjamini-Hochberg FDR
#' @param log10_p Vector of base-10 log p-values, each at most zero. Missing
#'   values are preserved and excluded from the family size. `-Inf` is allowed.
#' @return Adjusted log10 p-values in input order. The caller supplies each
#'   prespecified family separately; no SHIELD hypothesis grid is embedded.
#' @export
adjust_fdr <- function(log10_p) {
  if (!is.numeric(log10_p) || any(log10_p > 0, na.rm = TRUE))
    stop("log10_p must be numeric and at most zero", call. = FALSE)
  bh_log10(log10_p)
}

#' Analyze a paired disruptions-disabled intervention
#'
#' Checks that the two runs have matching parameters, policy, disruption_model,
#' replication identifiers and seeds, with disruptions enabled only in the first.
#' Rows are aligned by replication identifier. The contrast is no disruptions minus
#' disruptions, a total modelled intervention effect that includes downstream and
#' administrative-horizon effects. It is not direct disruption mortality.
#'
#' @param disruptions,no_disruptions `shield_run` objects from [simulate_shield()] with
#'   identical seeds, replication counts and settings except disruption enablement.
#' @param endpoint Arrival survival at Stages 1--3, or clinical `surv_24h` /
#'   `surv_30d` when both runs use the clinical extension.
#' @param B Positive integer paired bootstrap resamples; default 10,000.
#' @param seed Bootstrap seed; default 20260512.
#' @return One row with paired mean difference and percentile 95 percent
#'   interval on the proportion scale. Replication indices are resampled jointly.
#' @export
compare_intervention <- function(disruptions, no_disruptions, endpoint = "surv_stage2",
                                  B = 10000L, seed = 20260512L) {
  if (!inherits(disruptions, "shield_run") || !inherits(no_disruptions, "shield_run"))
    stop("both inputs must be shield_run objects", call. = FALSE)
  endpoint <- match.arg(endpoint, c("surv_stage1", "surv_stage2", "surv_stage3", "surv_24h", "surv_30d"))
  check_scalar(B, "B", 1, integer = TRUE)
  a <- disruptions[order(disruptions$rep), ]; b <- no_disruptions[order(no_disruptions$rep), ]
  pa <- attr(disruptions, "parameters"); pb <- attr(no_disruptions, "parameters")
  if (!isTRUE(pa$disruptions_enabled) || !identical(pb$disruptions_enabled, FALSE))
    stop("supply disruptions enabled first and disabled second", call. = FALSE)
  pa$disruptions_enabled <- pb$disruptions_enabled <- NULL
  keys <- c("rep", "replicate_seed", "cell_seed", "policy", "disruption_model")
  if (!nrow(a) || anyDuplicated(a$rep) || anyDuplicated(b$rep) ||
      !identical(pa, pb) ||
      !all(vapply(keys, function(k) identical(a[[k]], b[[k]]), logical(1))))
    stop("runs do not have matching parameters and replication identities", call. = FALSE)
  check_sample(a[[endpoint]], "disruptions endpoint")
  check_sample(b[[endpoint]], "no_disruptions endpoint")
  ci <- with_shield_rng(seed, boot_ci_paired(b[[endpoint]], a[[endpoint]],
    B = as.integer(B), seed = seed))
  data.frame(endpoint = endpoint, n_pairs = nrow(a),
    contrast = "no_disruptions minus disruptions", mean_difference = unname(ci["est"]),
    lower = unname(ci["lwr"]), upper = unname(ci["upr"]),
    point_interval = unname(ci["lwr"]) == unname(ci["upr"]))
}
