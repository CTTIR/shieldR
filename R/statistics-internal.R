# Five-stage network simulation model.

# Internal nonparametric primitives; log probabilities are authoritative.
mwu <- function(x, y) {
  wt <- suppressWarnings(
    stats::wilcox.test(x, y, alternative = "two.sided",
                       exact = FALSE, correct = TRUE))
  n1 <- length(x); n2 <- length(y); N <- n1 + n2
  U  <- unname(wt$statistic)
  mu <- n1 * n2 / 2
  tie <- table(rank(c(x, y)))
  sigma <- sqrt((n1 * n2 / 12) *
                ((N + 1) - sum(tie^3 - tie) / (N * (N - 1))))
  if (!is.finite(sigma) || sigma == 0) {
    z <- 0
    log10p <- 0
    underflow <- FALSE
    p_safe <- 1
  } else {
    z <- (U - mu - sign(U - mu) * 0.5) / sigma
    log10p <- (log(2) + stats::pnorm(-abs(z), log.p = TRUE)) / log(10)
    underflow <- is.na(wt$p.value) || wt$p.value == 0
    p_safe <- if (underflow) max(.Machine$double.xmin, 10^log10p) else wt$p.value
  }
  list(W = U, p = p_safe,
       p_log10 = log10p, z = z, underflow = underflow)
}

# Cliff's delta
#
# delta = P(x > y) - P(x < y), computed from mid-ranks so that ties
# contribute exactly zero. Identical to the direct pairwise definition.
#
# @param x,y numeric vectors (two independent arms)
# @return scalar in [-1, 1]
cliffs_delta <- function(x, y) {
  n1 <- length(x); n2 <- length(y)
  r <- rank(c(x, y))
  U <- sum(r[seq_len(n1)]) - n1 * (n1 + 1) / 2
  2 * U / (n1 * n2) - 1
}

# Interpretation thresholds for Cliff's delta (Romano et al. 2006)
# @param d numeric delta
# @return character label
cliff_magnitude <- function(d) {
  a <- abs(d)
  ifelse(a < 0.147, "negligible",
    ifelse(a < 0.33, "small", ifelse(a < 0.474, "medium", "large")))
}

# Bootstrap percentile CI for a two-sample statistic
#
# Both arms are resampled independently with replacement, preserving the
# original arm sizes.
#
# @param x,y numeric vectors
# @param fun function(x, y) -> scalar
# @param B number of resamples
# @param conf confidence level
# @param seed RNG seed for this CI (derived deterministically by the caller)
# @return named numeric vector c(est, lwr, upr)
boot_ci2 <- function(x, y, fun, B = 10000L, conf = 0.95, seed = 20260512L) {
  set.seed(seed)
  n1 <- length(x); n2 <- length(y)
  est <- fun(x, y)
  bs <- numeric(B)
  for (b in seq_len(B)) {
    bs[b] <- fun(x[sample.int(n1, n1, replace = TRUE)],
                 y[sample.int(n2, n2, replace = TRUE)])
  }
  a <- (1 - conf) / 2
  q <- stats::quantile(bs, c(a, 1 - a), names = FALSE, na.rm = TRUE)
  c(est = est, lwr = q[1], upr = q[2])
}

# Bootstrap percentile CI for a paired statistic
#
# Replication indices are resampled jointly, preserving the common-seed
# pairing used by the disrupted/no-disruption counterfactual.
boot_ci_paired <- function(x, y, fun = function(a, b) mean(a - b),
                           B = 10000L, conf = 0.95,
                           seed = 20260512L) {
  stopifnot(length(x) == length(y))
  set.seed(seed)
  n <- length(x)
  est <- fun(x, y)
  bs <- numeric(B)
  for (b in seq_len(B)) {
    take <- sample.int(n, n, replace = TRUE)
    bs[b] <- fun(x[take], y[take])
  }
  a <- (1 - conf) / 2
  q <- stats::quantile(bs, c(a, 1 - a), names = FALSE, na.rm = TRUE)
  c(est = est, lwr = q[1], upr = q[2])
}

# Difference of medians (arm 1 minus arm 2)
# @param x,y numeric vectors
# @return scalar
median_diff <- function(x, y) stats::median(x) - stats::median(y)

# Complete two-arm comparison: MWU + Cliff's delta + bootstrap CIs
#
# @param x,y numeric vectors (arm 1, arm 2)
# @param label_x,label_y arm labels for the output row
# @param family scenario-family label (the BH adjustment unit)
# @param seed bootstrap seed
# @param B bootstrap resamples
# @return one-row data.frame with statistic, p, effect size and both CIs
compare_arms <- function(x, y, label_x, label_y, family,
                         seed = 20260512L, B = 10000L) {
  wt <- mwu(x, y)
  d  <- boot_ci2(x, y, cliffs_delta, B = B, seed = seed)
  md <- boot_ci2(x, y, median_diff,  B = B, seed = seed + 1L)

  data.frame(
    family      = family,
    arm_x       = label_x,
    arm_y       = label_y,
    n_x         = length(x),
    n_y         = length(y),
    median_x    = stats::median(x),
    median_y    = stats::median(y),
    iqr_x_lo    = unname(stats::quantile(x, 0.25)),
    iqr_x_hi    = unname(stats::quantile(x, 0.75)),
    iqr_y_lo    = unname(stats::quantile(y, 0.25)),
    iqr_y_hi    = unname(stats::quantile(y, 0.75)),
    test        = "Mann-Whitney U",
    W           = wt$W,
    p_raw       = wt$p,
    p_raw_log10 = wt$p_log10,
    p_underflow = wt$underflow,
    cliff_delta = unname(d["est"]),
    cliff_lwr   = unname(d["lwr"]),
    cliff_upr   = unname(d["upr"]),
    cliff_mag   = cliff_magnitude(unname(d["est"])),
    med_diff    = unname(md["est"]),
    med_diff_lwr = unname(md["lwr"]),
    med_diff_upr = unname(md["upr"]),
    ## The endpoint lives on a 1/30 lattice, so a percentile CI for a
    ## difference of medians can collapse to a single attainable value. Such a
    ## CI is an artefact of the granularity, not a precise estimate, and is
    ## flagged rather than presented as a narrow interval.
    med_diff_ci_degenerate = unname(md["lwr"]) == unname(md["upr"]),
    stringsAsFactors = FALSE
  )
}

# Kruskal-Wallis omnibus over >= 3 arms
#
# @param values numeric vector
# @param groups grouping factor
# @param family scenario-family label
# @return one-row data.frame
kw_omnibus <- function(values, groups, family) {
  ng <- length(unique(groups))
  if (length(unique(values)) == 1L) {
    statistic <- 0
    df <- ng - 1L
    log10p <- 0
    p_value <- 1
  } else {
    kt <- stats::kruskal.test(values ~ factor(groups))
    statistic <- unname(kt$statistic)
    df <- unname(kt$parameter)
    log10p <- stats::pchisq(statistic, df, lower.tail = FALSE,
                            log.p = TRUE) / log(10)
    p_value <- kt$p.value
  }
  p_safe <- max(.Machine$double.xmin, p_value)
  data.frame(
    family = family, test = "Kruskal-Wallis",
    arms   = paste(sort(unique(as.character(groups))), collapse = "|"),
    H      = statistic,
    df     = df,
    p_raw  = p_safe,
    p_raw_log10 = log10p,
    p_underflow = p_value == 0,
    stringsAsFactors = FALSE
  )
}

# Benjamini-Hochberg FDR adjustment within one declared family set
# @param p numeric vector of raw p-values
# @return numeric vector of adjusted p-values
bh <- function(p) stats::p.adjust(p, method = "BH")

# Benjamini-Hochberg FDR adjustment on base-10 log p-values
# @param log10p numeric vector of log10 raw p-values
# @return adjusted log10 p-values in the original order
bh_log10 <- function(log10p) {
  n <- length(log10p)
  out <- rep(NA_real_, n)
  ok <- which(!is.na(log10p))
  if (!length(ok)) return(out)
  ord <- ok[order(log10p[ok])]
  candidate <- pmin(0, log10p[ord] + log10(length(ok) / seq_along(ord)))
  adjusted <- rev(cummin(rev(candidate)))
  out[ord] <- adjusted
  out
}

# Format a p-value for tables and captions
# @param p numeric vector
# @return character vector
fmt_p <- function(p) {
  ifelse(is.na(p), NA_character_,
    ifelse(p == 0, "<1e-300",
      ifelse(p < 1e-4, "<0.0001", formatC(p, format = "f", digits = 4))))
}
