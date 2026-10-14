set.seed(20260512)

test_that("independent comparisons agree with direct pairwise effect definitions", {
  x <- c(0, .5, .5, 1); y <- c(0, 0, .5, .5)
  expected_delta <- mean(outer(x, y, ">")) - mean(outer(x, y, "<"))
  before <- .Random.seed
  z <- compare_policies(x, y, B = 50L)
  expect_equal(z$cliff_delta, expected_delta)
  expect_equal(z$U, unname(stats::wilcox.test(x, y, exact = FALSE, correct = TRUE)$statistic))
  expect_equal(z$p_raw, stats::wilcox.test(x, y, exact = FALSE, correct = TRUE)$p.value)
  expect_identical(.Random.seed, before)
  expect_equal(z$med_diff, median(x) - median(y))
  expect_lte(z$cliff_lwr, z$cliff_upr)
})

test_that("tied and extreme comparisons retain defined log probabilities", {
  z <- compare_policies(rep(1, 8), rep(1, 8), B = 20L)
  expect_equal(z$p_raw, 1)
  expect_equal(z$cliff_delta, 0)
  expect_true(z$med_diff_ci_degenerate)
  extreme <- shieldR:::mwu(rep(1, 1000), rep(0, 1000))
  expect_true(is.finite(extreme$p_log10))
  expect_gt(extreme$p, 0)
  expect_lt(extreme$p_log10, -300)
  kw <- compare_multiple(rep(1, 9), rep(letters[1:3], each = 3))
  expect_equal(kw$H, 0)
  expect_equal(kw$df, 2)
  expect_equal(kw$p_raw, 1)
})

test_that("Kruskal-Wallis agrees with hand-derived tied rank arithmetic", {
  values <- c(1, 1, 2, 2, 3, 3, 4, 5, 5)
  groups <- rep(c("A", "B", "C"), each = 3L)
  # Mid-rank sums are 6.5, 14.5 and 24. Four ties of size two give
  # correction 1 - 4 * (2^3 - 2) / (9^3 - 9) = 29/30.
  # Thus H = [12/(9*10) * sum(R_i^2/3) - 3*10] / (29/30) = 614/87.
  expected_H <- 614 / 87
  expected_p <- exp(-expected_H / 2) # Exact upper tail for chi-square df = 2.
  result <- compare_multiple(values, groups, family = "tied_three_arm")
  expect_equal(result$H, expected_H, tolerance = 1e-12)
  expect_equal(result$df, 2L)
  expect_equal(result$p_raw, expected_p, tolerance = 1e-12)
  expect_equal(result$p_raw_log10, -expected_H / (2 * log(10)),
               tolerance = 1e-12)
  expect_identical(result$test, "Kruskal-Wallis")
  expect_identical(result$family, "tied_three_arm")
  expect_identical(result$arms, "A|B|C")
  expect_false(result$p_underflow)
  # Group labels, row order and increasing rescaling do not change ranks.
  permutation <- c(9, 2, 6, 1, 8, 4, 7, 3, 5)
  expect_equal(compare_multiple(10 + 2 * values[permutation],
    groups[permutation], family = "tied_three_arm"), result)
})

test_that("log-scale FDR agrees with base R and preserves extreme tails", {
  p <- c(.01, .03, .04, .9)
  expect_equal(10^adjust_fdr(log10(p)), p.adjust(p, "BH"))
  expect_identical(adjust_fdr(c(-1000, -500, NA_real_))[3], NA_real_)
  expect_true(all(is.finite(adjust_fdr(c(-1000, -500)))))
  expect_equal(adjust_fdr(c(-Inf, 0)), c(-Inf, 0))
  expect_error(adjust_fdr(1), "at most zero")
})

test_that("input errors cannot silently alter sample sizes", {
  expect_error(compare_policies(c(1, NA), 1:3, B = 2L), "finite")
  expect_error(compare_policies(numeric(), 1:3, B = 2L), "nonempty")
  expect_error(compare_policies(1:3, 1:3, B = 0L), "integer")
  expect_error(compare_policies(1:3, 1:3, seed = .Machine$integer.max), "seed")
  expect_error(compare_multiple(1:4, c(1, 1, 2, 2)), "three arms")
  expect_error(compare_multiple(1:6, c("A", "A", "B", "B", "C", NA)),
               "without missing labels")
  expect_error(compare_multiple(1:6, rep(c("A", "B", "C"), each = 2),
    family = ""), "one nonempty label")
  expect_error(simulate_shield(list()), "shield_parameters")
})

test_that("paired interventions check identities and align shuffled rows", {
  s <- stochastic_scenario()
  a <- simulate_shield(s, "P3", "adaptive", n_rep = 5L, disruptions_enabled = TRUE)
  b <- simulate_shield(s, "P3", "adaptive", n_rep = 5L, disruptions_enabled = FALSE)
  z <- compare_intervention(a, b, B = 50L)
  expect_equal(z$mean_difference, mean(b$viable_stage2 - a$viable_stage2))
  expect_identical(z, compare_intervention(a, b[5:1, ], B = 50L))
  expect_error(compare_intervention(b, a, B = 2L), "enabled first")
  bad <- b; bad$replicate_seed[1] <- 1L
  expect_error(compare_intervention(a, bad, B = 2L), "identities")
  expect_error(compare_intervention(a, b[-1, ], B = 2L), "identities")
  different <- simulate_shield(s, "P2", "adaptive", n_rep = 5L, disruptions_enabled = FALSE)
  expect_error(compare_intervention(a, different, B = 2L), "identities")
})
