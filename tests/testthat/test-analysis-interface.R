set.seed(20260512)

test_that("external scheduling reproduces the cohort runner including chunk order", {
  s <- example_scenario(N = 3L)
  seed <- 20260512L
  schedule <- replication_seeds(5L, seed)
  x <- simulate_shield(s, "P3", "adaptive", n_rep = 5L, seed = seed)
  expect_identical(schedule, x$replicate_seed)
  individual <- do.call(rbind, lapply(schedule, function(z)
    simulate_replication(s, "P3", "adaptive", z)))
  for (name in names(individual)) expect_identical(individual[[name]], x[[name]])
  shuffled <- c(3L, 1L, 5L, 2L, 4L)
  replay <- lapply(shuffled, function(i) simulate_replication(s, "P3", "adaptive", schedule[i]))
  replay <- do.call(rbind, replay[order(shuffled)])
  expect_identical(replay, individual)
})

test_that("named bootstrap statistics preserve the original algorithms", {
  x <- c(0, .5, 1, 1); y <- c(0, 0, .5, 1)
  ref <- shieldR:::with_shield_rng(20260512L,
    shieldR:::boot_ci2(x, y, shieldR:::cliffs_delta, B = 40L))
  expect_identical(bootstrap_statistic(x, y, B = 40L), ref)
  ref <- shieldR:::with_shield_rng(20260512L,
    shieldR:::boot_ci_paired(x, y, B = 40L))
  expect_identical(bootstrap_statistic(x, y, "mean_difference", paired = TRUE, B = 40L), ref)
  before <- .Random.seed
  invisible(bootstrap_statistic(x, y, "median_difference", B = 20L))
  expect_identical(.Random.seed, before)
  expect_error(bootstrap_statistic(x, y[-1], paired = TRUE), "equal lengths")
  expect_error(bootstrap_statistic(x, y, conf = 1), "strictly")
  expect_equal(mann_whitney_u(x, y)$U,
    unname(stats::wilcox.test(x, y, exact = FALSE)$statistic))
  expect_identical(effect_magnitude(c(0, .2, .4, .8)),
    c("negligible", "small", "medium", "large"))
  expect_error(effect_magnitude(2), "[-1, 1]", fixed = TRUE)
})
