set.seed(20260512)

test_that("simulation preserves ambient RNG state and replays exactly", {
  set.seed(20260512)
  old <- .Random.seed
  kind <- RNGkind()
  s <- stochastic_scenario()
  a <- simulate_shield(s, "P3", "adaptive", n_rep = 3L)
  expect_identical(.Random.seed, old)
  expect_identical(RNGkind(), kind)
  invisible(runif(10))
  b <- simulate_shield(s, "P3", "adaptive", n_rep = 3L)
  expect_identical(a, b)
})

test_that("RNG restoration also covers missing seed and error paths", {
  old_kind <- RNGkind()
  old_seed <- .Random.seed
  on.exit({do.call(RNGkind, as.list(old_kind)); assign(".Random.seed", old_seed,
    envir = .GlobalEnv)}, add = TRUE)
  rm(".Random.seed", envir = .GlobalEnv)
  invisible(simulate_shield(example_scenario(N = 1L), n_rep = 1L))
  expect_false(exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
  set.seed(20260512)
  before <- .Random.seed
  expect_error(shieldR:::with_shield_rng(20260512, stop("fixture failure")), "fixture failure")
  expect_identical(.Random.seed, before)
  expect_identical(RNGkind(), old_kind)
})

test_that("disruption toggles retain clinical random numbers when disruptions have no consequences", {
  s <- stochastic_scenario()
  s$q <- 0
  s$att_delay <- 0L
  a <- simulate_shield(s, "P0", "adaptive", n_rep = 4L, disruptions_enabled = TRUE)
  b <- simulate_shield(s, "P0", "adaptive", n_rep = 4L, disruptions_enabled = FALSE)
  for (nm in c("replicate_seed", "surv_stage1", "surv_stage2", "surv_stage3",
                "t_stage2_med", "t_stage3_med")) expect_identical(a[[nm]], b[[nm]])
})
