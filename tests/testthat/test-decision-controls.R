set.seed(20260512)

test_that("controls are explicit and preserve the caller's parameter object", {
  p <- stochastic_scenario()
  q <- with_decision_controls(p, "initial_observed", "reactive")
  expect_null(p$decision_controls)
  expect_identical(q$decision_controls$clinical, "initial_observed")
  expect_identical(q$decision_controls$initial_information, "observed")
  expect_error(with_decision_controls(list()), "shield_parameters")
  expect_error(with_decision_controls(p, "truth"), "arg")
  expect_error(with_decision_controls(p, anticipation = "optimal"), "arg")
})

test_that("fixed controlled policies use the first observed rather than true class", {
  p <- stochastic_scenario(N = 2L)
  state <- shieldR:::init_patients_full(p)
  state$cls0 <- state$cls <- c(1L, 3L)
  state$obs_initial <- c(2L, 1L)
  state$obs <- c(1L, 2L)
  theta <- shieldR:::init_theta_full(p)
  decide <- function(params) shieldR:::decide_transport_full("P0", state,
    theta, 1L, 1L, params, demand = integer(8))$patient
  expect_equal(decide(p), 1L)
  expect_equal(decide(with_decision_controls(p, "initial_observed")), 2L)
  expect_equal(decide(with_decision_controls(p, "updated")), 1L)
})

test_that("freezing decision information does not freeze physiological state", {
  p <- with_decision_controls(stochastic_scenario(N = 2L), "initial_observed")
  state <- shieldR:::init_patients_full(p)
  state$cls[] <- 3L
  state$obs <- state$obs_initial <- rep(3L, 2)
  changed <- shieldR:::update_patient_state_full(state, params = p,
    u_death = rep(1, 2), u_escalate = rep(0, 2))
  changed <- shieldR:::reassess_patients_full(changed, p,
    u_error = rep(1, 2), u_shift = rep(1, 2))
  decision <- shieldR:::prepare_decision_state_full(changed, p)
  expect_equal(changed$cls, c(2L, 2L))
  expect_equal(changed$obs, c(2L, 2L))
  expect_equal(decision$cls, c(2L, 2L))
  expect_equal(decision$obs, c(3L, 3L))
  expect_equal(changed$obs_initial, c(3L, 3L))
  expect_error(shieldR:::prepare_decision_state_full(
    shieldR:::init_patients_full(p), p), "first observed")
})

test_that("updated known-model controls preserve legacy P3 numerical results", {
  p <- stochastic_scenario()
  q <- with_decision_controls(p, "updated", "known_model")
  for (a in c("static", "adaptive")) for (seed in 20260512L + 0:2) {
    expect_identical(simulate_replication(p, "P3", a, seed),
      simulate_replication(q, "P3", a, seed))
  }
})

test_that("reactive decisions do not use future-response information", {
  p <- with_decision_controls(stochastic_scenario(N = 1L), "updated", "reactive")
  state <- shieldR:::init_patients_full(p)
  theta <- shieldR:::init_theta_full(p)
  hist <- data.frame(time = -3:-1, route = rep(1L, 3))
  scheduled <- data.frame(time = rep(1, 6), route = rep(c(2L, 3L), each = 3))
  decision <- function(disruption_model, history, scheduled) shieldR:::decide_transport_full(
    "P3", state, theta, 1L, 1L, p, demand = integer(8),
    disruption_model = disruption_model, history = history, scheduled = scheduled, decision_u = .3)
  a <- decision("adaptive", hist, scheduled)
  b <- decision("static", NULL, NULL)
  expect_identical(a, b)
  expect_equal(a$hold, 0L)
})

test_that("forecast switch changes no decisions for a static response", {
  p <- stochastic_scenario()
  a <- with_decision_controls(p, "updated", "reactive")
  b <- with_decision_controls(p, "updated", "known_model")
  expect_identical(simulate_replication(a, "P3", "static"),
    simulate_replication(b, "P3", "static"))
})

test_that("information-use controls agree when patient information never changes", {
  p <- stochastic_scenario()
  p$esc32 <- p$esc21 <- p$misclass <- 0
  a <- with_decision_controls(p, "initial_observed")
  b <- with_decision_controls(p, "updated")
  for (policy in c("P0", "P1", "P2", "P3"))
    expect_identical(simulate_replication(a, policy, "adaptive"),
      simulate_replication(b, policy, "adaptive"))
})

test_that("controlled simulation preserves RNG and paired intervention identity", {
  p <- with_decision_controls(stochastic_scenario(), "initial_observed", "reactive")
  set.seed(20260512)
  before <- .Random.seed
  a <- simulate_shield(p, "P3", "adaptive", n_rep = 3L)
  b <- simulate_shield(p, "P3", "adaptive", n_rep = 3L, disruptions_enabled = FALSE)
  expect_identical(.Random.seed, before)
  expect_identical(a$replicate_seed, b$replicate_seed)
  expect_equal(compare_intervention(a, b, B = 20L)$n_pairs, 3L)
  changed <- p
  changed$decision_controls$clinical <- "updated"
  c <- simulate_shield(changed, "P3", "adaptive", n_rep = 3L,
    disruptions_enabled = FALSE)
  expect_error(compare_intervention(a, c, B = 20L), "matching parameters")
})

test_that("controlled runs retain physical-arrival endpoint semantics", {
  p <- with_decision_controls(shield_parameters(toy_graph(), N = 1L,
    horizon = 11L), "initial_observed", "reactive")
  x <- simulate_shield(p, "P0", n_rep = 1L)
  expect_equal(x$surv_stage2, 1)
  expect_equal(x$surv_stage3, 0)
  expect_true(is.na(x$t_stage3_med))
})
