set.seed(20260512)

test_that("neutral arrivals agree with an analytical event-time recurrence", {
  s <- example_scenario(N = 1L)
  x <- simulate_shield(s, "P0", n_rep = 1L)
  # One minute loading, one minute travel, one minute unloading between legs.
  expect_equal(x$t_stage2_med, 11)
  expect_equal(x$t_stage3_med, 14)
  expect_equal(x$surv_stage2, 1)
  expect_equal(x$n_dispatch, 5)
  expect_equal(x$n_disruption, 0)
  expect_equal(x$util_L1, 4 / (2 * 60))
})

test_that("the administrative horizon counts physical arrival before unloading", {
  s <- shield_parameters(toy_graph(), N = 1L, horizon = 11L)
  x <- simulate_shield(s, n_rep = 1L)
  expect_equal(x$surv_stage2, 1)
  expect_equal(x$surv_stage3, 0)
  expect_true(is.na(x$t_stage3_med))
  expect_equal(x$n_pending, 1)
  summary <- summarize_shield(x)
  expect_equal(summary$n[summary$endpoint == "t_stage3_med"], 0)
  expect_true(is.na(summary$median[summary$endpoint == "t_stage3_med"]))
})

test_that("unrealized reservations contribute occupancy but never dispatches", {
  s <- shield_parameters(toy_graph(), N = 1L, horizon = 1L, t_load = 2L)
  x <- simulate_shield(s, n_rep = 1L)
  expect_equal(x$util_L1, .5)
  expect_equal(x$n_dispatch, 0)
  expect_equal(x$n_disruption, 0)
})

test_that("death during loading cancels travel and retains elapsed occupancy", {
  s <- shield_parameters(toy_graph(), N = 1L, h = rep(1000, 3), t_load = 2L)
  x <- simulate_shield(s, n_rep = 1L)
  expect_equal(x$n_dispatch, 0)
  expect_equal(x$n_disruption, 0)
  expect_gt(x$util_L1, 0)
  expect_equal(x$surv_stage1, 0)
})

test_that("simultaneous history order cannot affect disruption_model allocation", {
  s <- stochastic_scenario()
  s$atk_window <- 3L
  a <- data.frame(time = c(1, 2, 3, 3, 3), route = c(1L, 1L, 1L, 2L, 3L))
  b <- a[c(1, 2, 5, 4, 3), ]
  expect_identical(shieldR:::disruption_model_response_full(a, s),
                   shieldR:::disruption_model_response_full(b, s))
  s$lambda <- 10000
  expect_true(all(is.finite(shieldR:::disruption_model_response_full(a, s))))
  expect_equal(sum(shieldR:::disruption_model_response_full(a, s)), 1)
})

test_that("extreme disruption_model response remains finite in coupled simulation", {
  s <- example_scenario(N = 6L)
  s$lambda <- 10000
  s$disruptions_enabled <- TRUE
  x <- simulate_shield(s, "P3", "adaptive", n_rep = 2L)
  expect_true(all(is.finite(x$surv_stage2)))
  expect_equal(x$surv_stage2, c(1, 1))
})

test_that("policy holding responds to committed earlier departures", {
  s <- stochastic_scenario(N = 1L)
  s$h[] <- 0
  s$lambda <- 20
  s$atk_window <- 3L
  state <- shieldR:::init_patients_full(s)
  hist <- data.frame(time = -3:-1, route = rep(1L, 3))
  scheduled <- data.frame(time = rep(1, 6), route = rep(c(2L, 3L), each = 3))
  choice <- shieldR:::decide_transport_full("P3", state,
    shieldR:::init_theta_full(s), 1L, 1L, s, demand = integer(8),
    disruption_model = "adaptive", history = hist, scheduled = scheduled, decision_u = 0)
  expect_equal(choice$route, 1L)
  expect_gt(choice$hold, 0L)
  expect_true(choice$hold %in% s$departure_holds)
})

test_that("neutral teaching scenarios demonstrate resource and horizon effects", {
  balanced <- simulate_shield(example_scenario(N = 8L), n_rep = 1L)
  constrained <- simulate_shield(example_scenario("constrained", N = 8L), n_rep = 1L)
  expect_equal(balanced$surv_stage3, 1)
  expect_lt(constrained$surv_stage3, balanced$surv_stage3)
  expect_true(is.na(constrained$t_stage3_med))
})

test_that("all policy and disruption_model combinations preserve endpoint invariants", {
  s <- stochastic_scenario()
  for (p in paste0("P", 0:3)) for (a in c("static", "adaptive")) {
    x <- simulate_shield(s, p, a, n_rep = 2L)
    expect_true(all(x$n_stage1 >= x$n_stage2 & x$n_stage2 >= x$n_stage3))
    expect_true(all(x$surv_stage2 >= 0 & x$surv_stage2 <= 1))
    expect_true(all(x$hold_mean >= 0 & x$hold_mean <= max(s$departure_holds)))
    expect_true(all(unlist(x[paste0("util_L", 1:5)]) >= 0 &
                    unlist(x[paste0("util_L", 1:5)]) <= 1))
  }
})
