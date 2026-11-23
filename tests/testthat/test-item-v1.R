item_fixture_v1 <- function(N=6L) {
  p <- example_scenario(N=N)
  m <- list(degradation_scale=.2,early_other=.02,late_other=.1,tau=40,residual=.1,
    prep=c(1,2,3),processing=c(2,3,4),failure=0,capacity=1L,fragility=c(1.2,1,.8),
    sensitivity=.7,specificity=.95,persistence=1,missing=0,refresh=2L,delivery=0L,
    useful_updates=TRUE,item_age=0)
  with_item_model(p,m)
}

trace_item_v1 <- function(p,policy="P3",seed=20260512L) {
  p$item_trace <- TRUE
  x <- simulate_replication(p,policy,"static",seed)
  list(result=x,state=attr(x,"item_trace"))
}

test_that("item configuration validates and old mode is unchanged", {
  p <- item_fixture_v1()
  expect_null(example_scenario()$item_model)
  bad <- p$item_model;bad$capacity <- 0
  expect_error(with_item_model(example_scenario(),bad),"capacity")
  bad <- p$item_model;bad$missing <- 2
  expect_error(with_item_model(example_scenario(),bad),"missing")
  expect_identical(simulate_replication(example_scenario(),"P3","static",20260512L),
    simulate_replication(example_scenario(),"P3","static",20260512L))
})

test_that("arrival, readiness, processing and effective control are distinct", {
  p <- item_fixture_v1(10L)
  p$item_model$degradation_scale <- p$item_model$early_other <- p$item_model$late_other <- 0
  x <- trace_item_v1(p);s<-x$state;c<-s$item_specs
  expect_true(all(s$t_stage2+p$t_unload <= c$ready))
  expect_true(all(c$ready<=c$start))
  expect_true(all(c$start<c$completion))
  expect_equal(c$completion,c$stabilization)
  expect_true(all(s$t_stage3>=c$completion | is.na(s$t_stage3)))
  for(t in seq(0,max(c$completion)))
    expect_lte(sum(c$start<=t & c$completion>t),p$item_model$capacity)
  expect_equal(x$result$viable_30d,1)
  expect_equal(x$result$never_stabilization,0)
})

test_that("risk persists after arrival and after stabilization", {
  p <- item_fixture_v1(20L)
  p$item_model$degradation_scale <- 0;p$item_model$early_other<-0
  p$item_model$late_other <- 8
  x <- trace_item_v1(p)
  expect_equal(x$result$viable_stage2,1)
  expect_lt(x$result$viable_30d,x$result$viable_24h)
  expect_true(all(x$state$item_specs$loss>=1440))
  expect_equal(x$result$loss_degradation,0)
})

test_that("competing loss prevents servicing and releases capacity", {
  p <- item_fixture_v1(20L);p$item_model$degradation_scale<-20
  x<-trace_item_v1(p);c<-x$state$item_specs
  expect_true(all(c$start[is.finite(c$start)]<c$loss[is.finite(c$start)]))
  expect_true(all(c$stabilization[is.finite(c$stabilization)]<c$loss[is.finite(c$stabilization)]))
  expect_equal(x$result$viable_30d+x$result$loss_degradation+x$result$loss_other+
    x$result$loss_second_hit,1)
})

test_that("missing and frozen information cannot stop condition dynamics", {
  p<-item_fixture_v1(10L);p$item_model$missing<-1
  x<-trace_item_v1(p);expect_true(all(is.na(x$state$item_specs$obs_condition_index)))
  expect_true(all(x$state$item_specs$obs_missing))
  p$item_model$useful_updates<-FALSE
  a<-with_decision_controls(p,"updated","reactive")
  b<-with_decision_controls(p,"initial_observed","reactive")
  expect_equal(simulate_replication(a,"P3","static",20260512L),
    simulate_replication(b,"P3","static",20260512L))
})

test_that("planner uses observed data and keeps truth coefficients separate", {
  p<-item_fixture_v1();s<-trace_item_v1(p)$state
  obs<-s$item_initial[1,]
  value<-item_observed_viability_v1(obs,10,c(20,40),p$item_planner)
  expect_gte(value[1],value[2])
  p$item_model$degradation_scale<-5
  expect_equal(value,item_observed_viability_v1(obs,10,c(20,40),p$item_planner))
})

test_that("item seed pairing and RNG preservation hold", {
  p<-item_fixture_v1();set.seed(20260512);before<-.Random.seed
  a<-simulate_replication(p,"P3","static",20260512L)
  expect_identical(before,.Random.seed)
  expect_identical(a,simulate_replication(p,"P3","static",20260512L))
  p$disruptions_enabled<-TRUE
  expect_equal(a,simulate_replication(p,"P3","static",20260512L))
})

test_that("persistent recognition preserves the specified initial marginal", {
  p<-item_fixture_v1(10000L)
  x<-with_shield_rng(20260512L,{
    s<-item_init_v1(init_items_full(p),p,init_rng_full())
    r<-init_rng_full();s<-item_observe_v1(s,0,p,r)
    first<-s$item_pending[[1]]$positive
    s<-item_observe_v1(s,2,p,r)
    list(first=first,second=s$item_pending[[2]]$positive)
  })
  expect_lt(abs(mean(x$first)-p$item_model$sensitivity),.02)
  expect_identical(x$first,x$second)
})

test_that("delayed initial measurements reach frozen policies once delivered", {
  p<-item_fixture_v1();p$item_model$delivery<-3L
  x<-with_shield_rng(20260512L,{
    r<-init_rng_full();s<-item_init_v1(init_items_full(p),p,r)
    s<-item_observe_v1(s,0,p,r)
    s<-item_deliver_v1(s,0,p)
    before<-s$item_initial
    s<-item_deliver_v1(s,3,p)
    initial<-s$item_initial
    s<-item_observe_v1(s,4,p,r);s<-item_deliver_v1(s,7,p)
    list(before=before,initial=initial,latest=s$item_initial,current=s$item_specs)
  })
  expect_true(all(is.na(x$before$obs_time)))
  expect_true(all(x$initial$obs_time==0))
  expect_identical(x$initial,x$latest)
  expect_true(all(x$current$obs_time==4))
})

test_that("a zero softmax parameter gives excluded item-model choices no weight", {
  g <- toy_graph()
  g$travel_min <- rep(c(1, 2, 3), 5)
  g$exposure_weight <- rep(c(.3, .2, .1), 5)
  p <- shield_parameters(g, N = 6L, horizon = 40L, q = .4, lambda = 2, beta = 0,
    disruption_risk = c(low = .2, high = .3), disruptions_enabled = TRUE)
  p <- with_item_model(p, item_fixture_v1()$item_model)
  x <- simulate_shield(p, "P3", "adaptive", n_rep = 3L)
  expect_true(all(is.finite(x$viable_stage2)))
  expect_true(all(x$n_dispatch > 0))
  # Excluded choices are never selected, whatever the uniform draw.
  s <- trace_item_v1(p)$state
  s$status[] <- 0L
  s$node[] <- 1L
  s$viable[] <- TRUE
  theta <- init_theta_full(p)
  theta$b[1, 1] <- theta$b[1, 1] + 10
  for (u in c(1e-12, .5, 1 - 1e-12)) {
    choice <- item_decide_v1("P3", s, theta, 1L, 1L, p, integer(8),
      disruption_model = "static", decision_u = u)
    expect_identical(choice$route, 1L)
  }
})

test_that("a zero softmax parameter neither strands items nor vehicles", {
  g <- toy_graph()
  g$travel_min <- rep(c(1, 2, 3), 5)
  g$exposure_weight <- rep(c(.3, .2, .1), 5)
  # Nondestructive disruptions and no item loss: every item must be delivered.
  p <- shield_parameters(g, N = 6L, horizon = 120L, q = 0, att_delay = 2L,
    lambda = 2, beta = 0, disruption_risk = c(low = .2, high = .3),
    disruptions_enabled = TRUE)
  m <- item_fixture_v1()$item_model
  m[c("degradation_scale", "early_other", "late_other")] <- list(0, 0, 0)
  x <- simulate_shield(with_item_model(p, m), "P3", "static", n_rep = 3L)
  expect_equal(x$viable_stage3, rep(1, 3))
  expect_equal(x$n_pending, rep(0L, 3))
  expect_equal(x$n_dispatch, rep(30L, 3))
})

test_that("decision draws at the upper end of the unit interval still select", {
  # With two items the normalised choice probabilities sum to just below one,
  # so a draw of exactly one exceeds the cumulative sum.
  s <- stochastic_scenario(N = 2L)
  state <- init_items_full(s)
  choice <- decide_transport_full("P3", state, init_theta_full(s), 1L, 1L, s,
    demand = integer(8), decision_u = 1)
  expect_true(choice$route %in% 1:3)
  expect_true(choice$hold %in% s$departure_holds)
})

test_that("item-model configuration errors name the problem", {
  m <- item_fixture_v1()$item_model
  expect_error(with_item_model(list(), m), "created by shield_parameters")
  expect_error(with_item_model(example_scenario(), m[-1]), "required fields")
  bad <- m; bad$prep <- c(3, 2, 1)
  expect_error(with_item_model(example_scenario(), bad), "service triangle")
  bad <- m; bad$fragility <- c(1, 0, 1)
  expect_error(with_item_model(example_scenario(), bad), "fragility")
  bad <- m; bad$useful_updates <- NA
  expect_error(with_item_model(example_scenario(), bad), "logical")
})

test_that("aggregate calibration recovers attainable targets and validates inputs", {
  m <- item_fixture_v1()$item_model
  control <- c(30, 60, 90, 120, Inf)
  fragility <- c(1.2, 1, 1, 0.8, 1)
  reference <- calibrate_item_model(m, control, fragility,
    c(loss_24 = 0.1, loss_30 = 0.15, degradation_24 = 0.05), step = 30)
  expect_named(reference$expected, c("loss_24", "loss_30", "degradation_24"))
  expect_true(all(reference$expected >= 0 & reference$expected <= 1))
  expect_identical(reference$integration_step, 30)
  expect_true(all(reference$stabilization_quantiles %in% control))
  # Targets produced by the fitted model are recovered from other start values.
  start <- m
  start[c("degradation_scale", "early_other", "late_other")] <- list(1, 0.5, 0.5)
  refit <- calibrate_item_model(start, control, fragility, reference$expected,
    step = 30)
  expect_equal(refit$convergence, 0L)
  expect_equal(refit$expected, reference$expected, tolerance = 1e-4)
  # Additional named entries are ignored.
  extra <- c(reference$expected, note = 2)
  expect_identical(calibrate_item_model(start, control, fragility, extra,
    step = 30), refit)
  expect_error(calibrate_item_model(m, as.character(control), fragility,
    reference$expected), "calibration batch")
  expect_error(calibrate_item_model(m, control[-1], fragility,
    reference$expected), "calibration batch")
  expect_error(calibrate_item_model(m, control, fragility,
    c(loss_24 = 0.1)), "missing calibration targets")
  expect_error(calibrate_item_model(m, control, fragility,
    c(loss_24 = 2, loss_30 = 0.1, degradation_24 = 0)), "probabilities")
})

test_that("serviced reference replay is reproducible and preserves RNG state", {
  m <- item_fixture_v1()$item_model
  set.seed(20260512)
  before <- .Random.seed
  a <- simulate_item_reference(m, N = 40L, seed = 20260512L)
  expect_identical(.Random.seed, before)
  expect_named(a, c("loss_24", "loss_30", "degradation_24"))
  expect_true(all(a >= 0 & a <= 1))
  expect_gte(a[["loss_30"]], a[["loss_24"]])
  expect_identical(a, simulate_item_reference(m, N = 40L, seed = 20260512L))
  zero <- m
  zero[c("degradation_scale", "early_other", "late_other")] <- list(0, 0, 0)
  expect_equal(unname(simulate_item_reference(zero, N = 10L)), c(0, 0, 0))
})
