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
